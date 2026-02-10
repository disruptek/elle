# SOLO Design Addendum v1.1 — Critical Fixes & Innovation Layer

**Status:** Approved amendments to v1.0 design  
**Date:** 2026-02-08  
**Supersedes:** Relevant sections of `solo_design_complete.md` v1.0

---

## 1. Critical Fixes

These are non-negotiable changes to the v1.0 design. They address issues that would cause production failures.

### 1.1 Atom Table Safety

**Problem:** The BEAM atom table is global, finite (~1M atoms), and never garbage collected. Any user service calling `String.to_atom(user_input)` will crash the entire VM — kernel, audit log, every service.

**Fix (three-pronged):**

**A. Static analysis at deploy time:**
- Before compiling user code, scan for dangerous functions:
  - `String.to_atom/1`
  - `List.to_atom/1`
  - `:erlang.binary_to_atom/2`
  - `Module.concat/1` with dynamic args
- Only `String.to_existing_atom/1` is permitted
- Reject deployment if dangerous patterns are found

**B. Runtime monitoring:**
```elixir
defmodule Solo.AtomMonitor do
  use GenServer

  @check_interval_ms 5_000
  @warn_threshold 0.80   # 80% of atom limit
  @kill_threshold 0.90   # 90% — start killing newest user services

  def init(_) do
    schedule_check()
    {:ok, %{}}
  end

  def handle_info(:check, state) do
    count = :erlang.system_info(:atom_count)
    limit = :erlang.system_info(:atom_limit)
    ratio = count / limit

    cond do
      ratio >= @kill_threshold ->
        Solo.UserSupervisor.kill_newest_services(5)
        Solo.AuditLog.critical(:atom_table_critical, %{ratio: ratio})
      ratio >= @warn_threshold ->
        Solo.AuditLog.warn(:atom_table_warning, %{ratio: ratio})
      true -> :ok
    end

    schedule_check()
    {:noreply, state}
  end
end
```

**C. Architecture preparation for separate-node isolation:**
- Design all service interfaces behind a behaviour (`Solo.Service`) that is location-transparent
- The behaviour must work identically whether the service is a local process or a process on a connected BEAM node
- This allows migrating user services to separate nodes later without API changes
- Erlang distribution provides transparent remote message passing

**Decision deferred:** Whether to run user services in separate BEAM nodes. The interfaces will support it; the decision of when to activate it depends on real-world atom table pressure.

### 1.2 NIF Ban for User Services

**Invariant:** User-deployed services may NOT load NIFs into the kernel BEAM node. This is non-negotiable.

**Enforcement:**
- Static analysis rejects any code referencing `:erlang.load_nif/2` or Rustler
- BEAM bytecode loading validates no NIF references exist
- External binaries requiring native code must use Port mode (Mode 3: separate OS process)

**Kernel NIFs:** Solo's own NIFs (if any, for proven bottlenecks only) run in the kernel and are audited/tested separately. For MVP, the kernel uses **zero NIFs** — CubDB for persistence (pure Elixir), no RocksDB.

### 1.3 Supervisor Hierarchy Rework

**Old design (broken):**
```
Root
└── User Process Supervisor (one DynamicSupervisor)
    ├── Service A (agent 1)
    ├── Service B (agent 1)
    ├── Service C (agent 2)   ← agent 1's restart storm kills this
    └── Service D (agent 2)
```

**New design (per-tenant, per-service):**
```
Root (:one_for_one)
├── System Supervisor (:rest_for_one)
│   ├── Solo.EventStore          ← NEW: replayable event store
│   ├── Solo.AtomMonitor         ← NEW: atom table safety
│   ├── Solo.Vault               ← NEW: secrets management
│   ├── Solo.Capability.Manager
│   ├── Solo.Trust.Engine        ← NEW: adaptive trust scoring
│   ├── Solo.Registry
│   ├── Solo.Resource.Monitor
│   └── Solo.BackpressureMonitor ← NEW: mailbox/circuit breakers
│
├── Driver Supervisor (:one_for_one)
│   ├── Filesystem Driver
│   ├── Network Driver
│   └── Hardware Driver(s)
│
├── Tenant Supervisor (DynamicSupervisor)
│   ├── Tenant:agent_1 Supervisor (:one_for_one, max_restarts: 10, max_seconds: 60)
│   │   ├── Service A Supervisor (:one_for_one, max_restarts: 3, max_seconds: 30)
│   │   │   └── Service A process tree
│   │   └── Service B Supervisor (:one_for_one, max_restarts: 3, max_seconds: 30)
│   │       └── Service B process tree
│   │
│   └── Tenant:agent_2 Supervisor (:one_for_one, max_restarts: 10, max_seconds: 60)
│       └── Service C Supervisor (:one_for_one, max_restarts: 3, max_seconds: 30)
│           └── Service C process tree
│
└── Solo.Gateway (gRPC server)
```

**Key properties:**
- Agent 1's services crashing never affects Agent 2's supervisor
- Each service gets its own supervisor with independent restart intensity
- Tenant-level supervisor catches service-level supervisor failures
- Root supervisor catches tenant-level failures (extremely unlikely)
- System services use `:rest_for_one` for ordered restart

### 1.4 Authentication from Day One

**The Deploy endpoint executes arbitrary code. It cannot be unauthenticated.**

**MVP authentication: mTLS (mutual TLS)**
- Solo generates a CA certificate at first boot
- Agents receive client certificates signed by Solo's CA
- gRPC server requires valid client certificate
- Certificate CN becomes the tenant/agent ID
- Certificate revocation via CRL (Certificate Revocation List)

**Why mTLS over API keys:**
- Cryptographically strong (not a shared secret)
- Certificate CN naturally maps to tenant identity
- TLS is already needed for gRPC; mTLS adds minimal overhead
- Standard tooling (`openssl`, `cfssl`) for cert management

```elixir
# In gRPC server config
config :solo, Solo.Gateway,
  port: 50051,
  tls: [
    certfile: "/etc/solo/server.pem",
    keyfile: "/etc/solo/server-key.pem",
    cacertfile: "/etc/solo/ca.pem",
    verify: :verify_peer,
    fail_if_no_peer_cert: true
  ]
```

### 1.5 Backpressure Primitives

**Three mechanisms:**

**A. Mailbox pressure detection:**
```elixir
defmodule Solo.BackpressureMonitor do
  @max_queue_len 5_000
  @check_interval_ms 1_000

  # Periodically check all monitored services
  def check_service(pid) do
    case Process.info(pid, :message_queue_len) do
      {:message_queue_len, len} when len > @max_queue_len ->
        Solo.Registry.mark_degraded(pid)
        {:overloaded, len}
      {:message_queue_len, len} ->
        Solo.Registry.mark_healthy(pid)
        {:ok, len}
      nil ->
        {:dead}
    end
  end
end
```

**B. Circuit breakers (per-capability):**
```elixir
defmodule Solo.CircuitBreaker do
  # States: :closed (normal), :open (rejecting), :half_open (testing)
  defstruct [:state, :failure_count, :last_failure_at, :threshold, :reset_after_ms]

  def call(breaker, fun) do
    case breaker.state do
      :open ->
        if time_to_retry?(breaker), do: try_half_open(breaker, fun), else: {:error, :circuit_open}
      :closed ->
        try_call(breaker, fun)
      :half_open ->
        try_call(breaker, fun)
    end
  end
end
```

**C. Load shedding at the gRPC gateway:**
- Track in-flight requests per tenant
- Reject new requests with `RESOURCE_EXHAUSTED` when tenant is at capacity
- Configurable per-tenant concurrency limits

### 1.6 Persistence: CubDB Replaces RocksDB

**Rationale:** RocksDB is a C NIF. A NIF crash in the kernel's persistence layer crashes Solo. CubDB is pure Elixir, ACID-compliant, and has no NIF risk.

**Trade-off:** CubDB is slower than RocksDB for high-throughput writes. This is acceptable because:
- Solo's kernel persistence is low-volume (service metadata, capabilities, audit events)
- User services that need high-performance storage can use external databases via capabilities
- CubDB handles 10K+ writes/sec which exceeds Solo's needs for 500 services

**Migration path:** If CubDB becomes a bottleneck (unlikely for kernel data), we can isolate RocksDB in a separate OS process communicating via ports.

```elixir
# Kernel store
{:ok, db} = CubDB.start_link(data_dir: "/var/solo/data/kernel")
CubDB.put(db, {:service, "my_service"}, %{status: :running, ...})
CubDB.get(db, {:service, "my_service"})
```

---

## 2. Innovation Layer

These four features are designed into the architecture from day one. They don't all ship in v0.1, but the interfaces and data models support them from the start.

### 2.1 Replayable Event Store

**What it is:** The mandatory audit log becomes a first-class, ordered, replayable event store. Every state change in Solo is an event. Events are the source of truth.

**Architecture:**

```
All state changes → Solo.EventStore → CubDB (persistent, ordered)
                         ↓
                    Solo.EventBus (pub/sub to subscribers)
                         ↓
                    [Audit backends, metrics, replay engine, trust engine]
```

**Event schema:**
```elixir
defmodule Solo.Event do
  @type t :: %__MODULE__{
    id: non_neg_integer(),           # Monotonic sequence number
    timestamp: integer(),            # System.monotonic_time(:nanosecond)
    wall_clock: DateTime.t(),        # For human readability
    tenant_id: String.t(),           # Which agent
    event_type: atom(),              # :service_deployed, :capability_granted, etc.
    subject: String.t(),             # Service ID or resource name
    payload: map(),                  # Event-specific data
    causation_id: non_neg_integer()  # Which event caused this one
  }
end
```

**Event types:**
```
:service_deployed        — code deployed, capabilities granted
:service_started         — process started, PID assigned
:service_crashed         — process died, exit reason
:service_restarted       — supervisor restarted process
:service_killed          — explicit kill request
:capability_granted      — new capability issued
:capability_revoked      — capability revoked
:capability_used         — capability exercised (sampled, not every call)
:capability_denied       — capability check failed
:trust_score_changed     — service trust score updated
:resource_limit_hit      — resource threshold crossed
:circuit_breaker_tripped — backpressure activated
:secret_accessed         — vault secret read
:code_hot_swapped        — hot code reload executed
:pipeline_step_completed — pipeline step finished
```

**Replay engine (ships later, interfaces designed now):**
```elixir
# "Show me the state of service 'cache' at timestamp T"
Solo.EventStore.replay_until(service_id: "cache", until: timestamp)

# "Replay all events for tenant 'agent_1' in the last hour"
Solo.EventStore.replay(tenant_id: "agent_1", since: one_hour_ago)

# "What caused this capability to be granted?"
Solo.EventStore.trace_causation(event_id: 42)
```

**Why this matters:** This is Solo's most differentiating feature. No existing agent runtime provides deterministic replay, time-travel debugging, or causal event tracing. It transforms debugging from "read logs and guess" to "replay exactly what happened."

### 2.2 Hot Code Loading as First-Class Operation

**What it is:** Leverage BEAM's unique ability to run two versions of a module simultaneously. Make live code updates a headline feature, not an afterthought.

**Architecture:**

```elixir
defmodule Solo.HotSwap do
  @doc """
  Hot-swap a running service's code.

  The old code continues running for in-flight requests.
  New code handles all new requests.
  If new code crashes within `rollback_window_ms`, automatically rollback.
  """
  def swap(service_id, new_code, opts \\ []) do
    rollback_window = Keyword.get(opts, :rollback_window_ms, 30_000)
    state_migration = Keyword.get(opts, :state_migration, &Function.identity/1)

    # 1. Compile new code in sandbox
    {:ok, new_module} = Solo.Compiler.compile_sandboxed(new_code)

    # 2. Load as new version (BEAM keeps old + new)
    :code.load_binary(new_module, ~c"hot_swap", bytecode)

    # 3. Trigger code_change on the running process
    GenServer.call(service_pid, {:code_change, state_migration})

    # 4. Start rollback timer
    Solo.HotSwap.Watchdog.start(service_id, rollback_window, old_module)

    # 5. Emit event
    Solo.EventStore.emit(:code_hot_swapped, %{
      service: service_id,
      old_version: old_hash,
      new_version: new_hash,
      rollback_window_ms: rollback_window
    })
  end
end
```

**Rollback semantics:**
- BEAM supports exactly two versions of a module simultaneously
- Old version handles in-flight calls; new version handles new calls
- If the new version's process crashes within `rollback_window_ms`:
  - Purge new code
  - Old code becomes current again
  - Service is restarted with old code
  - Event emitted: `:code_hot_swap_rolled_back`

**gRPC API addition:**
```protobuf
message HotSwapRequest {
  string service_id = 1;
  bytes new_code = 2;
  CodeFormat format = 3;
  int32 rollback_window_ms = 4;   // Auto-rollback if crash within window
  bytes state_migration_code = 5;  // Optional: Elixir function to transform state
}

message HotSwapResponse {
  string service_id = 1;
  string old_version_hash = 2;
  string new_version_hash = 3;
  Status status = 4;  // SWAPPING, ACTIVE, ROLLED_BACK
}
```

### 2.3 Adaptive Trust / Earned Capabilities

**What it is:** A trust scoring system where services earn capabilities through good behavior and lose them through violations.

**Architecture:**

```elixir
defmodule Solo.Trust do
  @doc """
  Trust score for a service: 0.0 (untrusted) to 1.0 (fully trusted).

  Score is computed from:
  - Uptime without crashes
  - Capability usage patterns (no denials)
  - Resource usage (within limits)
  - Time since deployment
  """

  defstruct [
    :service_id,
    :score,              # 0.0 to 1.0
    :successful_ops,     # Count of successful capability uses
    :denied_ops,         # Count of capability denials
    :crashes,            # Count of process crashes
    :resource_violations, # Count of resource limit hits
    :age_seconds,        # Time since first deployment
    :tier                # :untrusted | :provisional | :trusted | :privileged
  ]

  @tier_thresholds %{
    untrusted:    0.0,
    provisional:  0.3,
    trusted:      0.6,
    privileged:   0.9
  }

  def compute_score(history) do
    base = min(history.age_seconds / 86400, 0.2)  # Max 0.2 for age (1 day)
    success_ratio = safe_ratio(history.successful_ops, history.successful_ops + history.denied_ops)
    stability = 1.0 - min(history.crashes / 10.0, 0.5)  # Penalize crashes
    resource_health = 1.0 - min(history.resource_violations / 20.0, 0.3)

    score = base + (success_ratio * 0.3) + (stability * 0.3) + (resource_health * 0.2)
    Float.round(min(score, 1.0), 3)
  end

  def tier_for_score(score) do
    cond do
      score >= 0.9 -> :privileged
      score >= 0.6 -> :trusted
      score >= 0.3 -> :provisional
      true -> :untrusted
    end
  end
end
```

**Capability gating by trust tier:**
```elixir
# Capabilities have minimum trust requirements
%Solo.Capability{
  permission: :fs_write,
  min_trust_tier: :trusted,    # Must be at least 0.6 trust score
  ...
}

# When a service requests a capability:
def grant_if_trusted(service_id, capability) do
  trust = Solo.Trust.current(service_id)
  if Solo.Trust.tier_for_score(trust.score) >= capability.min_trust_tier do
    {:ok, Solo.Capability.create(capability)}
  else
    {:error, :insufficient_trust, trust.score, capability.min_trust_tier}
  end
end
```

**Trust events feed the event store:**
```
:trust_score_changed — emitted whenever score changes significantly (>0.05)
:trust_tier_changed  — emitted on tier transitions (provisional → trusted)
```

**Why this matters for LLM agents:** Agents can reason about trust explicitly. An agent can say "my service needs trust level 0.6 for filesystem write access" and Solo can respond "your service is at 0.45 — it needs 200 more successful operations without violations."

### 2.4 Composable Service Pipelines

**What it is:** A higher-level abstraction for chaining services together, where each step has isolated capabilities and backpressure propagates naturally.

**Architecture:**

```elixir
defmodule Solo.Pipeline do
  @doc """
  Define a typed, capability-aware service pipeline.

  Each step:
  - Runs in its own process (isolated)
  - Has only the capabilities it needs
  - Receives typed input, produces typed output
  - Failures are handled per-step (retry, skip, abort)
  """

  defstruct [:id, :tenant_id, :steps, :status]

  defmodule Step do
    defstruct [
      :name,
      :service_id,        # Which service handles this step
      :capabilities,       # Capabilities needed for this step only
      :input_type,         # Expected input shape
      :output_type,        # Expected output shape
      :timeout_ms,         # Per-step timeout
      :on_failure,         # :retry | :skip | :abort
      :max_retries         # Retry count
    ]
  end
end
```

**Pipeline definition:**
```elixir
pipeline = %Solo.Pipeline{
  id: "document_processor",
  tenant_id: "agent_1",
  steps: [
    %Step{name: :fetch,     service_id: "http_client",    capabilities: [:net_outbound],
          timeout_ms: 5000, on_failure: :abort},
    %Step{name: :parse,     service_id: "html_parser",    capabilities: [],
          timeout_ms: 2000, on_failure: :abort},
    %Step{name: :summarize, service_id: "llm_summarizer", capabilities: [:net_outbound],
          timeout_ms: 30000, on_failure: :retry, max_retries: 2},
    %Step{name: :store,     service_id: "database",       capabilities: [:fs_write],
          timeout_ms: 3000, on_failure: :retry, max_retries: 3}
  ]
}
```

**Execution model:**
```
Input → [fetch] → [parse] → [summarize] → [store] → Output
           ↓          ↓           ↓            ↓
        Events     Events      Events       Events
```

- Each step runs as a separate GenServer call with its own timeout
- Backpressure: if a step is slow, the pipeline applies the step's `on_failure` policy
- Events emitted for each step completion/failure
- The pipeline is itself a service (can be deployed, observed, killed)

**gRPC API addition:**
```protobuf
message PipelineDeployRequest {
  string pipeline_id = 1;
  repeated PipelineStep steps = 2;
}

message PipelineStep {
  string name = 1;
  string service_id = 2;
  repeated string capabilities = 3;
  int32 timeout_ms = 4;
  FailurePolicy on_failure = 5;
  int32 max_retries = 6;
}

enum FailurePolicy {
  ABORT = 0;
  RETRY = 1;
  SKIP = 2;
}

message PipelineExecuteRequest {
  string pipeline_id = 1;
  bytes input = 2;
}

message PipelineExecuteResponse {
  bytes output = 1;
  repeated PipelineStepResult step_results = 2;
}
```

---

## 3. New Kernel Services

### 3.1 Solo.Vault (Secrets Management)

```elixir
defmodule Solo.Vault do
  @moduledoc """
  Encrypted secrets store with capability-gated access.

  - Secrets encrypted at rest (AES-256-GCM)
  - Short-lived leases (not raw values)
  - Every access audited via EventStore
  - Automatic rotation support
  """

  use GenServer

  def store(secret_name, value, opts \\ []) do
    GenServer.call(__MODULE__, {:store, secret_name, encrypt(value), opts})
  end

  def fetch(secret_name, capability_token) do
    GenServer.call(__MODULE__, {:fetch, secret_name, capability_token})
  end

  def handle_call({:fetch, name, cap_token}, _from, state) do
    with :ok <- Solo.Capability.verify(cap_token, {:vault, :read, name}),
         {:ok, encrypted} <- CubDB.get(state.db, {:secret, name}),
         {:ok, value} <- decrypt(encrypted) do
      Solo.EventStore.emit(:secret_accessed, %{name: name, tenant: cap_token.tenant_id})
      {:reply, {:ok, value}, state}
    else
      {:error, reason} ->
        Solo.EventStore.emit(:secret_access_denied, %{name: name, reason: reason})
        {:reply, {:error, reason}, state}
    end
  end
end
```

### 3.2 Solo.Trust.Engine

Processes trust score changes from the EventStore. Subscribes to relevant events and updates scores in real-time.

### 3.3 Solo.AtomMonitor

Monitors atom table usage and takes protective action when thresholds are approached.

### 3.4 Solo.BackpressureMonitor

Monitors process mailbox depths across all services. Marks services as degraded when overloaded. Triggers circuit breakers.

---

## 4. Revised Supervisor Hierarchy (Complete)

```
Solo.Kernel (root, :one_for_one)
│
├── Solo.System.Supervisor (:rest_for_one)
│   ├── Solo.EventStore              # Replayable event log (CubDB-backed)
│   ├── Solo.AtomMonitor             # Atom table safety
│   ├── Solo.Vault                   # Secrets management
│   ├── Solo.Capability.Manager      # Token creation/validation
│   ├── Solo.Trust.Engine            # Adaptive trust scoring
│   ├── Solo.Registry                # Service discovery
│   ├── Solo.Resource.Monitor        # Memory/CPU tracking
│   └── Solo.Backpressure.Monitor    # Mailbox/circuit breakers
│
├── Solo.Driver.Supervisor (:one_for_one)
│   ├── Filesystem.Driver
│   ├── Network.Driver
│   └── Hardware.Driver(s)
│
├── Solo.Tenant.Supervisor (DynamicSupervisor)
│   │
│   ├── Tenant:agent_1 (:one_for_one, max_restarts: 10/60s)
│   │   ├── Service:svc_a (:one_for_one, max_restarts: 3/30s)
│   │   │   └── [service process tree]
│   │   └── Service:svc_b (:one_for_one, max_restarts: 3/30s)
│   │       └── [service process tree]
│   │
│   └── Tenant:agent_2 (:one_for_one, max_restarts: 10/60s)
│       └── Service:svc_c (:one_for_one, max_restarts: 3/30s)
│           └── [service process tree]
│
└── Solo.Gateway (gRPC server, mTLS)
```

---

## 5. Revised Module List

New modules (added or changed):

```
lib/solo/
├── event_store.ex              # NEW: Replayable event log
├── event.ex                    # NEW: Event schema
├── event_bus.ex                # NEW: Pub/sub for events
├── atom_monitor.ex             # NEW: Atom table safety
├── vault.ex                    # NEW: Secrets management
├── trust/
│   ├── engine.ex               # NEW: Trust score computation
│   ├── score.ex                # NEW: Trust score struct
│   └── policy.ex               # NEW: Trust-gated capability rules
├── backpressure/
│   ├── monitor.ex              # NEW: Mailbox monitoring
│   ├── circuit_breaker.ex      # NEW: Per-capability circuit breakers
│   └── load_shedder.ex         # NEW: Gateway-level rejection
├── pipeline/
│   ├── pipeline.ex             # NEW: Pipeline definition
│   ├── step.ex                 # NEW: Pipeline step
│   ├── executor.ex             # NEW: Pipeline runtime
│   └── supervisor.ex           # NEW: Pipeline process tree
├── hot_swap/
│   ├── hot_swap.ex             # NEW: Live code replacement
│   ├── watchdog.ex             # NEW: Rollback timer
│   └── state_migrator.ex       # NEW: State transformation
├── tenant/
│   ├── tenant_supervisor.ex    # CHANGED: Per-tenant supervisor
│   └── service_supervisor.ex   # CHANGED: Per-service supervisor
├── security/
│   ├── code_analyzer.ex        # NEW: Static analysis for dangerous patterns
│   ├── nif_guard.ex            # NEW: NIF detection and rejection
│   └── mtls.ex                 # NEW: Certificate management
└── persistence/
    ├── persistence.ex          # CHANGED: CubDB interface
    └── cubdb_backend.ex        # NEW: Replaces rocksdb_backend.ex
```

---

## 6. Updated Proto Definitions (Additions)

```protobuf
// Additions to solo.proto

service SoloKernel {
  // ... existing RPCs ...

  // Hot code swap
  rpc HotSwap(HotSwapRequest) returns (HotSwapResponse);

  // Pipeline operations
  rpc DeployPipeline(PipelineDeployRequest) returns (PipelineDeployResponse);
  rpc ExecutePipeline(PipelineExecuteRequest) returns (PipelineExecuteResponse);

  // Trust queries
  rpc GetTrustScore(TrustScoreRequest) returns (TrustScoreResponse);

  // Vault operations
  rpc StoreSecret(StoreSecretRequest) returns (StoreSecretResponse);
  rpc FetchSecret(FetchSecretRequest) returns (FetchSecretResponse);

  // Event replay
  rpc ReplayEvents(ReplayRequest) returns (stream Event);
}

message HotSwapRequest {
  string service_id = 1;
  bytes new_code = 2;
  CodeFormat format = 3;
  int32 rollback_window_ms = 4;
  bytes state_migration_code = 5;
}

message TrustScoreResponse {
  string service_id = 1;
  float score = 2;
  string tier = 3;  // "untrusted", "provisional", "trusted", "privileged"
  int32 successful_ops = 4;
  int32 denied_ops = 5;
  int32 crashes = 6;
}

message ReplayRequest {
  string tenant_id = 1;      // Filter by tenant
  string service_id = 2;     // Filter by service (optional)
  int64 since_timestamp = 3; // Replay from this time
  int64 until_timestamp = 4; // Replay until this time (optional)
}
```

---

## 7. Updated Dependencies

```elixir
defp deps do
  [
    # Core
    {:grpc, "~> 0.6.0"},
    {:protobuf, "~> 0.11.0"},
    {:google_protos, "~> 0.3"},

    # Persistence (CHANGED: CubDB replaces RocksDB)
    {:cubdb, "~> 2.0"},          # Pure Elixir, ACID, no NIFs

    # Observability
    {:prometheus_ex, "~> 3.0"},
    {:telemetry, "~> 1.2"},      # NEW: Event pipeline
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"},

    # Security
    {:x509, "~> 0.8"},          # NEW: Certificate generation for mTLS

    # Testing
    {:stream_data, "~> 0.6", only: :test},
    {:mox, "~> 1.0", only: :test},

    # Code quality
    {:credo, "~> 1.7", only: [:dev, :test]},
    {:dialyxir, "~> 1.4", only: [:dev, :test]},
    {:ex_doc, "~> 0.30", only: :dev},
  ]
end
```

**Removed:** `:rocksdb`, `:rustler` (no NIFs in kernel for MVP)  
**Added:** `:cubdb`, `:x509`, `:telemetry`, `:telemetry_metrics`, `:telemetry_poller`

---

## 8. Updated Threat Model

| Threat | v1.0 Coverage | v1.1 Coverage | Change |
|--------|:---:|:---:|--------|
| Atom table exhaustion | ❌ Not addressed | ✅ Monitor + static analysis | NEW |
| NIF VM crash | ❌ Not addressed | ✅ Banned for user services | NEW |
| Cross-tenant restart cascade | ❌ Shared supervisor | ✅ Per-tenant supervisors | FIXED |
| Unauthenticated RCE | ❌ No auth | ✅ mTLS from day one | FIXED |
| Backpressure/DoS | ⚠️ Resource limits only | ✅ Circuit breakers + load shedding | IMPROVED |
| Secret exposure | ❌ Not addressed | ✅ Encrypted vault | NEW |
| Service memory isolation | ✅ Actor model | ✅ Actor model | Unchanged |
| Confused deputy | ✅ Capability tokens | ✅ Capability tokens + trust | IMPROVED |
| Privilege escalation | ✅ Attenuation | ✅ Attenuation + trust tiers | IMPROVED |
| Resource exhaustion | ✅ Limits | ✅ Limits + backpressure | IMPROVED |

---

## 9. What This Design Does NOT Address (Conscious Deferrals)

1. **Formal verification of isolation** — Would require TLA+ or similar. Deferred to post-MVP.
2. **WASM sandboxing** — Could provide stronger isolation than BEAM processes. Research needed.
3. **Distributed consensus** — Multi-machine Solo needs a consensus protocol. Not in MVP.
4. **Code signing** — mTLS authenticates the agent, not the code. Code signing deferred.
5. **GPU/accelerator access** — Needs investigation. Likely requires Port-based isolation.
6. **Rate limiting per operation** — Load shedding is per-tenant; per-operation limits deferred.

---

## 10. Summary of Changes from v1.0

| Area | v1.0 | v1.1 |
|------|------|------|
| Atom safety | Not addressed | Static analysis + runtime monitor |
| NIFs | Allowed (Rust) | Banned for user services; kernel uses zero NIFs |
| Supervisor tree | Flat (one DynamicSupervisor) | Hierarchical (tenant → service) |
| Authentication | None (MVP) | mTLS from day one |
| Backpressure | Not addressed | Mailbox monitor + circuit breakers + load shedding |
| Persistence | RocksDB (NIF) | CubDB (pure Elixir) |
| Secrets | Not addressed | Solo.Vault with encrypted storage |
| Audit log | Write-only | Replayable event store |
| Code updates | Phase 8 afterthought | First-class hot swap with rollback |
| Trust model | Static capabilities | Adaptive trust with earned capabilities |
| Service composition | Not addressed | Typed capability-aware pipelines |
| Rust NIFs | Strategic from day 1 | Zero NIFs in MVP; add only for proven bottlenecks |

---

**Document Version:** 1.1  
**Date:** 2026-02-08  
**Status:** Approved amendments — ready for implementation
