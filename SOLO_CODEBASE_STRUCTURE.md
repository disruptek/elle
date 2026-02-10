# Solo Codebase Structure and Organization

**Project**: Solo - User-Level Operating System for LLM Agents  
**Language**: Elixir 1.19.5 / OTP 28.3.1  
**Architecture**: Event-driven, capability-based multi-tenant system  
**Total Modules**: 43 source files  
**Total Tests**: 18 test files  

---

## 1. Directory Layout

```
/home/adavidoff/git/solo/
├── lib/                           # Main source code (43 files)
│   ├── solo_cli.ex               # CLI entry point
│   └── solo/                      # Main namespace
│       ├── application.ex         # Application startup & initialization
│       ├── kernel.ex              # Root supervisor
│       ├── event.ex               # Event struct definition
│       ├── event_store.ex         # EventStore GenServer (CubDB-backed)
│       ├── registry.ex            # Service discovery registry
│       ├── service_registry.ex    # Service metadata registry
│       ├── vault.ex               # Encrypted secret storage
│       ├── config.ex              # Configuration management
│       ├── atom_monitor.ex        # Atom table monitoring
│       ├── gateway.ex             # Dual gRPC/HTTP gateway
│       ├── gateway/               # REST API handlers
│       │   ├── server.ex          # gRPC server definition
│       │   ├── health_handler.ex  # Health checks
│       │   ├── metrics_handler.ex # Metrics endpoints
│       │   ├── not_found_handler.ex
│       │   └── rest/              # REST API handlers
│       │       ├── router.ex      # HTTP route dispatcher
│       │       ├── helpers.ex     # Utilities
│       │       ├── service_handler.ex
│       │       ├── services_handler.ex
│       │       ├── events_handler.ex
│       │       ├── logs_handler.ex
│       │       └── secrets_handler.ex
│       ├── deployment/            # Deployment & compilation
│       │   ├── deployer.ex        # Service lifecycle manager
│       │   └── compiler.ex        # Elixir code compiler
│       ├── capability/            # Access control
│       │   ├── capability.ex      # Capability token struct
│       │   ├── manager.ex         # Capability lifecycle manager
│       │   └── attenuated.ex      # Capability attenuation
│       ├── resource/              # Resource management
│       │   ├── limits.ex          # Resource limit definitions
│       │   └── monitor.ex         # Runtime resource monitoring
│       ├── backpressure/          # Overload protection
│       │   ├── circuit_breaker.ex # Circuit breaker pattern
│       │   └── load_shedder.ex    # Load shedding
│       ├── tenant/                # Multi-tenant isolation
│       │   ├── supervisor.ex      # Top-level tenant supervisor
│       │   └── service_supervisor.ex # Per-tenant service supervisor
│       ├── hot_swap/              # Live code replacement
│       │   ├── hot_swap.ex        # Hot swap coordinator
│       │   └── watchdog.ex        # Crash detection & rollback
│       ├── security/              # Security features
│       │   └── mtls.ex            # mTLS certificate management
│       ├── hardening/             # Code safety analysis
│       │   ├── code_analyzer.ex   # Static analysis
│       │   └── hardening.ex       # Main hardening module
│       ├── system/                # System services
│       │   └── supervisor.ex      # Root system supervisor
│       ├── telemetry/             # Observability
│       │   ├── telemetry.ex       # Main telemetry module
│       │   └── prometheus.ex      # Prometheus metrics
│       └── v1/                    # Generated protobuf files
│           ├── solo.grpc.pb.ex
│           └── solo.pb.ex
│
├── test/                          # Test files (18 files)
│   ├── test_helper.exs           # Test configuration
│   ├── support/                   # Test helpers
│   └── solo/                      # Test modules
│       ├── atom_monitor_test.exs
│       ├── event_store_test.exs
│       ├── registry_test.exs
│       ├── vault_test.exs
│       ├── telemetry_test.exs
│       ├── supervisor_tree_test.exs
│       ├── backpressure/
│       │   ├── circuit_breaker_test.exs
│       │   └── load_shedder_test.exs
│       ├── capability/
│       │   ├── capability_test.exs
│       │   ├── attenuated_test.exs
│       │   └── manager_test.exs
│       ├── deployment/
│       │   ├── compiler_test.exs
│       │   └── deployer_test.exs
│       ├── gateway/rest/
│       │   ├── logs_handler_test.exs
│       │   └── secrets_handler_test.exs
│       ├── hot_swap/
│       │   └── hot_swap_test.exs
│       └── resource/
│           └── limits_test.exs
│
├── data/                         # Persistent data directory
│   ├── events/                   # CubDB: Event store database
│   └── vault/                    # CubDB: Encrypted secrets
│
├── priv/                         # Private resources
├── deps/                         # External dependencies
├── docs/                         # Documentation
├── mix.exs                       # Project manifest
├── mix.lock                      # Dependency lock file
└── config.example.toml           # Configuration template

```

---

## 2. Core Module Responsibilities

### 2.1 Startup & Initialization Chain

**Entry Point: `Solo.Application`**
- Supervisor: `:one_for_one` strategy
- Loading: Reads `SOLO_CONFIG` environment variable for config file
- Starts: `Solo.Kernel` as single child

```elixir
Solo.Application
  └── Solo.Kernel (root supervisor, :one_for_one)
       ├── Solo.System.Supervisor (:rest_for_one)
       │    ├── Solo.EventStore (CubDB append-only log)
       │    ├── Solo.AtomMonitor (runtime atom monitoring)
       │    ├── Solo.Registry (tenant -> service_id -> pid)
       │    ├── Solo.Deployment.Deployer (lifecycle manager)
       │    ├── Solo.Capability.Manager (capability lifecycle)
       │    ├── Solo.Backpressure.LoadShedder (overload protection)
       │    ├── Solo.Vault (encrypted secrets, CubDB)
       │    ├── Solo.ServiceRegistry (service discovery)
       │    ├── Solo.Telemetry (observability)
       │    └── Solo.Gateway (gRPC + REST servers)
       │
       └── Solo.Tenant.Supervisor (DynamicSupervisor)
            └── Solo.Tenant.ServiceSupervisor (per-tenant, created on-demand)
                 └── User Service GenServers (dynamically started)
```

**Key Property**: Supervisor order matters due to `:rest_for_one` strategy.

---

### 2.2 EventStore Module

**File**: `lib/solo/event_store.ex`  
**Type**: GenServer (named)  
**Persistence**: CubDB (./data/events)  
**Pattern**: Append-only event log

**Responsibilities**:
- Emit events asynchronously (GenServer.cast) to avoid blocking critical path
- Stream events with optional filters (tenant_id, service_id, time range)
- Calculate monotonic sequence IDs (never reuse, no gaps)
- Persist events to disk via CubDB

**Data Structure**:
```elixir
defstruct [
  :id,                 # Monotonically increasing (not UUID)
  :timestamp,          # Erlang monotonic_time
  :wall_clock,         # DateTime UTC
  :tenant_id,          # nil for system events
  :event_type,         # :service_deployed, :capability_granted, etc.
  :subject,            # What event is about (e.g., {tenant_id, service_id})
  :payload,            # Event-specific data (map)
  :causation_id        # ID of event that caused this one
]
```

**Event Types**:
- `:system_started`, `:service_deployed`, `:service_deployment_failed`
- `:service_started`, `:service_killed`, `:service_crashed`
- `:atom_usage_high`, `:resource_violation`
- `:capability_granted`, `:capability_revoked`, `:capability_denied`
- `:hot_swap_started`, `:hot_swap_succeeded`, `:hot_swap_rolled_back`
- `:secret_stored`, `:secret_accessed`, `:secret_access_denied`

**Public API**:
```elixir
emit(event_type, subject, payload, tenant_id, causation_id)  # async cast
stream(opts)                                                   # returns Enumerable
filter(opts)                                                   # returns list
last_id()                                                      # returns current max ID
reset!()                                                       # testing only
```

**CubDB Storage Keys**:
- `:next_id` - Counter for sequence IDs
- `{:event, id}` - Event data by ID

---

### 2.3 ServiceRegistry Module

**File**: `lib/solo/service_registry.ex`  
**Type**: GenServer (named)  
**Storage**: In-memory maps with indices  
**Pattern**: Service discovery and metadata

**Responsibilities**:
- Register services with metadata (tags, version, environment)
- Discover services by name with optional filters
- List all services for a tenant
- Track TTL-based expiration with periodic cleanup
- Maintain three indices for fast lookup

**Data Structures**:
```elixir
# In state:
registrations:      # tenant_id -> service_id -> registration
name_index:         # service_name -> [registrations]
handle_map:         # handle (random UUID) -> {tenant_id, service_id}

# Registration record:
%{
  tenant_id: String,
  service_id: String,
  service_name: String,
  version: String,
  metadata: Map,       # Custom key-value pairs
  handle: String,      # Unforgeable identifier
  registered_at: integer (milliseconds),
  expires_at: integer (milliseconds) or nil
}
```

**Public API**:
```elixir
register(tenant_id, service_id, service_name, version, metadata, ttl_seconds)
discover(tenant_id, service_name, filters)                    # returns list
list_services(tenant_id, service_name)                        # returns list
get_metadata(tenant_id, service_id)                           # returns map or nil
get_handle(tenant_id, service_id)                             # returns handle or nil
unregister(tenant_id, service_id)
```

**Cleanup**: Periodic `:cleanup_expired` task (every 60 seconds)

---

### 2.4 Deployer Module

**File**: `lib/solo/deployment/deployer.ex`  
**Type**: GenServer (named)  
**Responsibility**: Service lifecycle management (deploy, kill, status, list)

**Key Features**:
- Compiles Elixir source code via `Solo.Deployment.Compiler`
- Starts services under tenant supervisors (dynamic workers)
- Tracks deployed services: `%{tenant_id => %{service_id => pid}}`
- Monitors service crashes (`:DOWN` messages)
- Emits events for observability
- Validates service module exports `start_link/1`

**Deployment Spec**:
```elixir
%{
  tenant_id: String,                    # required
  service_id: String,                   # required
  code: String,                         # Elixir source, required
  format: :elixir_source,               # required, only format in Phase 2
  restart_limits: %{                    # optional
    max_restarts: integer,
    max_seconds: integer,
    startup_timeout_ms: integer,
    shutdown_timeout_ms: integer
  }
}
```

**Public API**:
```elixir
deploy(spec)                            # returns {:ok, pid} | {:error, reason}
kill(tenant_id, service_id, opts)       # timeout, force options
status(tenant_id, service_id)           # returns map or {:error, :not_found}
list(tenant_id)                         # returns [{service_id, pid}, ...]
```

**Internal Flow**:
1. Validate format (only :elixir_source)
2. Get or create tenant supervisor via `Solo.Tenant.Supervisor`
3. Compile code via `Solo.Deployment.Compiler`
4. Validate module exports `start_link/1`
5. Start child in tenant supervisor with spec
6. Register in `Solo.Registry` and track locally
7. Emit `:service_deployed` event
8. Monitor process for crashes

---

### 2.5 Registry Module (Service Discovery)

**File**: `lib/solo/registry.ex`  
**Type**: Thin wrapper around Elixir's Registry  
**Pattern**: `:unique` keys (one PID per key)

**Key Format**: `{tenant_id, service_id}` tuple

**Responsibilities**:
- Register service processes by tenant + service ID
- Fast lookup of running services
- List all services for a tenant
- Unregister when services terminate

**Public API**:
```elixir
register(tenant_id, service_id, pid)    # {:ok, pid} | {:error, {:already_registered, pid}}
lookup(tenant_id, service_id)           # [{pid, meta}] or []
list_for_tenant(tenant_id)              # [{service_id, pid}, ...]
unregister(tenant_id, service_id)
```

---

### 2.6 Capability System

**Capability Struct** (`lib/solo/capability/capability.ex`):
```elixir
defstruct [
  :resource_ref,      # String: "filesystem", "eventstore", etc.
  :token_hash,        # SHA-256 hash of token (secure storage)
  :permissions,       # [String]: ["read", "write", "delete"]
  :expires_at,        # Unix timestamp (seconds)
  :tenant_id,         # String
  revoked?: false     # Boolean
]
```

**Capability.Manager** (`lib/solo/capability/manager.ex`):
- GenServer managing capability lifecycle
- Stores capabilities in ETS `:capabilities` table
- Token hash -> capability mapping
- Periodic cleanup of expired tokens

**Public API**:
```elixir
grant(tenant_id, resource_ref, permissions, ttl_seconds)
  # returns {:ok, token}
  # token is opaque 32-byte binary
  
verify(token, resource_ref, required_permission)
  # returns :ok | {:error, reason}
  
revoke(token_hash)
  # returns :ok
```

**Key Security Properties**:
- Tokens are 256-bit random bytes (generated via `:crypto.strong_rand_bytes(32)`)
- Only token hashes stored (SHA-256)
- TTL enforced (checked against `System.system_time(:second)`)
- Revocation via token hash lookup

---

### 2.7 Vault Module (Encrypted Secrets)

**File**: `lib/solo/vault.ex`  
**Type**: GenServer (named)  
**Persistence**: CubDB (./data/vault)  
**Encryption**: AES-256-GCM (authenticated encryption)

**Security**:
- Algorithm: AES-256-GCM (AEAD cipher)
- IV: 12 bytes (96 bits), randomly generated per secret
- Auth tag: 16 bytes (128 bits)
- Key derivation: SHA-256 (production should use PBKDF2)
- Storage format: IV (12) + tag (16) + ciphertext

**Data Structure** (CubDB key format):
```elixir
{:secret, tenant_id, secret_name} => encrypted_data_blob
```

**Public API**:
```elixir
store(tenant_id, secret_name, secret_value, key, opts)
  # returns :ok | {:error, reason}
  
retrieve(tenant_id, secret_name, key)
  # returns {:ok, secret_value} | {:error, reason}
  
list_secrets(tenant_id)
  # returns {:ok, [secret_names]} | {:error, reason}
  
revoke(tenant_id, secret_name)
  # soft delete, returns :ok | {:error, reason}
```

**Events Emitted**:
- `:secret_stored` - on successful storage
- `:secret_accessed` - on successful retrieval
- `:secret_access_denied` - on decryption failure
- `:secret_revoked` - on revocation

---

### 2.8 Compiler Module

**File**: `lib/solo/deployment/compiler.ex`  
**Responsibility**: Compile Elixir source code to BEAM bytecode

**Key Features**:
- Wraps user code in a namespace to prevent collisions
- Namespace format: `Solo.User{tenant_id}{service_id}` (sanitized)
- Uses `Code.compile_string/2` for dynamic compilation
- Returns list of `{module, bytecode}` tuples
- No static analysis (that's Phase 8: CodeAnalyzer)

**Public API**:
```elixir
compile(tenant_id, service_id, source_code)
  # returns {:ok, [{module, bytecode}, ...]} | {:error, reason}
  
namespace(tenant_id, service_id)
  # returns "Solo.User_tenant_id_service_id"
```

**Namespace Sanitization**:
- Replace non-alphanumeric with `_`
- Strip leading/trailing `_`
- Prepend `_` prefix

---

### 2.9 Tenant Isolation

**Solo.Tenant.Supervisor** (`lib/solo/tenant/supervisor.ex`):
- Top-level DynamicSupervisor
- Creates per-tenant supervisors on-demand
- One supervisor per tenant

**Solo.Tenant.ServiceSupervisor** (`lib/solo/tenant/service_supervisor.ex`):
- Per-tenant supervisor (created dynamically)
- DynamicSupervisor with `:one_for_one` strategy
- Manages all services for one tenant
- Registered in `Solo.Registry` with key `{:tenant, tenant_id}`

**Isolation Properties**:
- Services from different tenants in separate supervisor trees
- Each service is a separate GenServer process
- Crash of one service doesn't affect other tenants
- Resource limits can be enforced per tenant

---

### 2.10 Gateway (Dual Protocol)

**File**: `lib/solo/gateway.ex`  
**Type**: GenServer  
**Ports**: 
- gRPC: 50051
- HTTP (REST): 8080

**Features**:
- mTLS authentication (client cert -> tenant_id)
- gRPC service definitions in `v1/solo.grpc.pb.ex`
- REST endpoints via Cowboy HTTP server

**gRPC RPCs** (Phase 3):
- Deploy, Kill, Status, List, Watch, Shutdown

**REST Endpoints**:
```
POST   /services           - Deploy service
GET    /services           - List services
GET    /services/{id}      - Get service status
DELETE /services/{id}      - Kill service
GET    /events             - Stream events (Server-Sent Events)
GET    /health             - Health check
```

**Handler Structure**:
- `gateway/rest/router.ex` - Cowboy dispatch routing
- `gateway/rest/helpers.ex` - Utility functions
- `gateway/rest/service_handler.ex` - Single service operations
- `gateway/rest/services_handler.ex` - Bulk operations
- `gateway/rest/events_handler.ex` - Event streaming
- `gateway/rest/logs_handler.ex` - Log access
- `gateway/rest/secrets_handler.ex` - Secret management

---

### 2.11 System Services

**Solo.System.Supervisor** (`lib/solo/system/supervisor.ex`):
- Uses `:rest_for_one` strategy
- Ordered list of critical services
- Order: EventStore → AtomMonitor → Registry → Deployer → Capability.Manager → LoadShedder → Vault → ServiceRegistry → Telemetry → Gateway

**Solo.AtomMonitor** (`lib/solo/atom_monitor.ex`):
- Monitors runtime atom table usage
- Emits `:atom_usage_high` events
- Prevents DoS from unbounded atom creation

**Solo.Backpressure.LoadShedder** (`lib/solo/backpressure/load_shedder.ex`):
- Circuit breaker pattern
- Sheds load when overloaded
- Fair distribution across tenants

---

### 2.12 Configuration

**File**: `lib/solo/config.ex`  
**Loading**: Via `SOLO_CONFIG` env var in `Solo.Application.load_configuration/0`

**Default Configuration**:
```elixir
%{
  solo: %{
    listen_port: 50051,
    http_port: 8080,
    data_dir: "./data",
    max_tenants: 100,
    log_level: "info"
  },
  limits: %{
    max_per_tenant: 100,
    max_total: 1000
  },
  telemetry: %{
    enabled: true,
    log_events: true
  },
  security: %{
    require_mtls: false,
    rate_limit_per_capability: 1000
  },
  database: %{
    events_db: "./data/events",
    vault_db: "./data/vault"
  }
}
```

---

## 3. Persistence Layer

### CubDB Usage

**EventStore** (`./data/events`):
- Key-value pairs:
  - `:next_id` → integer (counter)
  - `{:event, id}` → Solo.Event struct
- Access pattern: Sequential scan, no filtering in DB

**Vault** (`./data/vault`):
- Key-value pairs:
  - `{:secret, tenant_id, secret_name}` → encrypted_blob
- Access pattern: Point lookups and scans by tenant

**Why CubDB**:
- Embedded, no external server
- ACID properties
- Performant range queries
- Written in pure Elixir

---

## 4. Data Structures

### Services & Metadata

**Service Registration** (ServiceRegistry):
```elixir
%{
  tenant_id: String,
  service_id: String,
  service_name: String,
  version: String,
  metadata: Map,
  handle: String,
  registered_at: integer,
  expires_at: integer | nil
}
```

**Deployment Tracking** (Deployer state):
```elixir
%{
  services: %{
    tenant_id => %{
      service_id => pid
    }
  }
}
```

**Capability** (stored in Capability.Manager ETS):
```elixir
%Solo.Capability{
  resource_ref: String,
  token_hash: binary,
  permissions: [String],
  expires_at: integer,
  tenant_id: String,
  revoked?: boolean
}
```

---

## 5. Startup Sequence

1. **Application Starts** (`Solo.Application.start/2`)
   - Load configuration from `$SOLO_CONFIG` file
   - Apply to application environment
   - Emit system initialization logs

2. **Kernel Starts** (`Solo.Kernel.init/1`)
   - Start `Solo.System.Supervisor`
   - Start `Solo.Tenant.Supervisor`

3. **System Services** (in order, `:rest_for_one`):
   ```
   EventStore       → Initialize CubDB, get next_id counter
   AtomMonitor      → Start monitoring atom table
   Registry         → Create Registry
   Deployer         → Empty services map
   Capability.Mgr   → Create ETS :capabilities table
   LoadShedder      → Initialize
   Vault            → Initialize CubDB for secrets
   ServiceRegistry  → Empty registrations
   Telemetry        → Start metric collectors
   Gateway          → Start gRPC (port 50051) + HTTP (port 8080)
   ```

4. **Ready for Requests**
   - Agents can connect via gRPC or REST
   - Deploy services by sending code
   - Receive events via streaming

---

## 6. Key Architectural Patterns

### Pattern 1: Event-Driven Audit Trail
- Every significant operation emits an event
- Events are immutable, append-only
- CubDB provides durability
- EventStore uses GenServer.cast to avoid blocking

### Pattern 2: Capability-Based Security
- Services receive tokens, not passwords
- Tokens are cryptographically unforgeable (256-bit random)
- Permissions are checked at runtime
- TTL prevents unauthorized long-lived access

### Pattern 3: Multi-Tenant Isolation
- Supervisor trees per tenant
- Registry keys include tenant_id
- Service discovery scoped to tenant
- Resource limits per tenant

### Pattern 4: Supervisor-Based Fault Tolerance
- Crash recovery via supervisor restarts
- Clear hierarchy: System → Tenant → Service
- Different strategies at each level
- Process monitoring via `:DOWN` messages

### Pattern 5: Dynamic Compilation
- Code compiled at runtime (not pre-compiled)
- Each service gets unique namespace
- Validation of module interface (start_link/1)
- No static analysis in Phase 2

### Pattern 6: CubDB for Embedded Persistence
- No external database server
- Event log stored as key-value pairs
- Secrets encrypted before storage
- Automatic compaction and durability

---

## 7. Testing Structure

**Test Files** (18 total):
- Unit tests for core modules
- Property-based tests (stream_data)
- Mock dependencies (mox)
- Integration tests for supervisor tree

**Test Coverage**:
- EventStore functionality
- Service deployment & lifecycle
- Capability creation & verification
- Registry lookups
- Vault encryption/decryption
- Configuration loading
- Supervisor startup

---

## 8. Key Dependencies

**Core**:
- `cubdb ~> 2.0` - Embedded key-value database
- `grpc ~> 0.9` - gRPC framework
- `protobuf ~> 0.13` - Protocol buffers
- `google_protos ~> 0.1` - Standard Google protos

**Security**:
- `x509 ~> 0.8` - mTLS certificate handling

**Observability**:
- `telemetry ~> 1.2` - Metrics & events
- `telemetry_metrics ~> 0.6` - Metric definitions
- `telemetry_poller ~> 1.0` - Polling metrics
- `telemetry_metrics_prometheus_core ~> 1.1` - Prometheus export

**Configuration**:
- `toml ~> 0.7` - TOML config parsing

**Testing**:
- `stream_data ~> 1.0` - Property-based testing
- `mox ~> 1.0` - Mocking library
- `credo ~> 1.7` - Code quality
- `dialyxir ~> 1.4` - Type checking

---

## 9. Summary Table

| Component | Module | Type | Key Responsibility |
|-----------|--------|------|-------------------|
| Application | `Solo.Application` | App | Load config, start kernel |
| Kernel | `Solo.Kernel` | Supervisor | Root supervision tree |
| System Supervisor | `Solo.System.Supervisor` | Supervisor | Manage system services |
| Event Store | `Solo.EventStore` | GenServer | Append-only audit log |
| Registry | `Solo.Registry` | Registry | Service discovery |
| Service Registry | `Solo.ServiceRegistry` | GenServer | Service metadata |
| Deployer | `Solo.Deployment.Deployer` | GenServer | Deploy & manage services |
| Compiler | `Solo.Deployment.Compiler` | Module | Compile user code |
| Capabilities | `Solo.Capability.Manager` | GenServer | Access control tokens |
| Vault | `Solo.Vault` | GenServer | Encrypted secrets |
| Tenant Supervisor | `Solo.Tenant.Supervisor` | Supervisor | Per-tenant isolation |
| Tenant Services | `Solo.Tenant.ServiceSupervisor` | Supervisor | Tenant's services |
| Gateway | `Solo.Gateway` | GenServer | gRPC + REST API |
| Config | `Solo.Config` | Module | Configuration loading |
| Telemetry | `Solo.Telemetry` | GenServer | Observability |
| Atom Monitor | `Solo.AtomMonitor` | GenServer | Atom table monitoring |
| Load Shedder | `Solo.Backpressure.LoadShedder` | GenServer | Overload protection |

---

## 10. Critical Startup Dependencies

```
Application.start
  ↓
Kernel.init
  ├─→ System.Supervisor (rest_for_one)
  │    ├─→ EventStore (init CubDB counter)
  │    ├─→ AtomMonitor
  │    ├─→ Registry (Elixir Registry)
  │    ├─→ Deployer (empty services)
  │    ├─→ Capability.Manager (create ETS)
  │    ├─→ LoadShedder
  │    ├─→ Vault (init CubDB)
  │    ├─→ ServiceRegistry (empty regs)
  │    ├─→ Telemetry
  │    └─→ Gateway (start gRPC + HTTP)
  │
  └─→ Tenant.Supervisor (DynamicSupervisor, empty)
```

If EventStore dies, all later services are restarted (rest_for_one).

---

## 11. Configuration File Example

```toml
[solo]
listen_port = 50051
http_port = 8080
data_dir = "./data"
max_tenants = 100
log_level = "info"

[limits]
max_per_tenant = 100
max_total = 1000

[telemetry]
enabled = true
log_events = true

[security]
require_mtls = false
rate_limit_per_capability = 1000

[database]
events_db = "./data/events"
vault_db = "./data/vault"
```

Load via: `export SOLO_CONFIG=/path/to/config.toml`

---

## 12. Monitoring & Observability Points

**Events Emitted**:
- System: `:system_started`
- Services: `:service_deployed`, `:service_started`, `:service_killed`, `:service_crashed`
- Capabilities: `:capability_granted`, `:capability_revoked`, `:capability_denied`
- Secrets: `:secret_stored`, `:secret_accessed`, `:secret_access_denied`
- Hot Swap: `:hot_swap_started`, `:hot_swap_succeeded`, `:hot_swap_rolled_back`
- Resources: `:atom_usage_high`, `:resource_violation`

**Telemetry Metrics**:
- Prometheus metrics via `Solo.Telemetry.Prometheus`
- Event measurements
- Service lifecycle counters

**Event Stream Access**:
- `Solo.EventStore.stream(opts)` - Get events as enumerable
- REST: `GET /events` - Server-Sent Events stream
- Filter by: `tenant_id`, `service_id`, `since_id`, `limit`

---

## Key Files to Understand First (Reading Order)

1. **`lib/solo/event.ex`** - Understand the Event struct
2. **`lib/solo/application.ex`** - Understand startup
3. **`lib/solo/kernel.ex`** - Understand supervision tree
4. **`lib/solo/system/supervisor.ex`** - Understand service ordering
5. **`lib/solo/event_store.ex`** - Understand persistence
6. **`lib/solo/deployment/deployer.ex`** - Understand service lifecycle
7. **`lib/solo/capability/capability.ex`** - Understand security model
8. **`lib/solo/vault.ex`** - Understand secret storage
9. **`lib/solo/gateway.ex`** - Understand API layer

