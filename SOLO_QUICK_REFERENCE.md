# Solo Quick Reference Guide

## Module Directory Map

| Path | Module | Purpose |
|------|--------|---------|
| `application.ex` | Solo.Application | App lifecycle, config loading |
| `kernel.ex` | Solo.Kernel | Root supervisor |
| `system/supervisor.ex` | Solo.System.Supervisor | System services root (rest_for_one) |
| `event.ex` | Solo.Event | Event struct definition |
| `event_store.ex` | Solo.EventStore | Append-only event log (CubDB) |
| `registry.ex` | Solo.Registry | Service discovery (Elixir Registry) |
| `service_registry.ex` | Solo.ServiceRegistry | Service metadata registry |
| `deployment/compiler.ex` | Solo.Deployment.Compiler | Elixir source compilation |
| `deployment/deployer.ex` | Solo.Deployment.Deployer | Lifecycle mgmt (deploy/kill/status) |
| `capability/capability.ex` | Solo.Capability | Capability token struct |
| `capability/manager.ex` | Solo.Capability.Manager | Capability lifecycle (ETS storage) |
| `capability/attenuated.ex` | Solo.Capability.Attenuated | Capability delegation |
| `vault.ex` | Solo.Vault | Encrypted secret storage (CubDB) |
| `tenant/supervisor.ex` | Solo.Tenant.Supervisor | Top-level tenant supervisor |
| `tenant/service_supervisor.ex` | Solo.Tenant.ServiceSupervisor | Per-tenant service supervisor |
| `gateway.ex` | Solo.Gateway | Dual gRPC/REST servers |
| `gateway/server.ex` | Solo.Gateway.Server | gRPC service handlers |
| `gateway/rest/router.ex` | Solo.Gateway.REST.Router | HTTP dispatch routing |
| `gateway/rest/service_handler.ex` | Solo.Gateway.REST.ServiceHandler | Single service REST ops |
| `gateway/rest/services_handler.ex` | Solo.Gateway.REST.ServicesHandler | Bulk service REST ops |
| `gateway/rest/events_handler.ex` | Solo.Gateway.REST.EventsHandler | Event streaming (SSE) |
| `gateway/rest/logs_handler.ex` | Solo.Gateway.REST.LogsHandler | Log access |
| `gateway/rest/secrets_handler.ex` | Solo.Gateway.REST.SecretsHandler | Secret management |
| `atom_monitor.ex` | Solo.AtomMonitor | Atom table monitoring |
| `config.ex` | Solo.Config | Configuration loading |
| `telemetry.ex` | Solo.Telemetry | Metrics & observability |
| `telemetry/prometheus.ex` | Solo.Telemetry.Prometheus | Prometheus export |
| `backpressure/load_shedder.ex` | Solo.Backpressure.LoadShedder | Load shedding |
| `backpressure/circuit_breaker.ex` | Solo.Backpressure.CircuitBreaker | Circuit breaker |
| `resource/limits.ex` | Solo.Resource.Limits | Resource limit definitions |
| `resource/monitor.ex` | Solo.Resource.Monitor | Runtime resource monitoring |
| `security/mtls.ex` | Solo.Security.MTLS | mTLS certificate management |
| `hot_swap/hot_swap.ex` | Solo.HotSwap | Live code replacement |
| `hot_swap/watchdog.ex` | Solo.HotSwap.Watchdog | Crash detection & rollback |
| `hardening/hardening.ex` | Solo.Hardening | Main hardening module |
| `hardening/code_analyzer.ex` | Solo.Hardening.CodeAnalyzer | Static code analysis |
| `v1/solo.pb.ex` | Solo.V1.Solo | Generated protobuf messages |
| `v1/solo.grpc.pb.ex` | Solo.V1.Solo.GRPC | Generated gRPC service |

---

## Key Function Signatures

### EventStore

```elixir
emit(event_type, subject, payload \\ %{}, tenant_id \\ nil, causation_id \\ nil) :: :ok
stream(opts \\ []) :: Enumerable.t()
  # opts: tenant_id, service_id, since_id, limit
filter(opts \\ []) :: list(Solo.Event.t())
last_id() :: non_neg_integer()
reset!() :: :ok
```

### ServiceRegistry

```elixir
register(tenant_id, service_id, service_name, version, metadata, ttl_seconds)
  :: {:ok, handle} | {:error, reason}
discover(tenant_id, service_name, filters \\ %{})
  :: {:ok, [registration]} | {:error, reason}
list_services(tenant_id, service_name \\ nil)
  :: {:ok, [registration]} | {:error, reason}
get_metadata(tenant_id, service_id) :: map() | nil
get_handle(tenant_id, service_id) :: String.t() | nil
unregister(tenant_id, service_id) :: :ok | {:error, reason}
```

### Deployer

```elixir
deploy(spec :: map()) :: {:ok, pid()} | {:error, String.t()}
  # spec keys: tenant_id, service_id, code, format, restart_limits
kill(tenant_id, service_id, opts \\ []) :: :ok | {:error, String.t()}
  # opts: timeout (ms), force (bool)
status(tenant_id, service_id) :: map() | {:error, :not_found}
list(tenant_id) :: [{service_id, pid}]
```

### Capability

```elixir
# Creation
create(resource_ref, permissions, ttl_seconds, tenant_id)
  :: {:ok, token, capability}
verify_token(token, capability) :: boolean()
valid?(capability) :: boolean()
allows?(capability, permission) :: boolean()
revoke(capability) :: capability
```

### Capability.Manager

```elixir
grant(tenant_id, resource_ref, permissions, ttl_seconds)
  :: {:ok, token}
verify(token, resource_ref, required_permission)
  :: :ok | {:error, reason}
revoke(token_hash) :: :ok
```

### Vault

```elixir
store(tenant_id, secret_name, secret_value, key, opts \\ [])
  :: :ok | {:error, String.t()}
retrieve(tenant_id, secret_name, key)
  :: {:ok, secret_value} | {:error, String.t()}
list_secrets(tenant_id)
  :: {:ok, [secret_names]} | {:error, String.t()}
revoke(tenant_id, secret_name)
  :: :ok | {:error, String.t()}
```

### Registry

```elixir
register(tenant_id, service_id, pid)
  :: {:ok, pid} | {:error, {:already_registered, pid}}
lookup(tenant_id, service_id) :: [{pid, meta}] | []
list_for_tenant(tenant_id) :: [{service_id, pid}]
unregister(tenant_id, service_id) :: :ok
```

### Compiler

```elixir
compile(tenant_id, service_id, source_code)
  :: {:ok, [{module, bytecode}]} | {:error, String.t()}
namespace(tenant_id, service_id) :: String.t()
```

---

## Event Types Emitted

```elixir
:system_started                 # System initialization
:service_deployed               # Service successfully deployed
:service_deployment_failed      # Deployment failed
:service_started                # Service process started
:service_killed                 # Service intentionally killed
:service_crashed                # Service crashed unexpectedly
:atom_usage_high                # Atom table usage exceeded threshold
:resource_violation             # Resource limit exceeded
:capability_granted             # Capability token created
:capability_revoked             # Capability revoked
:capability_denied              # Capability access denied
:capability_verified            # Capability verified successfully
:hot_swap_started               # Hot code replacement started
:hot_swap_succeeded             # Hot swap completed successfully
:hot_swap_rolled_back           # Hot swap aborted, rolled back
:secret_stored                  # Secret encrypted and stored
:secret_accessed                # Secret retrieved successfully
:secret_access_denied           # Secret decryption failed
```

---

## CubDB Storage Keys

### EventStore (`./data/events`)

```elixir
:next_id                        # Next event sequence number
{:event, id}                    # Event struct by ID
```

### Vault (`./data/vault`)

```elixir
{:secret, tenant_id, secret_name}  # Encrypted secret blob
```

---

## Configuration Keys (Default Values)

```toml
[solo]
listen_port = 50051            # gRPC port
http_port = 8080               # REST API port
data_dir = "./data"            # Base data directory
max_tenants = 100              # Max concurrent tenants
log_level = "info"             # Erlang logger level

[limits]
max_per_tenant = 100           # Max services per tenant
max_total = 1000               # Max services total

[telemetry]
enabled = true                 # Enable metrics
log_events = true              # Log all events

[security]
require_mtls = false           # Require mTLS authentication
rate_limit_per_capability = 1000  # Requests per minute

[database]
events_db = "./data/events"    # EventStore CubDB path
vault_db = "./data/vault"      # Vault CubDB path
```

---

## REST API Endpoints

```
POST   /services                      Deploy service
GET    /services                      List services
GET    /services/{service_id}         Get service status
DELETE /services/{service_id}         Kill service
GET    /events                        Stream events (SSE)
GET    /health                        Health check
```

### Deploy Payload

```json
{
  "tenant_id": "agent_1",
  "service_id": "my_service",
  "code": "defmodule MyService do ... end",
  "format": "elixir_source",
  "restart_limits": {
    "max_restarts": 5,
    "max_seconds": 60
  }
}
```

---

## gRPC Service Definition

```protobuf
service Solo {
  rpc Deploy(DeployRequest) returns (DeployResponse);
  rpc Kill(KillRequest) returns (KillResponse);
  rpc Status(StatusRequest) returns (StatusResponse);
  rpc List(ListRequest) returns (ListResponse);
  rpc Watch(WatchRequest) returns (stream Event);
  rpc Shutdown(ShutdownRequest) returns (ShutdownResponse);
}
```

---

## Supervisor Strategy Reference

| Supervisor | Strategy | Restarts When | Details |
|-----------|----------|---------------|---------|
| Solo | :one_for_one | Any child dies | Only that child restarted |
| System.Supervisor | :rest_for_one | Any child dies | All later children restarted |
| Tenant.Supervisor | (Dynamic) | New tenant | Create new ServiceSupervisor |
| Tenant.ServiceSupervisor | :one_for_one | Service dies | Only that service restarted |

---

## Startup Sequence Checklist

- [ ] Solo.Application.start() called
- [ ] Load $SOLO_CONFIG configuration file
- [ ] Start Solo.Kernel
- [ ] Start Solo.System.Supervisor (:rest_for_one)
  - [ ] 1. EventStore (CubDB init, get counter)
  - [ ] 2. AtomMonitor (start monitoring)
  - [ ] 3. Registry (create Registry)
  - [ ] 4. Deployer (initialize)
  - [ ] 5. Capability.Manager (create ETS)
  - [ ] 6. LoadShedder (initialize)
  - [ ] 7. Vault (CubDB init)
  - [ ] 8. ServiceRegistry (initialize)
  - [ ] 9. Telemetry (start metrics)
  - [ ] 10. Gateway (start gRPC:50051 + HTTP:8080)
- [ ] Start Solo.Tenant.Supervisor (DynamicSupervisor)
- [ ] System ready for deployments

---

## Common Operations

### Deploy a Service

```elixir
spec = %{
  tenant_id: "agent_1",
  service_id: "calculator",
  code: """
  defmodule Calculator do
    def start_link(_opts) do
      {:ok, self()}
    end
  end
  """,
  format: :elixir_source
}

{:ok, pid} = Solo.Deployment.Deployer.deploy(spec)
```

### Get Service Status

```elixir
status = Solo.Deployment.Deployer.status("agent_1", "calculator")
# Returns: %{pid: ..., service_id: ..., tenant_id: ..., alive: true, info: ...}
```

### Kill a Service

```elixir
:ok = Solo.Deployment.Deployer.kill("agent_1", "calculator", timeout: 5000)
```

### Grant a Capability

```elixir
{:ok, token} = Solo.Capability.Manager.grant(
  "agent_1",
  "eventstore",
  ["read", "write"],
  3600  # TTL in seconds
)
```

### Verify a Capability

```elixir
:ok = Solo.Capability.Manager.verify(token, "eventstore", "read")
# or {:error, reason}
```

### Store a Secret

```elixir
:ok = Solo.Vault.store(
  "agent_1",
  "db_password",
  "super_secret_123",
  "master_key_from_agent"
)
```

### Retrieve a Secret

```elixir
{:ok, value} = Solo.Vault.retrieve(
  "agent_1",
  "db_password",
  "master_key_from_agent"
)
```

### Stream Events

```elixir
Solo.EventStore.stream(tenant_id: "agent_1")
|> Stream.take(10)
|> Enum.each(&IO.inspect/1)
```

### Filter Events

```elixir
events = Solo.EventStore.filter(
  event_type: :service_deployed,
  tenant_id: "agent_1"
)
```

---

## Testing

### Run All Tests

```bash
mix test
```

### Run Specific Test File

```bash
mix test test/solo/event_store_test.exs
```

### Run Single Test

```bash
mix test test/solo/event_store_test.exs:22
```

### Run with Coverage

```bash
mix test --cover
```

---

## Debugging Tips

### Check Supervisor Tree

```elixir
Supervisor.which_children(Solo.Supervisor)
Supervisor.which_children(Solo.System.Supervisor)
Supervisor.which_children(Solo.Tenant.Supervisor)
```

### Get Running Services

```elixir
Solo.Deployment.Deployer.list("agent_1")
# Returns: [{service_id, pid}, ...]
```

### Lookup Service in Registry

```elixir
Solo.Registry.lookup("agent_1", "my_service")
# Returns: [{pid, meta}] or []
```

### View Event Store Stats

```elixir
id = Solo.EventStore.last_id()
IO.puts("Total events: #{id}")
```

### Count Events for Tenant

```elixir
Solo.EventStore.stream(tenant_id: "agent_1")
|> Enum.count()
```

---

## Performance Characteristics

| Operation | Time | Notes |
|-----------|------|-------|
| EventStore.emit() | <1ms | Async cast, doesn't block |
| Deploy service | 5-100ms | Depends on code size |
| Service startup | 100-1000ms | Depends on module |
| Capability verify | <1ms | ETS lookup |
| Vault store | 1-5ms | AES-256-GCM encryption |
| Vault retrieve | 1-5ms | AES-256-GCM decryption |
| Registry lookup | <0.1ms | O(1) Elixir Registry |

---

## Error Handling Patterns

### Deployment Errors

```elixir
case Solo.Deployment.Deployer.deploy(spec) do
  {:ok, pid} -> 
    IO.puts("Deployed: #{inspect(pid)}")
  {:error, reason} ->
    IO.puts("Failed: #{reason}")
end
```

### Capability Errors

```elixir
case Solo.Capability.Manager.verify(token, "resource", "read") do
  :ok ->
    IO.puts("Access granted")
  {:error, reason} ->
    IO.puts("Access denied: #{reason}")
end
```

### Vault Errors

```elixir
case Solo.Vault.retrieve("agent_1", "secret", key) do
  {:ok, value} ->
    IO.puts("Retrieved: #{value}")
  {:error, reason} ->
    IO.puts("Retrieval failed: #{reason}")
end
```

---

## Environment Variables

| Variable | Purpose | Example |
|----------|---------|---------|
| `SOLO_CONFIG` | Path to TOML config file | `/etc/solo/config.toml` |
| `SOLO_LOG_LEVEL` | Erlang logger level | `info`, `debug`, `warn` |
| `MIX_ENV` | Build environment | `dev`, `test`, `prod` |

---

## Important Constants

| Name | Value | Purpose |
|------|-------|---------|
| gRPC Port | 50051 | Default gRPC server port |
| HTTP Port | 8080 | Default REST API port |
| Capability Token Size | 32 bytes | Cryptographic strength |
| Event Sequence | Monotonic, no gaps | Audit trail ordering |
| AES Algorithm | AES-256-GCM | Vault encryption |
| IV Size | 12 bytes (96 bits) | GCM mode standard |
| Auth Tag Size | 16 bytes (128 bits) | GCM authentication |
| Cleanup Interval | 60 seconds | ServiceRegistry TTL cleanup |
| Capability Cleanup | 60 seconds | Expired capability cleanup |

---

## Key Design Decisions

1. **CubDB for Persistence**: Embedded KV store, no external dependencies
2. **GenServer for State**: Actor model for safe concurrency
3. **Event Sourcing**: All state changes logged immutably
4. **Capability-Based Security**: Tokens instead of identity
5. **Multi-Tenant Isolation**: Supervisor trees per tenant
6. **Dynamic Code Compilation**: Runtime compilation with namespacing
7. **mTLS Authentication**: Client cert extracts tenant_id
8. **AES-256-GCM**: Authenticated encryption for secrets
9. **Erlang Registry**: Lightweight service discovery
10. **Event Streaming**: EventStore for audit trail and replay

---

## Reading Order for New Developers

1. **Architecture Overview** - README.md (main design)
2. **Event Struct** - `lib/solo/event.ex` (fundamental)
3. **Application Startup** - `lib/solo/application.ex` (initialization)
4. **Supervisor Hierarchy** - `lib/solo/kernel.ex` + `system/supervisor.ex` (supervision)
5. **EventStore** - `lib/solo/event_store.ex` (persistence)
6. **Deployer** - `lib/solo/deployment/deployer.ex` (service lifecycle)
7. **Capability** - `lib/solo/capability/capability.ex` (security)
8. **Vault** - `lib/solo/vault.ex` (secrets)
9. **Gateway** - `lib/solo/gateway.ex` (API layer)

