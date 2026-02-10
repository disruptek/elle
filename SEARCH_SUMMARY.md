# Solo gRPC Gateway Implementation - Search Summary

## What I Found

### 1. Proto File Analysis
- **Location**: `/home/adavidoff/git/solo/priv/protos/solo/v1/solo.proto`
- **Service**: `SoloKernel` with 6 RPC methods (Deploy, Status, Kill, List, Watch, Shutdown)
- **Message Types**: DeployRequest, StatusRequest, KillRequest, ListRequest, WatchRequest, ShutdownRequest + corresponding Response types
- **Streaming**: Watch RPC returns `stream Event` for real-time event monitoring

### 2. Dependencies Configured
✅ **Already installed and ready to use**:
- `grpc ~> 0.9` - Full gRPC implementation with stream-based API
- `protobuf ~> 0.13` - Protocol Buffer support
- `google_protos ~> 0.1` - Google proto types
- `x509 ~> 0.8` - mTLS certificate support

### 3. Protobuf Code Generation
**Status**: NOT YET GENERATED (no `.pb.ex` files in `/home/adavidoff/git/solo/lib/solo/`)

**How to generate**:
```bash
cd /home/adavidoff/git/solo
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos priv/protos/solo/v1/solo.proto
```

This will create:
- `lib/solo/v1/solo_kernel.pb.ex` - Message definitions
- `lib/solo/v1/solo_kernel.grpc.pb.ex` - Service specification

### 4. Key Backend Modules Available

#### Solo.Deployment.Deployer
- `deploy(spec)` → `{:ok, pid} | {:error, reason}`
  - Spec: `{:tenant_id, :service_id, :code, :format}`
  - Compiles Elixir source, starts service under tenant supervisor
- `status(tenant_id, service_id)` → map with pid, alive, info (memory, queue_len, reductions)
- `kill(tenant_id, service_id, opts)` → `:ok | {:error, reason}`
  - Options: `:timeout` (ms), `:force` (bool)
- `list(tenant_id)` → `[{service_id, pid}]`

#### Solo.EventStore
- **Append-only log** backed by CubDB
- `emit(event_type, subject, payload)` - Async event recording
- `stream(opts)` - Stream events with filters (tenant_id, service_id, since_id)
- Event types: `:service_deployed`, `:service_killed`, `:service_crashed`, etc.
- **Perfect for Watch RPC** - stream events to clients in real-time

#### Solo.Registry
- Service discovery using Elixir's Registry
- `register/unregister` by `{tenant_id, service_id}` key
- `lookup()`, `list_for_tenant()`

#### Solo.Capability.Manager
- Token-based capability system (for Phase 4)
- `grant()`, `verify()`, `revoke()`
- Uses ETS for fast lookup

#### Solo.Vault
- Encrypted secret storage per tenant (Phase 7)

#### Solo.Telemetry
- Observability and metrics collection (Phase 7)

### 5. Supervision Tree

```
Solo.Application
  └─ Solo.Kernel (supervisor)
      ├─ Solo.System.Supervisor
      │  ├─ Solo.EventStore
      │  ├─ Solo.AtomMonitor
      │  ├─ Solo.Registry
      │  ├─ Solo.Deployment.Deployer
      │  ├─ Solo.Capability.Manager
      │  ├─ Solo.Backpressure.LoadShedder
      │  ├─ Solo.Vault
      │  ├─ Solo.Telemetry
      │  └─ Solo.Gateway ← gRPC GenServer
      └─ Solo.Tenant.Supervisor (dynamic)
         └─ Solo.Tenant.ServiceSupervisor (per tenant)
            └─ User services (dynamically started)
```

**The Gateway is already integrated!** It just needs implementation.

### 6. Current Gateway Status

**Location**: `/home/adavidoff/git/solo/lib/solo/gateway.ex`

**Current state**: Skeleton GenServer that logs "Ready (gRPC server to be bound to port 50051)" but doesn't actually start the server.

**What needs to be done**:
1. Create `lib/solo/gateway/server.ex` with RPC handler functions
2. Update `lib/solo/gateway.ex` to actually start GRPC.Server
3. Integrate mTLS certificate loading

### 7. gRPC Implementation Pattern

The `grpc` library uses a **unified stream-based API** for ALL RPC types:

```elixir
defmodule Solo.Gateway.Server do
  use GRPC.Server, service: Solo.V1.SoloKernel.Service
  
  # Unary RPC (Deploy, Status, Kill, Shutdown)
  def deploy(request, stream) do
    request
    |> GRPC.Stream.unary(materializer: stream)
    |> GRPC.Stream.map(fn req -> process_deploy(req) end)
    |> GRPC.Stream.run()
  end
  
  # Server-side streaming (Watch)
  def watch(request, stream) do
    event_stream = Solo.EventStore.stream(tenant_id: extract_tenant(stream))
    
    event_stream
    |> GRPC.Stream.from()
    |> GRPC.Stream.map(fn event -> convert_to_proto(event) end)
    |> GRPC.Stream.run_with(stream)
  end
end
```

Key points:
- All handlers use `GRPC.Stream` pipeline operators
- Error handling via `GRPC.RPCError.exception()`
- Watch RPC streams from EventStore for real-time events
- Tenant ID extracted from mTLS certificate context

### 8. Authentication (mTLS)

**Mechanism**: Client certificate's CN/SAN field contains tenant_id

**Current gap**: Extraction logic not implemented (needs x509 library parsing)

**What to do**:
```elixir
defp extract_tenant(stream) do
  case GRPC.Server.Stream.get_metadata(stream, :ssl_peer_cert) do
    nil -> raise "Client certificate required"
    cert -> extract_from_cert(cert)  # TODO: parse cert with x509
  end
end
```

### 9. No Existing gRPC Examples in Project

- No existing RPC handlers or GRPC.Server usage found
- This is a greenfield implementation
- Good news: All backend modules (Deployer, EventStore, Registry, etc.) are well-designed and tested
- Just need to wire them up to gRPC

## Files That Will Be Modified/Created

### Must Create:
- `lib/solo/gateway/server.ex` - RPC handlers (6 functions for 6 RPCs)
- `lib/solo/v1/solo_kernel.pb.ex` - Generated message definitions
- `lib/solo/v1/solo_kernel.grpc.pb.ex` - Generated service spec

### Must Modify:
- `lib/solo/gateway.ex` - Implement actual gRPC.Server startup

### Optional (for completeness):
- `lib/solo/gateway/auth.ex` - mTLS certificate parsing helpers
- `lib/solo/gateway/converters.ex` - Proto ↔ internal data structure conversions

## Implementation Roadmap

```
Phase 1: Code Generation
  └─ Run protoc to generate .pb.ex files

Phase 2: RPC Handler Implementation
  ├─ Create gateway/server.ex with 6 RPC handlers
  ├─ Wire each RPC to corresponding backend function
  ├─ Implement error handling
  └─ Test each RPC individually

Phase 3: Authentication
  ├─ Implement tenant extraction from mTLS cert
  ├─ Add certificate validation
  └─ Test multi-tenant isolation

Phase 4: Event Streaming
  ├─ Complete Watch RPC implementation
  ├─ Test real-time event delivery
  └─ Verify event filtering by service_id

Phase 5: Integration & Testing
  ├─ Update gateway.ex to start gRPC server
  ├─ Integration tests with grpcurl
  └─ Load tests and validation
```

## Key Insights

1. **Well-Designed Backend**: The Solo codebase has excellent module organization with clear concerns (Deployer, Registry, EventStore, Capability, Vault)

2. **Multi-Tenancy Ready**: Tenant isolation is built in via:
   - Per-tenant supervisors in `Solo.Tenant.Supervisor`
   - mTLS-based tenant identity
   - Tenant-scoped service registration

3. **Event-Driven Architecture**: EventStore is a first-class citizen - all significant events are logged and can be streamed to clients

4. **Supervision Critical**: The supervisor tree dependency order matters (System.Supervisor uses `:rest_for_one` strategy)

5. **Stream-Based Everything**: The gRPC library's stream API is composable and works for unary, server-streaming, and bidirectional RPCs

6. **No Magic Needed**: Just straightforward mapping between proto messages and Elixir functions

## Quick Reference: RPC → Backend Function Mapping

| RPC | Backend Function | Handler Type | Complexity |
|-----|------------------|--------------|-----------|
| Deploy | `Deployer.deploy(spec)` | Unary | Medium (compile + start) |
| Status | `Deployer.status(tenant, service)` | Unary | Low (query) |
| Kill | `Deployer.kill(tenant, service, opts)` | Unary | Low (command) |
| List | `Deployer.list(tenant)` | Unary | Low (query) |
| Watch | `EventStore.stream(filters)` | Server-streaming | Medium (event filtering) |
| Shutdown | System.shutdown() | Unary | Medium (graceful shutdown) |

## Available Documentation

- **gRPC README**: `/home/adavidoff/git/solo/deps/grpc/README.md` - Complete examples
- **Backend source**: All backend modules have excellent inline documentation
- **Proto file**: Clear message definitions with field documentation

## Recommended Implementation Order

1. **Start with Status & List** - Simple queries, no side effects
2. **Then Deploy & Kill** - Basic commands with error handling
3. **Then Shutdown** - Single command
4. **Finally Watch** - Most complex due to streaming

This order lets you validate the basic gRPC integration before tackling streaming.

---

**Complete detailed implementation guide available at**:
`/var/run/user/1000/grpc_implementation_guide.md`
