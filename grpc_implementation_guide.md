# Solo gRPC Gateway Implementation Guide

## Overview

This document provides a comprehensive guide for implementing the gRPC gateway for the Solo project. The project has dependencies configured for gRPC and protobuf code generation, but the actual implementation is not yet complete.

## 1. Generating Elixir Code from Proto File

### Current Setup
- **Proto file location**: `/home/adavidoff/git/solo/priv/protos/solo/v1/solo.proto`
- **Dependencies installed**:
  - `grpc` (~> 0.9) - gRPC framework
  - `protobuf` (~> 0.13) - Protobuf support
  - `google_protos` (~> 0.1) - Google proto types
- **Protoc gen version**: 0.13.0 (configured in mix.exs)

### Generation Step

Using `protoc` with the elixir and grpc plugins:

```bash
cd /home/adavidoff/git/solo

# Generate both message and service code
protoc \
  --elixir_out=plugins=grpc:./lib \
  -I./priv/protos \
  priv/protos/solo/v1/solo.proto
```

This will generate:
- `lib/solo/v1/solo_kernel.pb.ex` - Message definitions (DeployRequest, StatusRequest, etc.)
- `lib/solo/v1/solo_kernel.grpc.pb.ex` - Service definition and RPC specifications

### Alternative: Using protobuf_generate

Add to mix.exs dependencies (dev only):
```elixir
{:protobuf_generate, "~> 0.1.1", only: [:dev, :test]}
```

Then configure in mix.exs:
```elixir
def project do
  [
    # ... other config
    protobuf_generate: [
      files: ["priv/protos/solo/v1/solo.proto"],
      output_dir: "lib",
      output: :package,
      include_docs: true
    ]
  ]
end
```

Then run:
```bash
mix protobuf.generate
```

## 2. Expected Generated Files Structure

Once generated, the files will look similar to:

```elixir
# lib/solo/v1/solo_kernel.pb.ex
defmodule Solo.V1.DeployRequest do
  use Protobuf, protoc_gen_elixir_version: "0.13.0", syntax: :proto3
  field :service_id, 1, type: :string
  field :code, 2, type: :string
  field :format, 3, type: Solo.V1.DeployRequest.DeployFormat, enum: true
end

# ... and service specification file
# lib/solo/v1/solo_kernel.grpc.pb.ex
defmodule Solo.V1.SoloKernel.Service do
  use GRPC.Service, name: "solo.v1.SoloKernel"
  
  rpc :Deploy, Solo.V1.DeployRequest, Solo.V1.DeployResponse
  rpc :Status, Solo.V1.StatusRequest, Solo.V1.StatusResponse
  rpc :Kill, Solo.V1.KillRequest, Solo.V1.KillResponse
  rpc :List, Solo.V1.ListRequest, Solo.V1.ListResponse
  rpc :Watch, Solo.V1.WatchRequest, stream(Solo.V1.Event)
  rpc :Shutdown, Solo.V1.ShutdownRequest, Solo.V1.ShutdownResponse
end
```

## 3. RPC Implementation Pattern (gRPC Stream API)

The gRPC library uses a unified **stream-based API** for all RPC types. All handler functions receive:
- A request/request stream (first parameter)
- A `GRPC.Server.Stream` materializer (second parameter)

### Unary RPC Handler Template

```elixir
def deploy(request, stream) do
  request
  |> GRPC.Stream.unary(materializer: stream)
  |> GRPC.Stream.map(fn %Solo.V1.DeployRequest{} = req ->
    # Call backend function
    case Solo.Deployment.Deployer.deploy(%{
      tenant_id: extract_tenant_from_context(stream),
      service_id: req.service_id,
      code: req.code,
      format: :elixir_source
    }) do
      {:ok, _pid} ->
        %Solo.V1.DeployResponse{
          service_id: req.service_id,
          status: "ok",
          error: ""
        }
      {:error, reason} ->
        %Solo.V1.DeployResponse{
          service_id: req.service_id,
          status: "error",
          error: to_string(reason)
        }
    end
  end)
  |> GRPC.Stream.run()
end
```

### Server-Side Streaming Handler Template (Watch RPC)

```elixir
def watch(request, stream) do
  # Create an event stream from the event store
  event_stream = create_event_stream(request.service_id, request.include_logs)
  
  event_stream
  |> GRPC.Stream.from()
  |> GRPC.Stream.map(fn event ->
    convert_event_to_proto(event)
  end)
  |> GRPC.Stream.run_with(stream)
end
```

### Error Handling

```elixir
def status(request, stream) do
  request
  |> GRPC.Stream.unary(materializer: stream)
  |> GRPC.Stream.map(fn %Solo.V1.StatusRequest{} = req ->
    case Solo.Deployment.Deployer.status(tenant_id, req.service_id) do
      status when is_map(status) ->
        %Solo.V1.StatusResponse{
          service_id: req.service_id,
          alive: status.alive,
          memory_bytes: extract_memory(status.info),
          message_queue_len: extract_message_queue_len(status.info),
          reductions: extract_reductions(status.info)
        }
      {:error, :not_found} ->
        {:error, GRPC.RPCError.exception(
          status: :not_found,
          message: "Service not found"
        )}
    end
  end)
  |> GRPC.Stream.run()
end
```

## 4. Key Backend Modules Available in Solo

### Solo.Deployment.Deployer
**Location**: `/home/adavidoff/git/solo/lib/solo/deployment/deployer.ex`

**Public API**:
- `deploy(spec)` - Deploy a service from source code
  - Spec keys: `:tenant_id`, `:service_id`, `:code`, `:format` (required)
  - Returns: `{:ok, pid} | {:error, reason}`
  
- `kill(tenant_id, service_id, opts)` - Kill a running service
  - Options: `:timeout` (ms), `:force` (bool)
  - Returns: `:ok | {:error, reason}`
  
- `status(tenant_id, service_id)` - Get service status
  - Returns: `map | {:error, :not_found}`
  - Map contains: `pid`, `service_id`, `tenant_id`, `alive`, `info` (process info)
  
- `list(tenant_id)` - List services for a tenant
  - Returns: `[{service_id, pid}]`

### Solo.EventStore
**Location**: `/home/adavidoff/git/solo/lib/solo/event_store.ex`

**Public API**:
- `emit(event_type, subject, payload, tenant_id, causation_id)` - Emit an event
  - Asynchronous (cast), returns `:ok` immediately
  - Event types: `:service_deployed`, `:service_deployment_failed`, `:service_killed`, etc.
  
- `stream(opts)` - Stream events with filters
  - Options: `:tenant_id`, `:service_id`, `:since_id`, `:limit`
  - Returns: `Enumerable.t()` of `Solo.Event` structs
  
- `filter(opts)` - Filter events by type, tenant, service
  - Returns: `[Solo.Event.t()]`
  
- `last_id()` - Get current sequence number

### Solo.Registry
**Location**: `/home/adavidoff/git/solo/lib/solo/registry.ex`

**Public API**:
- `register(tenant_id, service_id, pid)` - Register a service process
  - Returns: `{:ok, pid} | {:error, {:already_registered, pid}}`
  
- `lookup(tenant_id, service_id)` - Find a service by ID
  - Returns: `[{pid, metadata}]` or `[]`
  
- `list_for_tenant(tenant_id)` - Get all services for a tenant
  - Returns: `[{service_id, pid}]`
  
- `unregister(tenant_id, service_id)` - Unregister a service

### Solo.Capability.Manager
**Location**: `/home/adavidoff/git/solo/lib/solo/capability/manager.ex`

**Public API**:
- `grant(tenant_id, resource_ref, permissions, ttl_seconds)` - Grant capability token
  - Returns: `{:ok, token}`
  
- `verify(token, resource_ref, required_permission)` - Verify a capability
  - Returns: `:ok | {:error, reason}`
  
- `revoke(token_hash)` - Revoke a capability
  - Returns: `:ok`

### Solo.Vault
**Location**: `/home/adavidoff/git/solo/lib/solo/vault.ex`

**Key capabilities**: Encrypted secret storage for tenants

### Solo.Telemetry
**Location**: `/home/adavidoff/git/solo/lib/solo/telemetry.ex`

**Key capabilities**: Observability and metrics collection

## 5. Authentication & Tenant Identity

### mTLS-based Tenant Extraction

The gateway uses mTLS (mutual TLS) for authentication. The client certificate's CN (Common Name) or SAN (Subject Alternative Name) should contain the tenant_id.

Helper function to extract tenant ID from gRPC context:

```elixir
defp extract_tenant_from_context(stream) do
  # gRPC stream provides metadata via GRPC.Server.metadata_get/2
  case GRPC.Server.Stream.get_metadata(stream, :ssl_peer_cert) do
    nil ->
      # Fallback or error - should fail gracefully
      raise "Client certificate required for authentication"
    cert ->
      extract_tenant_from_cert(cert)
  end
end

defp extract_tenant_from_cert(cert_data) do
  # Parse DER/PEM certificate and extract CN or SAN
  # Implementation depends on x509 library integration
  # This is a placeholder
  "tenant_from_cert"
end
```

**Note**: The `x509` library is already a dependency (for mTLS support).

## 6. Complete Gateway Server Implementation Template

```elixir
defmodule Solo.Gateway.Server do
  @moduledoc """
  gRPC service handler for Solo kernel RPC methods.
  
  Maps gRPC requests to backend Solo functions:
  - Deploy/Kill/Status/List - managed by Solo.Deployment.Deployer
  - Watch - streams events from Solo.EventStore
  - Shutdown - graceful kernel shutdown
  """

  use GRPC.Server, service: Solo.V1.SoloKernel.Service

  require Logger

  alias GRPC.Stream
  alias Solo.V1.{
    DeployRequest, DeployResponse,
    StatusRequest, StatusResponse,
    KillRequest, KillResponse,
    ListRequest, ListResponse,
    WatchRequest, Event,
    ShutdownRequest, ShutdownResponse
  }

  # === RPC Handlers ===

  def deploy(request, stream) do
    request
    |> Stream.unary(materializer: stream)
    |> Stream.map(fn %DeployRequest{} = req ->
      tenant_id = extract_tenant(stream)
      
      case Solo.Deployment.Deployer.deploy(%{
        tenant_id: tenant_id,
        service_id: req.service_id,
        code: req.code,
        format: :elixir_source
      }) do
        {:ok, _pid} ->
          %DeployResponse{
            service_id: req.service_id,
            status: "ok"
          }
        {:error, reason} ->
          %DeployResponse{
            service_id: req.service_id,
            status: "error",
            error: to_string(reason)
          }
      end
    end)
    |> Stream.run()
  end

  def status(request, stream) do
    request
    |> Stream.unary(materializer: stream)
    |> Stream.map(fn %StatusRequest{} = req ->
      tenant_id = extract_tenant(stream)
      
      case Solo.Deployment.Deployer.status(tenant_id, req.service_id) do
        status when is_map(status) ->
          info = status[:info] || []
          memory = Keyword.get(info, :memory, 0)
          message_queue_len = Keyword.get(info, :message_queue_len, 0)
          reductions = Keyword.get(info, :reductions, 0)
          
          %StatusResponse{
            service_id: req.service_id,
            alive: status[:alive],
            memory_bytes: memory,
            message_queue_len: message_queue_len,
            reductions: reductions
          }
        {:error, :not_found} ->
          {:error, GRPC.RPCError.exception(
            status: :not_found,
            message: "Service #{req.service_id} not found"
          )}
      end
    end)
    |> Stream.run()
  end

  def kill(request, stream) do
    request
    |> Stream.unary(materializer: stream)
    |> Stream.map(fn %KillRequest{} = req ->
      tenant_id = extract_tenant(stream)
      timeout = if req.timeout_ms > 0, do: req.timeout_ms, else: 5000
      
      case Solo.Deployment.Deployer.kill(tenant_id, req.service_id, 
           timeout: timeout, force: req.force) do
        :ok ->
          %KillResponse{
            service_id: req.service_id,
            status: "ok"
          }
        {:error, reason} ->
          %KillResponse{
            service_id: req.service_id,
            status: "error",
            error: to_string(reason)
          }
      end
    end)
    |> Stream.run()
  end

  def list(request, stream) do
    tenant_id = extract_tenant(stream)
    services = Solo.Deployment.Deployer.list(tenant_id)
    
    service_infos = Enum.map(services, fn {service_id, pid} ->
      %Solo.V1.ServiceInfo{
        service_id: service_id,
        alive: Process.alive?(pid)
      }
    end)
    
    {:ok, %ListResponse{services: service_infos}}
  end

  def watch(request, stream) do
    tenant_id = extract_tenant(stream)
    
    # Stream events from the event store
    event_stream = Solo.EventStore.stream(
      tenant_id: tenant_id,
      service_id: if(request.service_id != "", do: request.service_id, else: nil),
      since_id: 0
    )
    
    event_stream
    |> Stream.from()
    |> Stream.map(fn event ->
      %Event{
        id: event.id,
        timestamp: event.timestamp,
        event_type: to_string(event.event_type),
        subject: event_subject_to_string(event.subject),
        payload: Jason.encode!(event.payload) |> String.to_charlist(),
        causation_id: event.causation_id || 0
      }
    end)
    |> Stream.run_with(stream)
  end

  def shutdown(request, stream) do
    grace_period = request.grace_period_ms || 30000
    
    request
    |> Stream.unary(materializer: stream)
    |> Stream.map(fn _req ->
      Logger.info("[Gateway] Shutdown requested with grace_period_ms=#{grace_period}")
      
      # Schedule shutdown
      Process.send_after(self(), :system_shutdown, grace_period)
      
      %ShutdownResponse{
        status: "ok",
        message: "Solo kernel shutting down"
      }
    end)
    |> Stream.run()
  end

  # === Private Helpers ===

  defp extract_tenant(stream) do
    # Extract tenant_id from mTLS certificate context
    # For now, use a placeholder - implement with x509 library
    case GRPC.Server.Stream.get_metadata(stream, :ce_client_cert) do
      nil -> "default_tenant"
      _cert -> extract_tenant_from_cert(_cert)
    end
  end

  defp extract_tenant_from_cert(_cert) do
    # TODO: Parse certificate and extract CN/SAN as tenant_id
    # Requires x509 library integration
    "tenant_from_cert"
  end

  defp event_subject_to_string({tenant_id, service_id}) do
    "#{tenant_id}:#{service_id}"
  end

  defp event_subject_to_string(subject) when is_binary(subject) do
    subject
  end

  defp event_subject_to_string(subject) do
    inspect(subject)
  end
end
```

## 7. Updating Gateway.ex

Replace the skeleton implementation:

```elixir
defmodule Solo.Gateway do
  @moduledoc """
  gRPC gateway for remote agent access.

  Provides:
  - mTLS authentication (verified client certificate = tenant_id)
  - Deploy, Kill, Status, List, Watch RPCs
  - Graceful shutdown

  The gateway runs on port 50051 by default.
  """

  use GenServer
  require Logger

  @port 50051

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl GenServer
  def init([]) do
    case start_server() do
      {:ok, server_pid} ->
        Logger.info("[Gateway] Started gRPC server on port #{@port}")
        {:ok, %{server_pid: server_pid}}
      
      {:error, reason} ->
        Logger.error("[Gateway] Failed to start: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  defp start_server do
    # Start gRPC server with mTLS support
    {:ok, _} = GRPC.Server.start_link(
      [{Solo.Gateway.Server, []}],
      port: @port,
      cred: create_server_credentials()
    )
    
    {:ok, self()}
  end

  defp create_server_credentials do
    # Load mTLS certificates - should be generated via mix task
    cert_path = "./data/certs/server.pem"
    key_path = "./data/certs/server.key"
    
    case File.read(cert_path) && File.read(key_path) do
      {:ok, cert, key} ->
        {:ok, cert_path, key_path}
      :error ->
        Logger.warning("[Gateway] Certificates not found at #{cert_path}")
        # In Phase 3, defer certificate generation
        {:error, :no_certificates}
    end
  end
end
```

## 8. Supervision Tree Integration

The Gateway is already registered in `Solo.System.Supervisor`:

```elixir
# lib/solo/system/supervisor.ex
children = [
  # ... other children ...
  Solo.Gateway  # GenServer for gRPC server
]
```

## 9. Testing & Development Workflow

### Generate Proto Files
```bash
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos priv/protos/solo/v1/solo.proto
```

### Start the Application
```bash
iex -S mix
```

### Test Deploy RPC (via grpcurl)
```bash
grpcurl -d '{"service_id":"test_service","code":"defmodule MyService do; def start_link(_), do: {:ok, self()}; end","format":1}' \
  localhost:50051 solo.v1.SoloKernel.Deploy
```

## 10. Key Implementation Considerations

1. **Tenant Isolation**: Always extract and validate tenant_id from mTLS certificate
2. **Error Propagation**: Use `GRPC.RPCError.exception()` for consistent error handling
3. **Event Streaming**: Use `Solo.EventStore.stream()` with proper filtering for Watch RPC
4. **Process Info**: Use Erlang's `Process.info()` for status metrics
5. **Graceful Shutdown**: Implement proper cleanup in the Shutdown RPC
6. **Async Events**: EventStore emits are asynchronous (cast), so they don't block RPC handlers

## 11. File Structure After Implementation

```
/home/adavidoff/git/solo/
├── lib/solo/
│   ├── gateway.ex                 # Updated GenServer
│   ├── gateway/
│   │   └── server.ex              # NEW: RPC handlers
│   ├── v1/                        # NEW: Generated proto code
│   │   ├── solo_kernel.pb.ex      # Generated message definitions
│   │   └── solo_kernel.grpc.pb.ex # Generated service spec
│   └── ...
└── priv/protos/
    └── solo/v1/
        └── solo.proto             # Source proto file
```

## Summary of Next Steps

1. ✅ Understand proto definition (COMPLETED)
2. ✅ Review available backend modules (COMPLETED)
3. Run `protoc` to generate Elixir code
4. Create `Solo.Gateway.Server` module with RPC handlers
5. Update `Solo.Gateway` GenServer to start gRPC server
6. Integrate mTLS certificate generation/loading
7. Test each RPC with grpcurl or gRPC client
8. Add integration tests
