# Solo gRPC Implementation - Ready-to-Use Code Examples

This document contains implementation patterns and commands for the gRPC gateway.

## 1. Generate Proto Files (One-time command)

Run this to generate Elixir code from the solo.proto file:

```bash
cd /home/adavidoff/git/solo
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos priv/protos/solo/v1/solo.proto
```

Generated files:
- lib/solo/v1/solo_kernel.pb.ex - Message definitions
- lib/solo/v1/solo_kernel.grpc.pb.ex - Service specification

## 2. Create lib/solo/gateway/server.ex

Create this file with the following structure:

- Module declaration with: use GRPC.Server, service: Solo.V1.SoloKernel.Service
- Six RPC handler functions: deploy/2, status/2, kill/2, list/2, watch/2, shutdown/2
- Each handler processes requests and calls backend functions
- Error handling with GRPC.RPCError.exception
- Tenant ID extraction from gRPC context
- Event stream conversion for Watch RPC

Key patterns:

```
def deploy(request, stream) do
  request
  |> Stream.unary(materializer: stream)
  |> Stream.map(fn req -> process_and_return_response end)
  |> Stream.run()
end

def watch(request, stream) do
  event_stream = Solo.EventStore.stream(...)
  event_stream
  |> Stream.from()
  |> Stream.map(fn event -> convert_event end)
  |> Stream.run_with(stream)
end
```

See grpc_implementation_guide.md Section 6 for full implementation.

## 3. Update lib/solo/gateway.ex

Changes needed:

1. Implement init/1 callback to call start_grpc_server()
2. Call GRPC.Server.start_link with the server handler
3. Load mTLS credentials in production
4. Handle server startup errors
5. Return {:ok, state} with server_pid on success

Key function:

```
defp start_grpc_server do
  GRPC.Server.start_link(
    [{Solo.Gateway.Server, []}],
    port: 50051,
    cred: get_credentials()
  )
end
```

See grpc_implementation_guide.md Section 7 for full implementation.

## 4. Quick Start Commands

After implementing the files:

Generate proto files:
```bash
cd /home/adavidoff/git/solo
protoc --elixir_out=plugins=grpc:./lib -I./priv/protos priv/protos/solo/v1/solo.proto
```

Start the application:
```bash
cd /home/adavidoff/git/solo
iex -S mix
```

Test with grpcurl (install grpcurl first if needed):
```bash
grpcurl -plaintext -d '{"service_id":"my_service"}' localhost:50051 solo.v1.SoloKernel.Status
```

## 5. Testing Checklist

After implementation:

- [ ] Deploy: Deploy a service, verify it starts
- [ ] Status: Check service status, verify response
- [ ] List: List services, verify count
- [ ] Kill: Kill a service, verify it's gone
- [ ] Watch: Stream events, verify they appear
- [ ] Shutdown: Request shutdown, verify graceful shutdown

## 6. Implementation Notes

Tenant ID Extraction:
- Extract from gRPC stream context using X509 library
- For dev: use placeholder "default_tenant"
- For prod: parse client certificate CN/SAN field

Error Handling:
- Always return GRPC.RPCError.exception for errors
- Use appropriate gRPC status codes (:not_found, :invalid_argument, etc)

Event Timestamps:
- Convert System.monotonic_time() to milliseconds
- Use System.convert_time_unit(event.timestamp, :native, :millisecond)

Payload Serialization:
- Events have map payloads
- Proto expects bytes/charlist
- Use Jason.encode! + String.to_charlist()

## 7. Backend API Summary

Solo.Deployment.Deployer:
- deploy(spec) -> {ok, pid} or error
- status(tenant_id, service_id) -> status_map or error
- kill(tenant_id, service_id, opts) -> ok or error
- list(tenant_id) -> [{service_id, pid}]

Solo.EventStore:
- emit(event_type, subject, payload) -> ok (async)
- stream(tenant_id, service_id, since_id) -> Enumerable
- filter(event_type, tenant_id) -> [events]

## 8. GRPC.Stream Pipeline

For all RPC handlers:

Unary: request |> Stream.unary() |> Stream.map() |> Stream.run()

Server-streaming: Stream.from(enum) |> Stream.map() |> Stream.run_with(stream)

Error handling: Use Stream.map to return either response or GRPC.RPCError.exception

## 9. Dependencies in mix.exs

Verify these are present:
- grpc ~> 0.9
- protobuf ~> 0.13
- google_protos ~> 0.1
- x509 ~> 0.8
- jason ~> 1.4

Run: mix deps.get

## 10. Directory Structure

After implementation:

lib/solo/
├── gateway.ex                  (updated)
├── gateway/
│   └── server.ex               (new)
├── v1/                         (generated)
│   ├── solo_kernel.pb.ex       
│   └── solo_kernel.grpc.pb.ex  
└── (other modules)

---

For complete code implementations with all error handling and logging:
See grpc_implementation_guide.md Sections 6-7

For detailed implementation walkthrough:
See grpc_implementation_guide.md
