# Concrete Implementation Examples for Solo

## Example 1: Capability Tokens (Pattern A)

```elixir
defmodule Solo.Capability do
  @moduledoc """
  Unforgeable capability tokens for fine-grained access control.
  
  Design: Uses random tokens stored in ETS with metadata.
  Once revoked, token is permanently invalid.
  """
  
  @type role :: :read_data | :write_data | :delete_data | :admin
  @type token :: binary()
  
  def start_link do
    :ets.new(:solo_capabilities, [:named_table, :set, {:read_concurrency, true}])
    {:ok, self()}
  end
  
  @doc "Create a new unforgeable capability token"
  def create(role, metadata \\ %{}) do
    token = :crypto.strong_rand_bytes(32)
    expires_at = DateTime.utc_now() |> DateTime.add(3600, :second)
    
    :ets.insert(:solo_capabilities, {
      token,
      role,
      metadata,
      expires_at,
      created_at: DateTime.utc_now()
    })
    
    {:ok, token}
  end
  
  @doc "Check if capability is valid (not revoked, not expired)"
  def check(token, required_role) do
    case :ets.lookup(:solo_capabilities, token) do
      [{^token, ^required_role, _meta, expires_at, _info}] ->
        if DateTime.compare(DateTime.utc_now(), expires_at) == :lt do
          :ok
        else
          {:error, :expired}
        end
      [{^token, other_role, _meta, _expires, _info}] ->
        {:error, {:wrong_role, other_role, required_role}}
      [] ->
        {:error, :revoked}
    end
  end
  
  @doc "Revoke a capability (one-way, irreversible)"
  def revoke(token) do
    :ets.delete(:solo_capabilities, token)
    :ok
  end
  
  @doc "List all active capabilities (for auditing)"
  def list_active do
    now = DateTime.utc_now()
    :ets.match_object(:solo_capabilities, {:'_', :'_', :'_', :'_', :'_'})
    |> Enum.filter(fn {_token, _role, _meta, expires_at, _info} ->
      DateTime.compare(now, expires_at) == :lt
    end)
  end
end

# Usage in application code
defmodule Solo.UserService do
  alias Solo.Capability
  
  def get_user(user_id, cap_token) do
    with :ok <- Capability.check(cap_token, :read_data) do
      # Perform the read
      {:ok, fetch_from_db(user_id)}
    else
      error -> error
    end
  end
  
  def delete_user(user_id, cap_token) do
    with :ok <- Capability.check(cap_token, :delete_data) do
      # Perform the deletion (after logging for audit)
      Logger.warn("User deletion", user_id: user_id, cap_token: inspect(cap_token))
      {:ok, delete_from_db(user_id)}
    else
      error -> error
    end
  end
end
```

---

## Example 2: Attenuated Process Wrappers (Pattern B)

```elixir
defmodule Solo.AttenuatedProcess do
  @moduledoc """
  Wraps a GenServer process to restrict which messages it accepts.
  
  Design: Acts as a proxy that validates messages before forwarding.
  Denies any message not in the allowlist.
  """
  
  @type restricted :: {:restricted, pid(), [atom()]}
  
  @doc "Wrap a pid with message restrictions"
  def wrap(pid, allowed_messages) do
    {:restricted, pid, allowed_messages}
  end
  
  @doc "Send a message to a restricted process"
  def call({:restricted, pid, allowed}, message, timeout \\ 5000) do
    case extract_operation(message) do
      {:ok, op} when op in allowed ->
        GenServer.call(pid, message, timeout)
      {:ok, op} ->
        {:error, {:forbidden_operation, op}}
      :error ->
        {:error, :invalid_message}
    end
  end
  
  @doc "Send a cast (fire-and-forget) to a restricted process"
  def cast({:restricted, pid, allowed}, message) do
    case extract_operation(message) do
      {:ok, op} when op in allowed ->
        GenServer.cast(pid, message)
      {:ok, op} ->
        {:error, {:forbidden_operation, op}}
      :error ->
        {:error, :invalid_message}
    end
  end
  
  # Helper: extract the operation from a message
  defp extract_operation({op, _args}), do: {:ok, op}
  defp extract_operation({op, _arg1, _arg2}), do: {:ok, op}
  defp extract_operation(op) when is_atom(op), do: {:ok, op}
  defp extract_operation(_), do: :error
end

# Usage in database access
defmodule Solo.Database do
  use GenServer
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end
  
  def handle_call({:get, key}, _from, state) do
    {:reply, {:ok, Map.get(state, key)}, state}
  end
  
  def handle_call({:put, key, value}, _from, state) do
    {:reply, :ok, Map.put(state, key, value)}
  end
  
  def handle_call({:delete, key}, _from, state) do
    {:reply, :ok, Map.delete(state, key)}
  end
  
  def handle_call(:health, _from, state) do
    {:reply, :ok, state}
  end
  
  init(opts) do
    {:ok, opts}
  end
end

# Create different capability levels
defmodule Solo.Application do
  def start(_type, _args) do
    {:ok, db_pid} = Solo.Database.start_link(%{})
    
    # Full access
    full_db = db_pid
    
    # Read-only access
    read_only_db = Solo.AttenuatedProcess.wrap(db_pid, [:get, :health])
    
    # Health-check only
    health_db = Solo.AttenuatedProcess.wrap(db_pid, [:health])
    
    # Store in application for different services to use
    Application.put_env(:solo, :db_full, full_db)
    Application.put_env(:solo, :db_read, read_only_db)
    Application.put_env(:solo, :db_health, health_db)
    
    # Supervisor setup...
  end
end
```

---

## Example 3: Time-Limited Delegated Capabilities (Pattern C)

```elixir
defmodule Solo.DelegatedCapability do
  @moduledoc """
  Time-limited capability delegation (e.g., API keys, session tokens).
  
  Design: Creates a delegation record with expiration.
  Useful for: temporary access grants, session tokens, API keys.
  """
  
  @doc "Delegate a capability to another process/user with expiration"
  def delegate(original_cap, delegated_to, valid_for_seconds) do
    token = :crypto.strong_rand_bytes(32) |> Base.encode64()
    expires_at = DateTime.utc_now() |> DateTime.add(valid_for_seconds, :second)
    
    :ets.insert(:solo_delegations, {
      token,
      original_cap,
      delegated_to,
      expires_at,
      DateTime.utc_now()
    })
    
    {:ok, token}
  end
  
  @doc "Check a delegated capability (returns original if valid)"
  def check_delegated(delegation_token) do
    case :ets.lookup(:solo_delegations, delegation_token) do
      [{^delegation_token, cap, _to, expires_at, _created}] ->
        if DateTime.compare(DateTime.utc_now(), expires_at) == :lt do
          {:ok, cap}
        else
          {:error, :expired}
        end
      [] ->
        {:error, :not_found}
    end
  end
  
  @doc "Revoke a delegation (cannot be re-enabled)"
  def revoke_delegation(token) do
    :ets.delete(:solo_delegations, token)
    :ok
  end
end

# Real-world example: API key system
defmodule Solo.APIKey do
  @moduledoc "Wraps capabilities in API keys with expiration"
  
  def create_api_key(client_id, permissions, valid_for_hours) do
    # Create underlying capability
    {:ok, cap} = Solo.Capability.create(permissions)
    
    # Wrap in delegation
    {:ok, token} = Solo.DelegatedCapability.delegate(
      cap,
      client_id,
      valid_for_hours * 3600
    )
    
    # Return as user-friendly key
    %{
      api_key: token,
      client_id: client_id,
      permissions: permissions,
      expires_at: DateTime.utc_now() |> DateTime.add(valid_for_hours * 3600, :second)
    }
  end
  
  def validate_api_key(api_key) do
    with {:ok, cap} <- Solo.DelegatedCapability.check_delegated(api_key) do
      {:ok, cap}
    end
  end
  
  def revoke_api_key(api_key) do
    Solo.DelegatedCapability.revoke_delegation(api_key)
  end
end

# Usage
defmodule Solo.HTTPHandler do
  def handle_request(conn) do
    api_key = Plug.Conn.get_req_header(conn, "authorization") |> List.first()
    
    with {:ok, cap} <- Solo.APIKey.validate_api_key(api_key) do
      # Process request with capability
      process_authenticated(conn, cap)
    else
      {:error, :expired} ->
        Plug.Conn.send_resp(conn, 401, "API key expired")
      {:error, :not_found} ->
        Plug.Conn.send_resp(conn, 401, "Invalid API key")
    end
  end
end
```

---

## Example 4: Systemd Service Unit

```ini
# /etc/systemd/system/solo.service

[Unit]
Description=Solo Elixir Application
After=network-online.target
Wants=network-online.target

[Service]
# User and group
User=solo
Group=solo
WorkingDirectory=/opt/solo

# Restart policy
Type=notify
Restart=always
RestartSec=10

# Main process
ExecStart=/opt/solo/bin/solo start
ExecStop=/bin/kill -TERM $MAINPID

# Standard streams
StandardOutput=journal
StandardError=journal

# === SECURITY OPTIONS ===

# Drop all capabilities except what we need
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
AmbientCapabilities=CAP_NET_BIND_SERVICE

# Prevent process from gaining new privileges
NoNewPrivileges=yes

# Filesystem isolation
PrivateTmp=yes
PrivateDevices=yes
ProtectSystem=strict
ProtectHome=yes
ReadWritePaths=/var/lib/solo /var/log/solo

# Network restrictions
RestrictAddressFamilies=AF_INET AF_INET6
RestrictNamespaces=yes
RestrictSUIDSGID=yes

# Syscall restrictions (allow only common ones)
SystemCallFilter=@system-service ~@privileged ~@resources
SystemCallErrorNumber=EPERM
SystemCallArchitectures=native

# Personality lock
LockPersonality=yes

# Resource limits
MemoryLimit=512M
CPUQuota=50%
TasksMax=256

# Device access
PrivateDevices=yes
DevicePolicy=closed

# Keyring isolation
PrivateKeyring=yes

# Protect process
ProtectClock=yes
ProtectHostname=yes
ProtectKernelLogs=yes
ProtectKernelModules=yes
ProtectKernelTunables=yes
ProtectControlGroups=yes

[Install]
WantedBy=multi-user.target
```

**Enable and start**:
```bash
sudo systemctl daemon-reload
sudo systemctl enable solo.service
sudo systemctl start solo.service

# Check status
sudo systemctl status solo.service
sudo journalctl -u solo.service -f
```

---

## Example 5: Erlang Distribution with TLS

```elixir
# rel/env.erl - Configure TLS for inter-node communication

{kernel, [
  % Network interface (bind to localhost for cluster-local)
  {inet_dist_use_interface, {127, 0, 0, 1}},
  
  % Port range for inter-node communication
  {inet_dist_listen_min, 9001},
  {inet_dist_listen_max, 9010},
  
  % TLS configuration
  {ssl_dist_opt, [
    % Certificate and key
    {certfile, "/etc/solo/certs/node.pem"},
    {keyfile, "/etc/solo/certs/node-key.pem"},
    
    % Verify peer certificates
    {verify, verify_peer},
    {cacertfile, "/etc/solo/certs/ca.pem"},
    
    % Require valid certificate
    {fail_if_no_peer_cert, true},
    
    % Strong TLS options
    {secure_renegotiate, true},
    {ciphers, [
      {rsa, aes_256_cbc, sha256},
      {rsa, aes_128_cbc, sha256}
    ]},
    {versions, ['tlsv1.3', 'tlsv1.2']},
    
    % Disable compression (CRIME attack)
    {compression, false}
  ]},
  
  % Require TLS (not optional)
  {net_kernel, [
    {net_ticktime, 60},
    {dist_nodedown_reason, true}
  ]}
]}.
```

**Generate certificates** (self-signed for testing):
```bash
# CA certificate
openssl genrsa -out ca-key.pem 2048
openssl req -new -x509 -days 3650 -key ca-key.pem -out ca.pem

# Node certificate
openssl genrsa -out node-key.pem 2048
openssl req -new -key node-key.pem -out node.csr
openssl x509 -req -days 365 -in node.csr \
  -CA ca.pem -CAkey ca-key.pem -CAcreateserial \
  -out node.pem

# Combine into single file
cat node.pem node-key.pem > /etc/solo/certs/node.pem
```

---

## Example 6: Application-Level RPC Authorization

```elixir
defmodule Solo.DistributedAuth do
  @moduledoc """
  Application-level authorization for distributed RPC calls.
  
  Wraps :rpc.call to check permissions before forwarding.
  """
  
  @doc "Call a function on a remote node with permission check"
  def rpc_call(node, module, function, args, requester_pid, required_permission) do
    with :ok <- authorize_rpc(requester_pid, node, required_permission) do
      log_rpc_call(requester_pid, node, module, function)
      :rpc.call(node, module, function, args)
    else
      {:error, reason} ->
        Logger.warn("RPC denied",
          requester: inspect(requester_pid),
          target_node: node,
          reason: reason
        )
        {:error, :unauthorized}
    end
  end
  
  defp authorize_rpc(requester, target_node, permission) do
    # Check 1: Is requester a trusted entity?
    case check_node_trust(target_node) do
      :ok -> :ok
      error -> error
    end
    
    # Check 2: Does requester have the permission?
    case Solo.Capability.check(requester, permission) do
      :ok -> :ok
      error -> error
    end
    
    # Check 3: Are we allowed to communicate with this node?
    case check_node_whitelist(target_node) do
      :ok -> :ok
      error -> error
    end
  end
  
  defp check_node_trust(node) do
    trusted = Application.get_env(:solo, :trusted_nodes, [])
    if node in trusted, do: :ok, else: {:error, :untrusted_node}
  end
  
  defp check_node_whitelist(node) do
    # Could check firewall rules, node health, etc.
    :ok
  end
  
  defp log_rpc_call(requester, node, module, function) do
    Logger.info("RPC call",
      requester: inspect(requester),
      target_node: node,
      module: module,
      function: function,
      timestamp: DateTime.utc_now()
    )
  end
end

# Usage in distributed application
defmodule Solo.WorkerPool do
  def submit_job(job_data, remote_node) do
    # Get the capability to submit jobs
    cap = Application.get_env(:solo, :job_cap)
    
    # Use authorized RPC
    Solo.DistributedAuth.rpc_call(
      remote_node,
      Solo.Worker,
      :execute,
      [job_data],
      self(),
      cap
    )
  end
end
```

---

## Integration Example: Complete HTTP Handler

```elixir
defmodule Solo.HTTPHandler do
  @moduledoc "Complete HTTP handler with capability-based security"
  
  require Logger
  
  def init(conn) do
    # Extract API key from authorization header
    case get_authorization(conn) do
      {:ok, api_key} ->
        conn
        |> assign_cap(api_key)
        |> assign_user(api_key)
      :error ->
        # No auth provided
        conn
        |> Plug.Conn.send_resp(401, "Unauthorized")
        |> Plug.Conn.halt()
    end
  end
  
  def handle_request(conn, action) do
    cap = conn.assigns[:cap]
    
    try do
      # All operations go through capability checks
      case action do
        :get_user ->
          with :ok <- Solo.Capability.check(cap, :read_data) do
            get_user(conn)
          else
            {:error, reason} ->
              error_response(conn, 403, "Forbidden", reason)
          end
        
        :create_user ->
          with :ok <- Solo.Capability.check(cap, :write_data) do
            create_user(conn)
          else
            {:error, reason} ->
              error_response(conn, 403, "Forbidden", reason)
          end
        
        :delete_user ->
          with :ok <- Solo.Capability.check(cap, :delete_data) do
            # Extra audit logging for destructive ops
            Logger.warn("Destructive operation authorized",
              user: conn.assigns[:user_id],
              action: :delete_user
            )
            delete_user(conn)
          else
            {:error, reason} ->
              error_response(conn, 403, "Forbidden", reason)
          end
      end
    rescue
      e in [RuntimeError] ->
        Logger.error("Handler error", exception: e, action: action)
        error_response(conn, 500, "Internal Error", e.message)
    end
  end
  
  defp get_authorization(conn) do
    case Plug.Conn.get_req_header(conn, "authorization") do
      [bearer_token] ->
        case String.split(bearer_token, " ") do
          ["Bearer", token] -> {:ok, token}
          _ -> :error
        end
      _ -> :error
    end
  end
  
  defp assign_cap(conn, api_key) do
    case Solo.APIKey.validate_api_key(api_key) do
      {:ok, cap} ->
        Plug.Conn.assign(conn, :cap, cap)
      {:error, _reason} ->
        conn
        |> Plug.Conn.send_resp(401, "Invalid token")
        |> Plug.Conn.halt()
    end
  end
  
  defp assign_user(conn, api_key) do
    # Look up which user owns this API key
    user_id = lookup_api_key_user(api_key)
    Plug.Conn.assign(conn, :user_id, user_id)
  end
  
  defp error_response(conn, status, reason, detail) do
    Plug.Conn.send_resp(conn, status, Jason.encode!(%{
      error: reason,
      detail: detail
    }))
  end
  
  # ... actual handler functions ...
end
```

---

## Testing Capabilities

```elixir
defmodule Solo.CapabilityTest do
  use ExUnit.Case
  
  setup do
    {:ok, _} = Solo.Capability.start_link()
    :ok
  end
  
  test "capability token can be created and checked" do
    {:ok, token} = Solo.Capability.create(:read_data)
    assert :ok == Solo.Capability.check(token, :read_data)
  end
  
  test "revoked capability is rejected" do
    {:ok, token} = Solo.Capability.create(:read_data)
    :ok = Solo.Capability.revoke(token)
    assert {:error, :revoked} == Solo.Capability.check(token, :read_data)
  end
  
  test "expired capability is rejected" do
    {:ok, token} = Solo.Capability.create(:read_data)
    # Manually expire it (in real test, use travel time)
    :ets.update_element(:solo_capabilities, token, {4, DateTime.utc_now()})
    assert {:error, :expired} == Solo.Capability.check(token, :read_data)
  end
  
  test "wrong role is rejected" do
    {:ok, token} = Solo.Capability.create(:read_data)
    assert {:error, {:wrong_role, :read_data, :write_data}} ==
      Solo.Capability.check(token, :write_data)
  end
  
  test "attenuated process restricts operations" do
    {:ok, db_pid} = Solo.Database.start_link(%{})
    read_only = Solo.AttenuatedProcess.wrap(db_pid, [:get, :health])
    
    # Allowed operation succeeds
    {:ok, _} = Solo.AttenuatedProcess.call(read_only, {:get, "key"})
    
    # Disallowed operation fails
    assert {:error, {:forbidden_operation, :put}} ==
      Solo.AttenuatedProcess.call(read_only, {:put, "key", "value"})
  end
end
```

---

## Deployment Checklist

```bash
#!/bin/bash
# deploy-solo.sh - Complete deployment with security

set -e

echo "=== Solo Deployment ==="

# 1. Create user and group
sudo useradd -r -s /bin/false solo || true
sudo groupadd -r solo || true

# 2. Create directories
sudo mkdir -p /opt/solo /var/lib/solo /var/log/solo /etc/solo/certs
sudo chown -R solo:solo /opt/solo /var/lib/solo /var/log/solo /etc/solo/certs
sudo chmod 700 /var/lib/solo /etc/solo/certs

# 3. Set POSIX capabilities
sudo setcap -r /opt/solo/bin/erl || true  # Drop all
sudo setcap cap_net_bind_service=ep /opt/solo/bin/erl

# 4. Generate TLS certificates
if [ ! -f /etc/solo/certs/node.pem ]; then
  echo "Generating TLS certificates..."
  # (See certificate generation example above)
fi

# 5. Install systemd service unit
sudo cp solo.service /etc/systemd/system/
sudo systemctl daemon-reload

# 6. Enable and start
sudo systemctl enable solo.service
sudo systemctl restart solo.service

# 7. Verify
echo "Checking service status..."
sudo systemctl status solo.service

echo "=== Deployment Complete ==="
echo "View logs: sudo journalctl -u solo.service -f"
```

