# Quick Reference: Security Patterns for Solo

## Pattern Rankings by Solo Fit

### ⭐⭐⭐⭐⭐ ESSENTIAL: Object Capability Model (OCap)
**Why**: Maps perfectly to Elixir's actor model
- Use process IDs (pids) as unforgeable capabilities
- Restrict process message protocols with wrappers
- No global registries for sensitive resources
- Implement attenuation patterns for fine-grained access

**Elixir Implementation**:
```elixir
# Bad (global ambient authority)
Database.delete(:user, user_id)

# Good (capability-based)
{:ok, pid} = Database.start_link()
Database.delete(pid, :user, user_id)  # Caller must have pid

# Even better (attenuated)
read_only_db = Database.wrap(pid, [:get, :list])  # Only allows reads
```

---

### ⭐⭐⭐⭐ HIGH PRIORITY: Pledge/Unveil (OpenBSD) or Seccomp (Linux)
**Why**: Protects against code compromises at OS level
- Declare allowed syscalls at startup
- Default-deny instead of default-allow
- Irrevocable restrictions (one-way ratchet)

**Deployment**:
```bash
# On OpenBSD
pledge -p 'stdio rpath wpath inet dns' erl -name solo@host

# On Linux (via systemd or directly with seccomp)
SystemCallFilter=@system-service ~@privileged
```

---

### ⭐⭐⭐⭐ HIGH PRIORITY: Systemd Service Isolation
**Why**: Best-practice Linux deployment
- PrivateTmp, ProtectSystem, RestrictAddressFamilies
- Per-service resource limits
- Automatic restart and supervision

**systemd unit file**:
```ini
[Service]
User=solo
PrivateTmp=yes
ProtectSystem=strict
ReadWritePaths=/var/lib/solo
RestrictAddressFamilies=AF_INET AF_INET6
NoNewPrivileges=yes
MemoryLimit=512M
SystemCallFilter=@system-service ~@privileged
```

---

### ⭐⭐⭐ USEFUL: POSIX Capabilities (Linux)
**Why**: Fine-grained OS-level privilege control
- CAP_NET_BIND_SERVICE (bind to ports <1024 without root)
- Irreversible privilege drop
- Per-thread, per-process enforcement

**Container setup**:
```dockerfile
USER solo
RUN setcap cap_net_bind_service=ep /opt/solo/bin/erl
RUN setcap -r /opt/solo/bin/erl  # Drop all others
```

---

### ⭐⭐⭐⭐ GOOD: Erlang Distribution Trust (Existing)
**Why**: Already built-in; enhance with layers above
- Cookie-based mutual authentication (pre-shared key)
- TLS optional encryption layer
- Add application-level ACLs on top

**Enhanced with TLS**:
```elixir
# rel/env.erl
{kernel, [
  {ssl_dist_opt, [
    {certfile, "/etc/solo/cert.pem"},
    {keyfile, "/etc/solo/key.pem"},
    {verify, verify_peer}
  ]}
]}
```

---

### ⭐⭐⭐⭐ EXCELLENT: Zircon Rights (Conceptual Only)
**Why**: Architecture inspiration, not directly portable
- Rights as metadata on handles
- Attenuation by creating restricted handles
- Revocation via handle invalidation

**Conceptual mapping to Elixir**:
```elixir
# Fuchsia: zx_handle_duplicate(h, reduced_rights)
# Elixir:  {:attenuated, pid, allowed_ops}

# Fuchsia: zx_channel_write() checks rights
# Elixir:  GenServer.call() checks if msg in allowed
```

---

## Three-Layer Security Architecture

```
┌─────────────────────────────────────────┐
│ Layer 3: Deployment (Systemd)           │
│ - Namespace isolation (PrivateTmp, etc) │
│ - Resource limits (MemoryLimit)         │
│ - Syscall restrictions (seccomp)        │
└────────────────────┬────────────────────┘
                     │
┌────────────────────▼────────────────────┐
│ Layer 2: Inter-Node (TLS Distribution)  │
│ - Encrypted inter-node communication    │
│ - Cert-based identity verification      │
│ - Application-level ACLs in Elixir      │
└────────────────────┬────────────────────┘
                     │
┌────────────────────▼────────────────────┐
│ Layer 1: In-Process (OCap + Wrappers)   │
│ - Process references as capabilities    │
│ - Attenuation (restricted pids)         │
│ - No global registries for secrets      │
└─────────────────────────────────────────┘
```

---

## Implementation Checklist

### Immediate (1-2 weeks)
- [ ] Document which modules/processes have sensitive APIs
- [ ] Create `Solo.Capability` module (unforgeable tokens)
- [ ] Audit `:global`, `:pg`, `:ets` registries - replace with capability passing
- [ ] Wrap sensitive GenServers with `Solo.AttenuatedProcess`

### Short-term (2-4 weeks)
- [ ] Create systemd service unit with isolation options
- [ ] Test seccomp filter: `SystemCallFilter=@system-service ~@privileged`
- [ ] Drop POSIX capabilities: only keep CAP_NET_BIND_SERVICE if needed
- [ ] Enable TLS for Erlang distribution if multi-node

### Medium-term (1-2 months)
- [ ] Implement audit logging for capability checks
- [ ] Add health checks via systemd sd_notify()
- [ ] Document "capability contracts" for each module
- [ ] Add expiration to delegated capabilities (tokens)

---

## Common Pitfalls to Avoid

### ❌ Using global registries for sensitive resources
```elixir
# Bad:
:global.register_name(:database, db_pid)
Database.query(:database, "SELECT ...")

# Good:
# Pass db_pid directly or through capability-aware modules
Database.query(db_pid, "SELECT ...")
```

### ❌ Assuming all nodes equally trust each other
```elixir
# Bad (multi-node without auth):
:rpc.call(untrusted_node, Mod, fn, args)

# Good:
case authorize_node(untrusted_node) do
  :ok -> :rpc.call(untrusted_node, Mod, fn, args)
  :denied -> {:error, :unauthorized}
end
```

### ❌ Running BEAM as root
```bash
# Bad:
erl -name solo@host

# Good (systemd):
User=solo
Group=solo
NoNewPrivileges=yes
```

### ❌ No syscall restrictions
```bash
# Bad (container):
docker run myapp

# Good:
docker run \
  --security-opt=seccomp=restricted_profile.json \
  --cap-drop=ALL \
  --cap-add=NET_BIND_SERVICE \
  myapp
```

---

## Threat Model Summary

| Threat | Mitigated By | Strength |
|--------|--------------|----------|
| Process communication leakage | OCap + wrappers | ⭐⭐⭐⭐⭐ |
| Unauthorized local operations | AttenuatedProcess + tokens | ⭐⭐⭐⭐⭐ |
| Privilege escalation | systemd + pledge/seccomp | ⭐⭐⭐⭐ |
| Inter-node unauthorized RPC | TLS + application ACLs | ⭐⭐⭐⭐ |
| Resource exhaustion | systemd limits + cgroups | ⭐⭐⭐⭐ |
| Code execution (unknown vuln) | OS sandboxing (Pledge/seccomp) | ⭐⭐⭐ |
| Network eavesdropping | TLS | ⭐⭐⭐⭐⭐ |
| Malicious node in cluster | TLS certs + manual review | ⭐⭐⭐ |

---

## Resources & Links

- **OCap in Elixir**: https://erights.org/talks/thesis/ (Mark Miller's work)
- **Pledge/Unveil**: man pledge(2), man unveil(2) on OpenBSD
- **Systemd Security**: https://www.freedesktop.org/wiki/Software/systemd/SecurityFeatures/
- **Erlang TLS Distribution**: https://erlang.org/doc/apps/ssl/using_ssl.html
- **Zircon Rights**: https://fuchsia.dev/fuchsia-src/concepts/kernel/rights

---

## Decision: Which Patterns to Implement?

**For a secure solo deployment**, use this decision tree:

```
┌─ Run on Linux systemd?
│  ├─ YES ──→ Use systemd isolation (mandatory)
│  └─ NO  ──→ Use pledge/unveil (OpenBSD) or seccomp
│
├─ Single node or cluster?
│  ├─ Cluster ──→ Enable TLS distribution
│  └─ Single  ──→ Optional TLS for admin interface
│
└─ OCap at code level?
   ├─ Always  ──→ Implement for all sensitive modules
   └─ At least for database, auth, config
```

**Recommended minimal stack**:
1. OCap patterns (always)
2. Systemd isolation (Linux) or pledge (OpenBSD)
3. TLS distribution (if multi-node)

