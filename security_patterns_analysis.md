# Capability-Based Security & OS Design Patterns: Analysis for Solo

## Executive Summary
Six key security patterns examined for applicability to solo's Elixir architecture. **Recommendation**: Adopt a hybrid approach combining OCap principles (core), Erlang distribution trust model (existing), and pledge-inspired promise patterns (tactical).

---

## 1. OBJECT CAPABILITY MODEL (OCap)

### Core Concept & Guarantees
**Unforgeable references** as the foundation: capabilities are object references that combine:
- Identity (which object to operate on)
- Authority (which operations are permitted)
- Unforgeability (can't be created without existing capability chain)

**Only connectivity begets connectivity**: A process can only access objects it received through:
1. **Initial conditions** - bootstrap state
2. **Parenthood** - objects it created
3. **Endowment** - parent handed capabilities to children
4. **Introduction** - received via message from another object

### Isolation & Access Control Mechanism
- **No ambient authority**: Can't look up resources by name/global registry
- **Principle of least privilege** built-in: objects only hold capabilities they need
- **Attenuation** pattern: wrap capabilities in proxies to restrict operations (read-only, revocable, rate-limited)
- **Membrane pattern**: deep attenuation transitively applied to objects accessed through wrapped capability

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Performance** | Indirection overhead from proxy/wrapper patterns | No permission checks per operation (done at open-time) |
| **Complexity** | Requires explicit capability passing; no "magic" globals | Compositional security: can reason about authority locally |
| **Expressiveness** | Can't support revocation of intermediate parties | Enables revocation at single point (wrapper) |

### Why OCap Matters for Solo
Elixir/Erlang processes are **already capability-like**:
- Process IDs (`pid`) are unforgeable handles
- Message passing is the only inter-process communication
- Processes can only interact with pids they know about

**OCap patterns map naturally** to Elixir's actor model, making it easier to build secure, composable systems.

---

## 2. POSIX CAPABILITIES (Linux)

### Core Concept & Guarantees
Granular **per-process capabilities** replacing binary root/non-root privilege:
- ~40+ capabilities (CAP_NET_BIND_SERVICE, CAP_SYS_ADMIN, CAP_DAC_OVERRIDE, etc.)
- Three sets per thread: **permitted** (max allowed), **effective** (currently active), **inheritable** (survive execve)
- **Bounding set**: kernel-enforced limit on capabilities a process can ever gain
- File capabilities: executable can grant specific caps to its process at launch

### Isolation & Access Control Mechanism
```
Traditional: root (UID 0) = ALL privileges
POSIX caps:  fine-grained per-operation grants
```

Examples:
- `CAP_NET_BIND_SERVICE`: bind to ports <1024 without being root
- `CAP_DAC_OVERRIDE`: bypass file permission checks
- `CAP_SYS_ADMIN`: overloaded catch-all (≈40 operations grouped)

**Enforcement**: Kernel checks effective set before allowing operation. Drop cap → operation denied.

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Granularity** | 64-bit limit means coarse grouping (CAP_SYS_ADMIN is huge) | Finer than root/non-root but not per-operation |
| **Revocation** | Once dropped, can't regain (unless execve with file caps) | Irreversible privilege drop is safe |
| **Complexity** | Apps must understand which caps they need | Better than "run as root" for security |

### Why This Matters for Solo
**Limited direct applicability**: POSIX caps are OS-level, solo runs on BEAM VM. But the **design philosophy** is valuable:
- Identify minimal privilege sets needed
- Make privilege grants explicit and auditable
- Default to dropping unnecessary capabilities

**For solo's Elixir code**:
- Can restrict BEAM process to specific OS caps at startup (via systemd, docker, erl flags)
- Example: Node doing only HTTP→Postgres should drop `CAP_NET_RAW`, `CAP_SYS_ADMIN`

---

## 3. PLEDGE/UNVEIL (OpenBSD)

### Core Concept & Guarantees
**Promise-based API restriction**: process declares what system calls it will need, OS kills it if it violates.

**pledge(promises, execpromises)**:
- `promises`: current process restrictions
- `execpromises`: restrictions inherited by child processes via execve
- One-way ratchet: can only reduce, never expand capabilities
- Violation: uncatchable SIGABRT with core dump

**unveil(path, permissions)**:
- Filesystem namespace scoping
- Restrict open() to specific paths
- Stacked restrictions: each unveil further constrains

### Example
```c
pledge("stdio rpath inet dns", NULL);  // HTTP client
pledge("stdio rpath wpath cpath", NULL); // now can write files
// Process execution: if tries socket(), gets SIGABRT
```

**Restricted API categories**:
- `stdio`: read/write operations
- `rpath`/`wpath`/`cpath`: file read/write/create
- `inet`/`unix`/`dns`: networking
- `proc`/`exec`: process creation
- `tty`, `audio`, `video`: device-specific

### Isolation & Access Control Mechanism
- **Syscall filtering at kernel level** (like seccomp on Linux)
- **No context switching**: violations fail immediately with SIGABRT
- **Namespace restriction** (unveil) for filesystem

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Granularity** | Promises are coarse (e.g., `inet` includes all socket ops) | Covers most common use cases |
| **Flexibility** | Static promises; can't change at runtime | Simple to reason about: once declared, contracts enforced |
| **Debuggability** | Violations kill the process (good security, bad dev experience) | Can use `error` promise to warn instead of kill |
| **Compatibility** | OpenBSD-only | Portable seccomp on Linux approximates it |

### Why This Matters for Solo
**Direct mapping to Elixir**:
- Can wrap Node startup to pledge before BEAM initializes
- declare `stdio rpath wpath inet dns` if running HTTP server + file ops
- `unveil("/var/solo/*", "rwc")` for data directory isolation

**Stronger than POSIX caps**: pledge is **positive (allowlist)** vs caps are **negative (blocklist)**
- More secure: default-deny vs default-allow

**Implementation strategy**:
```elixir
# At startup, before accepting requests
:erlang.open_port({:spawn, "pledge -p 'stdio rpath inet dns' erl ..."}, [])
# Or in systemd SystemCallFilter=
```

---

## 4. SYSTEMD SERVICE ISOLATION

### Core Concept & Guarantees
**Service unit file configurations** for per-service sandboxing:

Key directives:
- **PrivateTmp**: `/tmp` and `/var/tmp` isolated per-service
- **PrivateDevices**: restricted `/dev` (no access to block devices, etc.)
- **PrivateNetwork**: isolated network namespace (can't see other services' traffic)
- **ReadOnlyPaths=/NonAccessiblePaths**: restrict filesystem
- **RestrictAddressFamilies**: allow only AF_INET/AF_INET6, deny AF_UNIX, etc.
- **SystemCallFilter**: allow/deny specific syscalls (seccomp)
- **MemoryLimit/CPUQuota**: resource limits
- **User=**: run as unprivileged user
- **ProtectSystem=strict**: entire `/usr` and `/etc` read-only
- **ProtectHome=yes**: hide `/home` and `/root`

### Example
```ini
[Service]
PrivateTmp=yes
PrivateDevices=yes
ProtectSystem=strict
ReadWritePaths=/var/lib/solo
RestrictAddressFamilies=AF_INET AF_INET6
SystemCallFilter=@system-service
SystemCallErrorNumber=EPERM
User=solo
```

### Isolation & Access Control Mechanism
Uses **Linux kernel namespaces** + **seccomp filters**:
- Mount namespace: private `/tmp`, `/dev`
- Network namespace: isolated networking
- User namespace: unprivileged user (UID remapping)
- Seccomp: allow/deny syscalls

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Complexity** | Many options; hard to get right | Comprehensive: covers multiple isolation dimensions |
| **Debuggability** | Restricting seccomp can hide bugs (call to unintended syscall) | Failures are explicit and auditable in systemd logs |
| **Performance** | Namespace overhead (~1-2% in studies) | Lightweight compared to VMs |
| **Portability** | Linux-only (cgroup v2 features need recent kernel) | Well-supported in container/cloud environments |

### Why This Matters for Solo
**Directly applicable** if running under systemd (most Linux servers):

```ini
[Service]
# For solo HTTP server with local database
Type=notify
ExecStart=/opt/solo/bin/solo
User=solo
Group=solo
WorkingDirectory=/var/lib/solo
PrivateTmp=yes
PrivateDevices=yes
NoNewPrivileges=yes
ReadWritePaths=/var/lib/solo /var/log/solo
ProtectSystem=strict
ProtectHome=yes
RestrictAddressFamilies=AF_INET AF_INET6
RestrictNamespaces=yes
RestrictSUIDSGID=yes
LockPersonality=yes
MemoryLimit=512M
```

**Integration points**:
- Can query `LISTEN_ADDR`, `LISTEN_PORT` from systemd socket activation
- Health checks via `sd_notify()` (Elixir library exists)
- Automatic restart, process supervision

---

## 5. ZIRCON KERNEL / FUCHSIA OS (Rights & Handles)

### Core Concept & Guarantees
**Handle-based capability system** at kernel level:
- **Handle**: unforgeable kernel-mediated reference to an object (port, channel, VMO, etc.)
- **Rights**: attached to each handle, specify allowed operations (ZX_RIGHT_READ, WRITE, EXECUTE, DUPLICATE, TRANSFER, etc.)
- **zx_handle_duplicate()**: create new handle with **attenuated rights** (subset)
- **zx_handle_replace()**: atomically swap handle with restricted version
- **zx_channel_write()**: only allows sending handles you have rights to send

### Rights & Their Meaning
| Right | Allows |
|-------|--------|
| ZX_RIGHT_DUPLICATE | Clone handle (with new rights) |
| ZX_RIGHT_TRANSFER | Send handle over channel |
| ZX_RIGHT_READ | Read from object |
| ZX_RIGHT_WRITE | Write to object |
| ZX_RIGHT_EXECUTE | Map as executable |
| ZX_RIGHT_MAP | Map to address space |
| ZX_RIGHT_DESTROY | Terminate object |
| ZX_RIGHT_SIGNAL | Send signals to object |
| ZX_RIGHT_INSPECT | Query object properties |

**Default rights** for each object type; can only reduce, never expand without capability.

### Isolation & Access Control Mechanism
```
Kernel enforces: Can only operate on handles you have with required right
Channel-based IPC: Handles transferred with messages, rights checked on send
```

Key property: **Once you lose a right, you can never regain it** (unless parent process gives you new handle).

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Universality** | Every resource is a handle; more uniform than Unix FDs | Consistent security model; no "special cases" |
| **Revocation** | Parent must track handles to revoke; no global revoke | Revocation is precise: sender controls which handles revoked |
| **Auditability** | All authority flows through kernel | Can trace "chain of custody" for any handle |
| **Complexity** | Requires thinking in handles, not names | Maps well to capability languages (E, Pony, etc.) |

### Why This Matters for Solo
**Conceptual match but not directly portable** (Fuchsia runs on BEAM-like VM internally):

1. **Elixir PIDs as handles**: Already unforgeable references
2. **Attenuation opportunities**: 
   - Wrap pid in struct with reduced protocol
   - GenServer that only accepts specific messages
   - Example: `RestrictedServer` that only allows `:get` and `:health` messages

```elixir
defmodule RestrictedPid do
  defstruct pid: nil, allowed_calls: [:get, :health]
  
  def call(%__MODULE__{pid: pid, allowed_calls: allowed}, msg) do
    case msg do
      {:call, fn, args} when fn in allowed ->
        GenServer.call(pid, {fn, args})
      _ -> {:error, :not_allowed}
    end
  end
end
```

3. **Duplicate with reduced rights**: Emulate via capability wrappers
4. **Transfer over channels**: Send restricted pids in messages

---

## 6. ERLANG DISTRIBUTED NODES

### Core Concept & Guarantees
**Cookie-based mutual authentication** between nodes:

**Handshake flow** (simplified):
1. Node A connects to Node B via TCP
2. A sends: `{node_name, version_flags}`
3. B responds: `{status, challenge}`
4. A sends: `{challenge_reply, MD5(B's_challenge + A's_cookie)}`
5. B sends: `{ack, MD5(A's_challenge + B's_cookie)}`
6. **Connection established** only if both cookies match

**Cookie validation**:
- Each node has one **shared secret** (cookie)
- A's outgoing cookie to B = B's incoming cookie
- Cookies never sent in cleartext, only in MD5 hashes
- Digest proves knowledge of cookie without revealing it

**Trust model**:
- All nodes in same cluster share same cookie
- Cookie is essentially a pre-shared key (PSK)
- No per-node identity verification (all nodes equally trusted)

### Isolation & Access Control Mechanism
```
Trusted network model:
- Cookie auth gates cluster membership
- Once connected, any node can send any message to any process
- No per-message or per-connection ACLs
- Relies on network isolation (firewall)
```

**Erlang Distribution Protocol** (ERTS):
- Two-phase handshake (authentication)
- Message format: `[length(4) | distribution_header | control | message]`
- Ordered delivery per connection (no message reordering)
- Atom cache for bandwidth optimization

### Trade-offs
| Aspect | Cost | Benefit |
|--------|------|---------|
| **Granularity** | Binary: in-cluster or out-cluster; no per-node ACLs | Simple, fast (no per-message checks) |
| **Threat model** | Assumes network isolation; weak against insider threats | Lightweight for trusted networks |
| **TLS support** | Optional; must configure separately | Can add encryption + cert-based auth |
| **Scalability** | Full-mesh connections; N nodes = N² connections | Direct connectivity; low latency |

**Modern improvements**:
- TLS distribution: adds confidentiality + cert-based verification
- `inet_ssl` and `inet_tls` transports
- Cookie can now be per-node (not just global)

### Why This Matters for Solo
**Existing strength**: Erlang's trust model is already quite good for **homogeneous clusters**.

**Leverage**:
1. **Inter-node process migration**: Use process references (pids) as unforgeable IDs
2. **Distributed application**: Rely on ERTS handshake for cluster membership
3. **Authorization layer on top**: Implement per-operation ACLs in Elixir, not at protocol

**Limitations**:
- All nodes equally trusted (no per-node privilege isolation)
- No fine-grained message-level authorization
- Assumes secure network (pre-shared cookie, firewall)

**Enhancement path**:
```elixir
defmodule Solo.DistributedAuth do
  # Wrap RPC calls with authorization checks
  def rpc_call(node, module, function, args, requester_pid, needed_perm) do
    case check_permission(requester_pid, needed_perm) do
      :ok -> :rpc.call(node, module, function, args)
      :denied -> {:error, :unauthorized}
    end
  end
end
```

---

## COMPARATIVE ANALYSIS TABLE

| Pattern | Granularity | Revocability | Auditability | Complexity | Fit for Solo |
|---------|-------------|--------------|--------------|-----------|--------------|
| **OCap** | Per-operation | High (wrapper) | Excellent | Medium-High | ⭐⭐⭐⭐⭐ |
| **POSIX Caps** | Per-capability | Limited (one-way drop) | Good | Medium | ⭐⭐⭐ |
| **Pledge** | Per-syscall-family | Very limited | Good | Low-Medium | ⭐⭐⭐⭐ |
| **Systemd isolation** | Per-namespace | Very limited | Excellent | Medium | ⭐⭐⭐⭐ |
| **Zircon Rights** | Per-handle-right | High (parent-driven) | Excellent | High | ⭐⭐⭐⭐ |
| **Erlang Distribution** | Per-cluster | None (network trust) | Fair | Low | ⭐⭐⭐⭐ |

---

## ACTIONABLE RECOMMENDATIONS FOR SOLO

### Tier 1: Core Architecture (Essential)
**Implement OCap principles in Elixir/GenServer patterns**:

1. **Process references as capabilities**:
   ```elixir
   # Bad: global registry
   Services.HTTP.call(:get, "/users/123")
   
   # Good: capability-based
   http_server_pid |> HTTP.call(:get, "/users/123")
   # Caller must have the pid; no global lookup
   ```

2. **Restrict message protocols**:
   ```elixir
   defmodule RestrictedServer do
     def new(base_pid, allowed_ops) do
       {:restricted, base_pid, allowed_ops}
     end
     
     def call({:restricted, pid, allowed}, op) when op in allowed do
       GenServer.call(pid, op)
     end
   end
   ```

3. **Explicit capability passing**:
   - Don't use ets/process registry for sensitive resources
   - Pass pids/capabilities directly in messages
   - Document "capability contracts" for modules

### Tier 2: Deployment (High Value)
**Systemd + pledge-like restrictions**:

1. **Systemd service unit** (assume systemd deployment):
   ```ini
   [Service]
   ExecStart=/opt/solo/bin/solo start
   User=solo
   PrivateTmp=yes
   ProtectSystem=strict
   ReadWritePaths=/var/lib/solo
   RestrictAddressFamilies=AF_INET AF_INET6
   NoNewPrivileges=yes
   SystemCallFilter=@system-service ~@privileged
   MemoryLimit=1G
   CPUQuota=50%
   ```

2. **Startup pledge wrapper** (if on OpenBSD):
   ```elixir
   defmodule Solo.Pledge do
     def restrict_early do
       # Before BEAM fully initializes
       System.cmd("pledge", ["stdio rpath wpath inet dns"])
     end
   end
   
   # In application.ex
   {:ok, _} = Application.start(:solo)
   Solo.Pledge.restrict_early()
   ```

3. **POSIX capabilities at container level** (Docker/K8s):
   ```dockerfile
   USER solo
   RUN setcap -r /opt/solo/bin/erl  # Drop all caps
   RUN setcap cap_net_bind_service=ep /opt/solo/bin/erl  # Only this one
   ```

### Tier 3: Inter-Node Security (If Multi-Node)
**Enhance Erlang distributed trust**:

1. **TLS-based distribution** (mandatory for untrusted networks):
   ```elixir
   # In rel/env.erl
   {kernel, [
     {inet_dist_use_interface, {127, 0, 0, 1}},
     {inet_dist_listen_min, 9001},
     {inet_dist_listen_max, 9010},
     {ssl_dist_opt, [
       {certfile, "/etc/solo/cert.pem"},
       {keyfile, "/etc/solo/key.pem"},
       {verify, verify_peer},
       {cacertfile, "/etc/solo/ca.pem"},
       {secure_renegotiate, true}
     ]}
   ]}
   ```

2. **Per-node authorization wrapper**:
   ```elixir
   defmodule Solo.DistAuth do
     def call_remote(node, module, fn, args) do
       # Check if we trust node and have permission
       case authorize_node(node) do
         :ok -> :rpc.call(node, module, fn, args)
         :denied -> {:error, :unauthorized}
       end
     end
   end
   ```

3. **Audit distributed RPC**:
   - Log all inter-node calls with source node + arguments
   - Monitor for unusual patterns

---

## SPECIFIC PATTERNS FOR SOLO'S ARCHITECTURE

### Pattern A: Role-Based Capability Tokens

Create typed, unforgeable tokens for capabilities:

```elixir
defmodule Solo.Capability do
  @opaque t :: {binary(), term()}
  
  def create(role, metadata) do
    token = :crypto.strong_rand_bytes(32)
    {:ok, _} = ETS.insert(:capabilities, {token, role, metadata})
    {token, role}
  end
  
  def check(token, required_role) do
    case ETS.lookup(:capabilities, token) do
      [{^token, ^required_role, _}] -> :ok
      _ -> :error
    end
  end
  
  def revoke(token) do
    ETS.delete(:capabilities, token)
  end
end
```

**Usage**:
```elixir
# At startup: user logs in, gets capability token
{cap_token, :read_user_data} = Solo.Capability.create(:read_user_data, %{user_id: 123})

# In handler: check capability before operation
:ok = Solo.Capability.check(cap_token, :read_user_data)
```

### Pattern B: Attenuated Process Wrappers

Restrict what operations are allowed on a process:

```elixir
defmodule Solo.AttenuatedProcess do
  def wrap(pid, allowed_messages) do
    {:attenuated, pid, allowed_messages}
  end
  
  def call({:attenuated, pid, allowed}, msg) do
    case msg do
      msg when msg in allowed ->
        GenServer.call(pid, msg)
      msg ->
        {:error, {:forbidden_message, msg}}
    end
  end
end
```

**Usage**:
```elixir
# Create a read-only interface to a resource
database_pid = Database.start_link()
read_only = Solo.AttenuatedProcess.wrap(database_pid, [{:get, :_}])

# Caller can only do :get operations
# Trying :delete or :write fails
```

### Pattern C: Capability Delegation with Expiry

Time-limited delegation:

```elixir
defmodule Solo.DelegatedCap do
  def delegate(cap, new_owner_pid, expires_at) do
    token = :crypto.strong_rand_bytes(32)
    :ets.insert(:delegations, {token, cap, new_owner_pid, expires_at})
    token
  end
  
  def check_delegated(token) do
    case :ets.lookup(:delegations, token) do
      [{^token, cap, _owner, expires_at}] ->
        if DateTime.utc_now() < expires_at, do: {:ok, cap}, else: {:error, :expired}
      [] ->
        {:error, :not_found}
    end
  end
end
```

**Usage**: API keys, session tokens, temporary access grants.

---

## THREAT MODEL IMPLICATIONS

### Assumptions
1. **BEAM VM is trusted**: exploit = game over (assume containerized/isolated at OS level)
2. **Network is partially untrusted**: TLS for inter-node
3. **Elixir code is trusted** (no arbitrary code execution): security is logical not cryptographic
4. **Admin has secure deployment** (systemd, pledge, firewall)

### Protected Against
- ✅ Information leakage via unintended process communication
- ✅ Unauthorized local operations (AttenuatedProcess pattern)
- ✅ Privilege escalation within node (capabilities not inherited)
- ✅ Revocation of access (wrapper/token patterns)
- ✅ Cross-service interference (systemd isolation)

### Not Protected Against
- ❌ Compromised Elixir code (need OS sandboxing: Pledge/systemd for that)
- ❌ Timing attacks on capability checks
- ❌ Side-channels in BEAM scheduling
- ❌ Network eavesdropping (mitigated by TLS distribution)

---

## IMPLEMENTATION ROADMAP

### Phase 1: Foundation (Week 1-2)
- [ ] Audit all `GenServer.call/cast` to identify public vs. restricted APIs
- [ ] Document capability model for solo's processes
- [ ] Create `Solo.Capability` module (Pattern A)
- [ ] Add capability checks to sensitive operations

### Phase 2: Wrapping (Week 3-4)
- [ ] Implement `Solo.AttenuatedProcess` (Pattern B)
- [ ] Wrap database, auth, and config processes with attenuation
- [ ] Test that wrapped processes reject disallowed operations
- [ ] Add audit logging to capability checks

### Phase 3: Deployment (Week 5-6)
- [ ] Create systemd service unit with security options
- [ ] Test in container with restricted seccomp profile
- [ ] Document capability requirements per module
- [ ] Add health checks, startup verification

### Phase 4: Distributed (Week 7+, if multi-node)
- [ ] Enable TLS for inter-node distribution
- [ ] Implement `Solo.DistAuth` wrapper
- [ ] Audit inter-node RPC calls
- [ ] Add node trust whitelist

---

## REFERENCES & FURTHER READING

1. **Object Capability Model**
   - Mark S. Miller: "Robust Composition" (thesis)
   - https://erights.org/talks/thesis/

2. **POSIX Capabilities**
   - man capabilities(7)
   - https://man7.org/linux/man-pages/man7/capabilities.7.html

3. **Pledge/Unveil**
   - OpenBSD pledge(2) and unveil(2) man pages
   - Alexander Bluhm: "OpenBSD's Pledge and Unveil" (EuroBSDcon talk)

4. **Systemd Security**
   - systemd.exec(5) man page (PrivateTmp, ProtectSystem, etc.)
   - https://www.freedesktop.org/software/systemd/man/systemd.exec.html

5. **Fuchsia Zircon**
   - https://fuchsia.dev/fuchsia-src/concepts/kernel/rights
   - https://fuchsia.dev/fuchsia-src/concepts/components/v2/capabilities

6. **Erlang Distribution**
   - Erlang Distribution Protocol (ERTS documentation)
   - http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html

---

## CONCLUSION

**For solo's Elixir architecture**, the recommendation is:

**OCap + Pledge + Systemd + Erlang Trust** = **Defense in Depth**

1. **At code level** (OCap): Use process references and capability wrappers to restrict what each component can do
2. **At OS level** (Pledge/Systemd): Restrict BEAM process to specific syscalls and filesystem paths
3. **At network level** (Erlang + TLS): Mutually authenticate nodes; use TLS for encryption
4. **At deployment** (Systemd): Isolate resources, drop unneeded privileges, enable audit logging

This combination provides **layered security**: even if one layer is compromised, others still hold.

