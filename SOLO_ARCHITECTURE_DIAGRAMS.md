# Solo Architecture - Visual Diagrams

## 1. Process Hierarchy Tree

```
┌─────────────────────────────────────────────────────────────────┐
│                    Solo.Application                              │
│                   (Application Callback)                         │
└──────────────────────────────┬──────────────────────────────────┘
                               │
                    ┌──────────▼──────────┐
                    │   Solo.Supervisor   │
                    │  (one_for_one)      │
                    └──────────┬──────────┘
                               │
        ┌──────────────────────┴──────────────────────┐
        │                                              │
        ▼                                              ▼
┌──────────────────────────────┐        ┌───────────────────────┐
│ Solo.System.Supervisor        │        │Solo.Tenant.Supervisor│
│ (rest_for_one)               │        │(DynamicSupervisor)   │
│                              │        │                      │
│ Children (in order):         │        │ Children (dynamic):   │
│ 1. EventStore                │        │ - ServiceSupervisor-1 │
│ 2. AtomMonitor               │        │ - ServiceSupervisor-2 │
│ 3. Registry                  │        │ - ...                │
│ 4. Deployer                  │        │                      │
│ 5. Capability.Manager        │        └──────────┬───────────┘
│ 6. LoadShedder               │                   │
│ 7. Vault                     │                   │
│ 8. ServiceRegistry           │                   │
│ 9. Telemetry                 │                   │
│10. Gateway                   │                   │
└──────────────────────────────┘                   │
                                                   ▼
                                    ┌──────────────────────────┐
                                    │ServiceSupervisor-N       │
                                    │(DynamicSupervisor)       │
                                    │for tenant_id="agent_X"   │
                                    │                          │
                                    │Children (per service):   │
                                    │ - service-pid-1          │
                                    │ - service-pid-2          │
                                    │ - ...                    │
                                    └──────────────────────────┘
```

---

## 2. Data Flow: Service Deployment

```
┌─────────────────────────────────────────────────────────────────┐
│                     External Agent (via gRPC)                    │
└────────────────────────────────┬────────────────────────────────┘
                                 │
                 Deploy RPC: {tenant_id, service_id, code}
                                 │
                                 ▼
                        ┌────────────────┐
                        │ Solo.Gateway   │
                        │   (gRPC)       │
                        └────────┬───────┘
                                 │
                                 ▼
                   ┌─────────────────────────┐
                   │ Verify mTLS Certificate │
                   │ Extract tenant_id       │
                   └────────────┬────────────┘
                                │
                                ▼
                 ┌──────────────────────────┐
                 │Solo.Deployment.Deployer │
                 │   handle_call(:deploy)  │
                 └────────────┬─────────────┘
                              │
                    ┌─────────┴─────────┐
                    │                   │
                    ▼                   ▼
         ┌────────────────────┐  ┌───────────────────┐
         │Ensure Tenant Spv.  │  │ Validate Format   │
         │ (create if needed) │  │ (:elixir_source)  │
         └────────┬───────────┘  └─────────────┬─────┘
                  │                            │
                  └──────────┬─────────────────┘
                             │
                             ▼
              ┌──────────────────────────────┐
              │ Solo.Deployment.Compiler     │
              │ Compile Elixir Source Code   │
              │ Namespace: Solo.User_*_*    │
              └──────────┬───────────────────┘
                         │
                ┌────────▼────────┐
                │                 │
                ▼                 ▼
         {:ok, [{module,  {:error,
          bytecode}]}      reason}
                │                 │
                │                 ▼
                │        ┌─────────────────┐
                │        │Emit :deployment │
                │        │    _failed      │
                │        └─────────────────┘
                │
                ▼
    ┌──────────────────────────────────┐
    │ Validate start_link/1 exported   │
    └────────────┬─────────────────────┘
                 │
                 ▼
    ┌──────────────────────────────────┐
    │DynamicSupervisor.start_child()   │
    │ Under tenant supervisor          │
    └────────────┬─────────────────────┘
                 │
                 ▼
    ┌──────────────────────────────────┐
    │Solo.Registry.register()          │
    │  Key: {tenant_id, service_id}    │
    │  Value: service_pid              │
    └────────────┬─────────────────────┘
                 │
                 ▼
    ┌──────────────────────────────────┐
    │Solo.EventStore.emit()            │
    │ Event: :service_deployed         │
    │ Subject: {tenant_id, service_id} │
    └────────────┬─────────────────────┘
                 │
                 ▼
    ┌──────────────────────────────────┐
    │Return {:ok, service_pid}         │
    │to gRPC caller                    │
    └──────────────────────────────────┘
```

---

## 3. EventStore Data Flow

```
┌──────────────────────────────────────────────────────────┐
│                  Any Module                              │
│           Emits Event (async):                           │
│   Solo.EventStore.emit(:service_deployed, subject, ...) │
└───────────────────┬──────────────────────────────────────┘
                    │
                    │ GenServer.cast
                    ▼
        ┌──────────────────────┐
        │ Solo.EventStore      │
        │ handle_cast/2        │
        └────────┬─────────────┘
                 │
        ┌────────▼────────┐
        │                 │
        ▼                 ▼
   ┌───────────┐    ┌──────────────┐
   │Create     │    │Get next ID   │
   │Event      │    │from state    │
   │struct     │    └──────┬───────┘
   │           │           │
   └─────┬─────┘    ┌──────▼──────┐
         │          │             │
         └──────┬───┤ Increment   │
                │   │ counter in  │
                │   │ state       │
                │   └─────────────┘
                │
                ▼
        ┌───────────────────┐
        │ CubDB.put(db,     │
        │  {:event, id},    │
        │  event)           │
        │                   │
        │ CubDB.put(db,     │
        │  :next_id,        │
        │  next_id + 1)     │
        └─────────┬─────────┘
                  │
                  ▼
        ┌──────────────────┐
        │Disk persisted    │
        │(immediate fsync) │
        └──────────────────┘
```

---

## 4. Vault Encryption/Decryption

```
┌────────────────────────────────────────────────────────┐
│           Service Calls:                               │
│  Solo.Vault.store(tenant_id, name, secret, key)       │
│  OR                                                    │
│  Solo.Vault.retrieve(tenant_id, name, key)            │
└───────────────────┬────────────────────────────────────┘
                    │
        ┌───────────┴────────────┐
        │                        │
        ▼                        ▼
  ┌──────────────┐        ┌──────────────────┐
  │STORE         │        │RETRIEVE          │
  └──────┬───────┘        └────────┬─────────┘
         │                         │
         ▼                         ▼
 ┌──────────────────┐    ┌────────────────────┐
 │Derive key from   │    │CubDB.get()         │
 │master_key        │    │{:secret, t, name}  │
 │using SHA-256     │    │                    │
 │                  │    │Retrieve encrypted  │
 │Generated: 32B    │    │blob                │
 └────────┬─────────┘    └────────┬───────────┘
          │                       │
          ▼                       ▼
 ┌──────────────────┐    ┌────────────────────┐
 │Generate IV       │    │Derive same key     │
 │(12 bytes random) │    │from provided key   │
 │                  │    │                    │
 │crypto.strong_    │    │Extract: IV (12B)   │
 │  rand_bytes(12)  │    │         + Tag (16B)│
 └────────┬─────────┘    │         + Cipher   │
          │              └────────┬───────────┘
          ▼                       │
 ┌──────────────────┐            ▼
 │AES-256-GCM       │   ┌────────────────────┐
 │Encrypt:          │   │AES-256-GCM         │
 │crypto_one_time   │   │Decrypt:            │
 │_aead(:aes_256_   │   │crypto_one_time_    │
 │  gcm, ...)       │   │  aead(...false)    │
 │                  │   │                    │
 │Returns:          │   │Verify tag (auth)   │
 │{ciphertext, tag} │   │                    │
 └────────┬─────────┘   │Return plaintext    │
          │             │on success          │
          ▼             └────────┬───────────┘
 ┌──────────────────┐            │
 │Combine & Store:  │            ▼
 │ IV + tag +       │   ┌────────────────────┐
 │ ciphertext       │   │Emit :secret_       │
 │                  │   │  accessed event    │
 │CubDB.put(db,     │   │                    │
 │{:secret, t, n},  │   │Return {:ok, value} │
 │combined)         │   │                    │
 └────────┬─────────┘   └────────────────────┘
          │
          ▼
 ┌──────────────────┐
 │Emit :secret_     │
 │  stored event    │
 │                  │
 │Return :ok        │
 └──────────────────┘
```

---

## 5. Capability Token Lifecycle

```
┌────────────────────────────────────────────┐
│      Agent requests capability             │
│Solo.Capability.Manager.grant(tenant_id, ...|
└────────────────────┬───────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │ Grant GenServer call   │
        │                        │
        │1. Create Solo.         │
        │   Capability struct    │
        └────────┬───────────────┘
                 │
        ┌────────▼────────┐
        │                 │
        ▼                 ▼
   ┌─────────┐    ┌──────────────┐
   │Generate │    │Hash token    │
   │32-byte  │    │using SHA-256 │
   │token    │    │              │
   │:crypto. │    │Store only    │
   │strong_  │    │hash (not     │
   │rand_    │    │plaintext!)   │
   │bytes(32)│    └──────┬───────┘
   └────┬────┘           │
        │                │
        └────────┬───────┘
                 │
                 ▼
        ┌──────────────────┐
        │:ets.insert(      │
        │ :capabilities,   │
        │ {token_hash,     │
        │  capability})    │
        └────────┬─────────┘
                 │
                 ▼
        ┌──────────────────┐
        │Emit :capability_ │
        │  granted event   │
        └────────┬─────────┘
                 │
                 ▼
        ┌──────────────────┐
        │Return {:ok,      │
        │ token} to agent  │
        └──────────────────┘


┌────────────────────────────────────────────┐
│    Service uses capability for access:     │
│ Solo.Capability.Manager.verify(token, ...) │
└────────────────────┬───────────────────────┘
                     │
                     ▼
        ┌────────────────────────┐
        │ Verify GenServer call  │
        │                        │
        │1. Hash provided token  │
        │2. Lookup in ETS        │
        │3. Check TTL            │
        │4. Check permissions    │
        └────────┬───────────────┘
                 │
        ┌────────▼─────┐
        │              │
        ▼              ▼
   ┌─────────┐    ┌────────────┐
   │Valid &  │    │Invalid:    │
   │Allowed  │    │Expired or  │
   │         │    │Revoked     │
   │Return   │    │            │
   │:ok      │    │Emit :cap._ │
   │         │    │denied event│
   └─────────┘    │            │
                  │Return      │
                  │{:error, ..}│
                  └────────────┘
```

---

## 6. Service Lifecycle State Machine

```
                        UNDEPLOYED
                            ▲
                            │
                            │ Kill successful
                            │
              ┌─────────────────────────┐
              │                         │
              │                         │
         ┌────▼─────┐           ┌──────▼──┐
         │           │           │          │
         │  CRASHED  │◄─────────►│ RUNNING  │
         │           │ Process   │          │
         │           │  down     │          │
         └────▲─────┘           └──────┬──┘
              │                        │
              │                        │
              │ Supervisor restart     │ Deploy
              │ (transient: no)        │ Successful
              │                        │
              └────────────────────────┘
                      │
                      │
                      ▼
                   STARTING
                      │
                      │
                      ▼
                  DEPLOYED
                      │
                      │ startup_timeout
                      │ or validation
                      │ failure
                      │
                      ▼
                DEPLOYMENT_FAILED
```

---

## 7. Multi-Tenant Isolation

```
┌─────────────────────────────────────────────────────────────┐
│                    Solo System                               │
├─────────────────────────────────────────────────────────────┤
│  Shared Resources:                                           │
│  - EventStore (CubDB)                                       │
│  - Registry (global service discovery)                      │
│  - Capability.Manager (ETS)                                │
│  - Vault (CubDB)                                           │
│  - Gateway (mTLS tenant extraction)                         │
└────────────────────────────────────────────────────────────┬┘
                                                              │
                ┌─────────────────────────┐
                │                         │
                ▼                         ▼
       ┌─────────────────┐      ┌─────────────────┐
       │ Tenant: agent_1 │      │ Tenant: agent_2 │
       │                 │      │                 │
       │ Supervisor      │      │ Supervisor      │
       │ (Isolated)      │      │ (Isolated)      │
       │                 │      │                 │
       │ Services:       │      │ Services:       │
       │ ┌─────────────┐ │      │ ┌─────────────┐ │
       │ │ service-1-1 │ │      │ │ service-2-1 │ │
       │ │ service-1-2 │ │      │ │ service-2-2 │ │
       │ │ ...         │ │      │ │ ...         │ │
       │ └─────────────┘ │      │ └─────────────┘ │
       │                 │      │                 │
       └─────────────────┘      └─────────────────┘

Isolation Guarantees:
✓ Separate supervisor trees (crash isolation)
✓ Separate processes (memory isolation)
✓ Registry keys include tenant_id
✓ EventStore events tagged with tenant_id
✓ Resource limits per tenant
✗ Shared Erlang VM (no kernel-level isolation)
```

---

## 8. Request Flow: gRPC Deploy Call

```
Agent (external)
    │
    │ gRPC RPC: Deploy(tenant_id, service_id, code)
    │ (mTLS certificate embedded)
    │
    ▼
┌──────────────────────────────────┐
│ Solo.Gateway.Server              │
│ (gRPC service handler)           │
│                                  │
│ Extract mTLS client cert         │
│ → tenant_id = cert subject CN    │
└──────────────────┬───────────────┘
                   │
                   ▼
        ┌──────────────────┐
        │Validate tenant   │
        │credentials       │
        └────────┬─────────┘
                 │
                 ▼
        ┌──────────────────┐
        │Check capability  │
        │ :deploy on       │
        │ "kernel"         │
        └────────┬─────────┘
                 │
                 ▼
        ┌──────────────────┐
        │Call Solo.        │
        │Deployment.       │
        │Deployer.deploy() │
        └────────┬─────────┘
                 │
    [See Service Deployment Diagram]
                 │
                 ▼
        ┌──────────────────┐
        │Return DeployResp │
        │{service_pid}     │
        │via gRPC stream   │
        └────────┬─────────┘
                 │
                 ▼
            Agent receives
          service_pid in response
```

---

## 9. Data Persistence Layout (CubDB)

```
┌──────────────────────────────────────────┐
│        ./data/events (EventStore)        │
├──────────────────────────────────────────┤
│                                          │
│ :next_id                 ──► 1043        │
│                                          │
│ {:event, 1}              ──► Event{}     │
│ {:event, 2}              ──► Event{}     │
│ {:event, 3}              ──► Event{}     │
│ ...                                      │
│ {:event, 1042}           ──► Event{}     │
│                                          │
│ Key properties:                          │
│ ✓ Append-only (no deletes)               │
│ ✓ Ordered by event ID                   │
│ ✓ Immutable event structs                │
│ ✓ Dense numbering (no gaps)              │
│                                          │
└──────────────────────────────────────────┘


┌──────────────────────────────────────────┐
│         ./data/vault (Vault)             │
├──────────────────────────────────────────┤
│                                          │
│ {:secret, "agent_1", "db_password"}      │
│           ──► [IV(12B) + Tag(16B) +      │
│               Ciphertext]                │
│                                          │
│ {:secret, "agent_1", "api_key"}          │
│           ──► [IV(12B) + Tag(16B) +      │
│               Ciphertext]                │
│                                          │
│ {:secret, "agent_2", "token"}            │
│           ──► [IV(12B) + Tag(16B) +      │
│               Ciphertext]                │
│                                          │
│ Key properties:                          │
│ ✓ Encrypted at rest                      │
│ ✓ Unique IV per secret                   │
│ ✓ Authenticated encryption (GCM)         │
│ ✓ Tenant isolation (key component)       │
│                                          │
└──────────────────────────────────────────┘
```

---

## 10. Supervisor Strategy Comparison

```
┌───────────────────────────────────────────────────┐
│        Solo.Supervisor (:one_for_one)             │
├───────────────────────────────────────────────────┤
│ If System.Supervisor dies:                        │
│  ├─ Only System.Supervisor is restarted          │
│  ├─ Tenant.Supervisor unaffected                 │
│  └─ Other System services NOT restarted          │
│                                                   │
│ If Tenant.Supervisor dies:                        │
│  ├─ Only Tenant.Supervisor is restarted          │
│  ├─ System.Supervisor unaffected                 │
│  ├─ All tenant supervisors cleaned up            │
│  └─ Services terminate                           │
└───────────────────────────────────────────────────┘


┌───────────────────────────────────────────────────┐
│      System.Supervisor (:rest_for_one)            │
├───────────────────────────────────────────────────┤
│ If EventStore dies (child 1):                     │
│  └─ All children 2-10 are RESTARTED             │
│                                                   │
│ If Registry dies (child 3):                       │
│  ├─ Children 1-2 left alone                      │
│  └─ Children 4-10 are RESTARTED                 │
│                                                   │
│ If Gateway dies (child 10):                       │
│  ├─ Children 1-9 left alone                      │
│  └─ Only Gateway is RESTARTED                   │
│                                                   │
│ Rationale:                                        │
│ • EventStore must always be available            │
│ • Later services depend on earlier ones          │
│ • Order: E-store → ... → Gateway                │
└───────────────────────────────────────────────────┘


┌───────────────────────────────────────────────────┐
│    Tenant.Supervisor (:one_for_one)              │
├───────────────────────────────────────────────────┤
│ If service 1 crashes:                             │
│  ├─ Service 1 restarted (per policy)             │
│  ├─ Service 2 unaffected                         │
│  ├─ Service 3 unaffected                         │
│  └─ Tenant remains operational                   │
│                                                   │
│ Rationale:                                        │
│ • Services are independent                       │
│ • One service failure shouldn't kill tenant      │
│ • Configurable restart policy per service       │
└───────────────────────────────────────────────────┘
```

---

## 11. Event-Driven Architecture Timeline

```
Time  Component              Event Type        Payload
────────────────────────────────────────────────────────
T0    Application           :system_started    -
T1    Deployer              :service_deployed  {tenant_id, service_id}
T2    Service               :service_started   {tenant_id, service_id}
T3    Capability.Manager    :capability_       {resource, permissions, ttl}
                            granted
T4    Vault                 :secret_stored     {secret_name}
T5    Service               :resource_         {memory, threshold}
                            violation
T6    HotSwap               :hot_swap_         {service_id, old_hash,
                            started            new_hash}
T7    HotSwap               :hot_swap_         {service_id}
                            succeeded
T8    Deployer              :service_killed    {tenant_id, service_id}
T9    AtomMonitor           :atom_usage_high   {count, threshold}

Each event:
• Has unique monotonic ID (never reused)
• Timestamped with Erlang monotonic_time
• Tagged with tenant_id (if applicable)
• Logged to immutable CubDB
• Available for replay/audit
```

---

## 12. Capability Authorization Flow

```
Agent (external)
    │
    │ 1. Request capability
    │    grant(tenant_id, "eventstore", ["read"], 3600)
    │
    ▼
┌─────────────────────────────────────┐
│ Solo.Capability.Manager             │
│                                     │
│ 2. Generate 256-bit token           │
│    token = :crypto.strong_rand_     │
│             bytes(32)               │
│                                     │
│ 3. Hash token                       │
│    hash = SHA256(token)             │
│                                     │
│ 4. Create capability struct         │
│    cap = %Capability{               │
│      resource_ref: "eventstore",    │
│      token_hash: hash,              │
│      permissions: ["read"],         │
│      expires_at: T+3600,            │
│      tenant_id: "agent_1",          │
│      revoked?: false                │
│    }                                │
│                                     │
│ 5. Store in ETS                     │
│    :ets.insert(:capabilities,       │
│     {hash, cap})                    │
│                                     │
│ 6. Emit event                       │
│    :capability_granted              │
└─────────────────────┬───────────────┘
                      │
                      ▼
                Agent receives token
                (returns only the 
                 plaintext token)
                      │
                      │ 7. Service stores token
                      │
                      ▼
              ┌──────────────┐
              │Service needs │
              │access to res.│
              └────────┬─────┘
                       │
                 8. Calls verify()
                    with token
                       │
                       ▼
        ┌──────────────────────────┐
        │ Capability.Manager       │
        │ verify(token, "eventst",│
        │        "read")          │
        │                          │
        │ 9. Hash provided token   │
        │    hash = SHA256(token)  │
        │                          │
        │ 10. Lookup in ETS        │
        │     cap = ETS[hash]      │
        │                          │
        │ 11. Check expiry         │
        │     T < cap.expires_at?  │
        │                          │
        │ 12. Check permissions    │
        │     "read" in perms?     │
        │                          │
        │ 13. Check revoked        │
        │     cap.revoked? == false│
        └────────┬────────────────┘
                 │
        ┌────────┴─────────┐
        │                  │
        ▼                  ▼
    All checks pass   One check fails
        │                  │
        ▼                  ▼
   Return :ok        Return {:error, reason}
        │                  │
        ▼                  ▼
   Service gets    Service is denied
   access         (event: :capability_denied)
```

---

## 13. Configuration Override Hierarchy

```
1. DEFAULT CONFIGURATION
   (hardcoded in code)
         │
         ▼
2. ENVIRONMENT VARIABLES
   (SOLO_CONFIG=path/to/config.toml)
         │
         ▼
3. CONFIG FILE (TOML)
   (merged on top of defaults)
         │
         ▼
4. RUNTIME UPDATES
   (Application.put_env/3)
         │
         ▼
5. ACTIVE CONFIGURATION
   (used by modules via Application.get_env/2)

Each layer overrides previous ones.
```

---

## 14. Registry Lookup Paths

```
┌─────────────────────────────────────────────┐
│  Solo.Registry (Elixir Registry)            │
│  (:unique keys)                             │
├─────────────────────────────────────────────┤
│                                             │
│ Lookup by exact key:                        │
│ {tenant_id, service_id} ──► [{pid, meta}]  │
│                                             │
│ List all for tenant (via select):           │
│ WHERE key.tenant_id == "agent_1"            │
│ ──► [{service_id_1, pid_1}, ...]            │
│                                             │
│ Lookup by tenant supervisor:                │
│ {:tenant, tenant_id} ──► [{pid, _}]        │
│                                             │
│ Lookup by tenant service supervisor:        │
│ {:tenant_service_supervisor, tenant_id}    │
│ ──► [{pid, _}]                              │
│                                             │
└─────────────────────────────────────────────┘


┌─────────────────────────────────────────────┐
│  Solo.ServiceRegistry (GenServer)           │
│  (in-memory maps)                           │
├─────────────────────────────────────────────┤
│                                             │
│ Three indices:                              │
│                                             │
│ 1. registrations:                           │
│    tenant_id →                              │
│      service_id →                           │
│        registration                         │
│                                             │
│ 2. name_index:                              │
│    service_name →                           │
│      [registration, ...]                    │
│                                             │
│ 3. handle_map:                              │
│    handle → {tenant_id, service_id}         │
│                                             │
│ Example query:                              │
│ discover("agent_1", "http_server", {})     │
│ ──► searches name_index for "http_server"   │
│ ──► filters by tenant_id == "agent_1"      │
│ ──► checks TTL                              │
│ ──► returns list of matching registrations  │
│                                             │
└─────────────────────────────────────────────┘
```

