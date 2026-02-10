# Persistence Analysis for Solo v0.2.0

## Current Persistence State

### ✅ What IS Persisted

1. **Events (EventStore)**
   - Location: `./data/events/` (CubDB)
   - Content: Append-only event log of all system state changes
   - Format: Binary CubDB format
   - Recovery: Events are loaded on startup (if needed for replay)
   - Durability: Disk-backed via CubDB

2. **Secrets (Vault)**
   - Location: `./data/vault/` (CubDB)
   - Content: Encrypted secrets with AES-256-GCM
   - Format: Binary CubDB format with encryption
   - Recovery: Secrets remain accessible after restart
   - Durability: Disk-backed via CubDB

3. **Certificates**
   - Location: `./data/certs/` (PEM files)
   - Content: CA certificates and keys for mTLS
   - Recovery: Loaded on startup for TLS configuration
   - Durability: File-based storage

4. **Configuration**
   - Format: TOML/JSON files
   - Recovery: Loaded from external files on startup
   - Persistence: External (not automatically persisted)

### ❌ What IS NOT Persisted (Critical Gaps)

1. **Deployed Services**
   - State: Only in-memory map in Deployer GenServer
   - Issue: Services lost on restart
   - Impact: All deployed services must be redeployed after crash/restart
   - Current: `state.services = %{tenant_id => %{service_id => pid}}`

2. **Service Metadata**
   - Missing: Service deployment specifications
   - Missing: Service configuration parameters
   - Missing: Service restart policies
   - Missing: Service code/binaries

3. **Service Registry State**
   - State: In-memory only (ServiceRegistry)
   - Issue: Discovered services lost on restart
   - Impact: Service-to-service discovery broken after crash

4. **Capability Tokens**
   - State: In-memory only (Capability.Manager)
   - Issue: Tokens invalidated on restart
   - Impact: Clients must re-authenticate after crash

5. **Load Shedding State**
   - State: In-memory per-tenant limits (LoadShedder)
   - Issue: Rate limit state lost on restart
   - Impact: Burst traffic possible immediately after restart

6. **Hot Swap State**
   - Missing: Code versions/rollback information
   - Missing: Active code versions per service
   - Impact: Cannot verify service state after restart

## Detailed Persistence Gaps

### 1. Service Deployment Persistence (CRITICAL)

**Current Flow:**
```
Deploy -> Compiler -> Start Process -> Register in Deployer (in-memory)
                                    -> Register in Registry (in-memory)
                                    -> Emit Event (persisted)
```

**On Restart:**
- EventStore reads persisted events
- Deployer state is empty (no services recovered)
- Services are gone - must be manually redeployed
- Events exist showing deployments, but no running services

**Missing:**
- Service specification persistence
- Service code artifacts
- Service state recovery mechanism
- Automatic service redeployment

### 2. Service Registry Persistence (IMPORTANT)

**Current:** In-memory map in Solo.ServiceRegistry
```elixir
%{
  tenant_id => %{
    service_id => %ServiceMetadata{...}
  }
}
```

**On Restart:** All service registrations lost

**Missing:**
- Service metadata storage
- Service discovery recovery
- Cross-service dependency information

### 3. Capability Token Persistence (SECURITY)

**Current:** In-memory tree structure in Capability.Manager

**On Restart:**
- All capability tokens invalidated
- Clients must re-authenticate
- Cached authorizations lost

**Missing:**
- Token persistence (if intended)
- Token revocation list persistence
- Token expiration tracking

### 4. Load Shedder State (AVAILABILITY)

**Current:** Per-tenant request counters in LoadShedder

**On Restart:**
- Rate limit windows reset
- Burst of requests immediately after restart

**Missing:**
- Token bucket state persistence
- Rate limit configuration persistence
- Request history for tracking

## What Should Be Persisted

### Tier 1: CRITICAL (Required for Production)

1. **Service Deployment Specifications**
   - tenant_id, service_id, code, format
   - Resource limits, restart policies
   - Deployment timestamp
   - Storage: EventStore or dedicated ServiceStore

2. **Service Running State**
   - Which services should be running
   - Current status per service
   - Last known state information
   - Storage: Durable service registry

### Tier 2: IMPORTANT (Recommended)

3. **Service Registry Metadata**
   - Service discovery information
   - Service interdependencies
   - Endpoint information
   - Storage: CubDB or similar

4. **Configuration State**
   - Per-service configurations
   - Per-tenant configurations
   - Feature flags
   - Storage: TOML/JSON or database

### Tier 3: NICE-TO-HAVE (Optimization)

5. **Capability Token State**
   - Active tokens
   - Token metadata
   - Revocation status
   - Storage: Encrypted CubDB

6. **Load Shedding Metrics**
   - Historical rate limit data
   - Request patterns
   - Storage: Time-series data

## Recovery Mechanisms Needed

### 1. Service Recovery on Startup
```
On System Start:
  1. Load persisted service specs from store
  2. For each spec with "enabled: true":
     - Recompile if needed
     - Start service
     - Register in registry
     - Update status
```

### 2. Event Replay
```
Current: Events are persisted but not used for recovery
Needed: Replay events to reconstruct system state
  - Reduce from initial state
  - Apply each event to state model
  - Output final state
```

### 3. Graceful Shutdown Recovery
```
On Crash:
  1. Detect incomplete transactions
  2. Replay from last safe checkpoint
  3. Reconcile EventStore with actual state
```

## Implementation Recommendations

### Phase 1: Service Specification Persistence
- Store deployment specs in CubDB alongside code
- Add recovery logic to Deployer
- Auto-start marked services on restart
- Estimated effort: 2-3 days

### Phase 2: State Recovery Mechanism
- Build generalized recovery framework
- Use event log for state reconstruction
- Implement reconciliation logic
- Estimated effort: 3-4 days

### Phase 3: Service Registry Durability
- Move ServiceRegistry to CubDB backing
- Add persistence layer
- Implement cache invalidation
- Estimated effort: 1-2 days

### Phase 4: Configuration Persistence
- Store runtime configurations
- Add configuration versioning
- Implement rollback capability
- Estimated effort: 2-3 days

## Testing Gaps

What's NOT tested for persistence:
- [ ] Service survival after crash
- [ ] Service recovery on restart
- [ ] Registry recovery
- [ ] State reconciliation
- [ ] Crash recovery scenarios
- [ ] Graceful shutdown
- [ ] Configuration loading

## Summary

**Current State:** Solo has excellent persistence of *audit data* (EventStore) and *secrets* (Vault), but zero persistence of *operational state* (running services, service registry, capabilities).

**Risk Level:** HIGH - Any crash loses all service deployments

**Priority:** Critical for production use

**Effort to Fix:** 2-3 weeks for complete solution
