# Display Subsystem Brainstorm - Summary

## What We Designed

A **3-layer display architecture** that balances sympathy for:
- **Hardware**: All e-ink quirks handled in HAL (power, refresh scheduling, partials)
- **Graphics**: Pure Cairo-based rendering with no hardware leakage
- **Developer**: Simple UI layer that just sends render commands

---

## The Three Layers

### 1. **Display HAL** (`src/display/hal/`)
- **Responsibility**: Direct e-paper hardware control
- **Autonomy**: Intelligent + configurable
- **Key Features**:
  - Auto power management (sleep/wake based on idle time)
  - Refresh orchestration (when to do partial vs full)
  - Partial update queueing + merging
  - Uses your exported EPD_4in2_V2 functions

**Hybrid Configuration**:
```
HAL can be configured with:
- idle_sleep_ms: Auto-sleep after N ms idle
- max_partials_before_full: Force full refresh after N partials
- power_mode: PERFORMANCE | BALANCED | POWER_SAVING
```

### 2. **Display Renderer** (`src/display/renderer/`)
- **Responsibility**: Graphics + frame compositing
- **Engine**: Cairo (2D graphics library)
- **Key Features**:
  - Dynamic layer registry (layers registered at runtime)
  - Layer-based dirty tracking (not pixel-by-pixel)
  - Dual Cairo surfaces: B/W for partials, 4-gray for full refresh
  - Publishes hints to HAL about regions
  - Communicates via ZMQ PUSH/PULL (not function calls)

**Layer Model**:
```
Each layer has:
  - Name + bounds (x, y, w, h)
  - Render function (draws to Cairo context)
  - Dirty flag (marked when layer changes)
  - always_full flag (some layers require full refresh)
```

### 3. **Display App** (`src/display/app/`)
- **Responsibility**: UI orchestration (glue between renderer + sensors)
- **Pattern**: Same as input_app, euc_app, gps_app
- **Key Features**:
  - Listens to sensor events (input, telemetry)
  - Sends render commands to renderer
  - Receives power/refresh state events from HAL

---

## Communication: ZMQ from Day One

### Renderer ↔ HAL (PUSH/PULL)

**Renderer sends**:
```json
{
  "seq": 12345,
  "frame": "<base64-400x300>",
  "regions": [{"x": 0, "y": 0, "w": 400, "h": 30}],
  "is_4gray": false,
  "layers_changed": ["statusbar"],
  "hints": {
    "overlapping": false,
    "suggests_full": false
  }
}
```

**HAL responds with events** (XPUB):
```json
{
  "event": "update_queued",
  "refresh_type": "partial",
  "partial_count": 5,
  "partials_until_full": 10,
  "power_state": "awake"
}
```

**Benefits**:
- Future process isolation (run on separate core/RPi)
- Non-blocking communication
- Testable independently
- Clear contracts

---

## Dirty Region Strategy: Smart & Simple

### How Renderer finds dirty regions:
1. **Tracks layers** with boolean dirty flags (not pixel-by-pixel)
2. **Uses Cairo primitives** for overlap detection if needed
3. **Computes region bounds** from dirty layer geometry
4. **Sends hints to HAL** about the update pattern

### How HAL uses hints:
- Decides if partial or full refresh is better
- Merges overlapping regions intelligently
- Chooses refresh immediately vs queue-and-defer based on heuristics
- Stays **in control** of hardware strategy

---

## Key Design Wins

| Aspect | Solution | Why It Works |
|--------|----------|------------|
| **Hardware Control** | HAL is smart + autonomous | Device-specific complexity hidden, transparent to caller |
| **Graphics** | Pure Cairo + layers | Renderer doesn't understand e-ink quirks |
| **Coupling** | ZMQ messaging | Future process isolation, clean boundaries |
| **Dirty Regions** | Layer-based tracking + hints | Fast computation, HAL gets intelligence data |
| **Power** | Automatic in HAL | Sleep/wake transparent, configurable |
| **UI Layers** | Dynamic registry | App registers layers at runtime, no recompilation |
| **Refresh Modes** | Renderer aware of 4-gray vs B/W | Better visual quality, explicit handling |

---

## File Structure (Ready to Code)

```
src/display/
├── hal/
│   ├── display_hal.h              # Public API
│   ├── display_hal.c              # ~600 LOC core loop
│   ├── display_hal_config.h       # Config types
│   ├── display_hal_power.c        # Power helpers
│   ├── display_hal_refresh.c      # Refresh scheduling
│   ├── display_region.c           # Region merging
│   └── tests/
│
├── renderer/
│   ├── display_renderer.h         # Public API
│   ├── display_renderer.c         # ~500 LOC main loop
│   ├── display_renderer_cairo.c   # Cairo surface management
│   ├── display_renderer_layers.c  # Layer registry
│   ├── display_messages.h         # ZMQ message types
│   ├── display_messages.c         # Serialization
│   └── tests/
│
├── app/
│   ├── display_app.h              # Lifecycle API
│   ├── display_app.c              # ~400 LOC event loop
│   ├── display_main.c             # CLI entry
│   ├── ui_state.h                 # State machine
│   └── tests/
│
└── tests/
    └── test_display_integration.c
```

---

## Next Steps to Implementation

When ready, we'll build in phases:

1. **Phase 1 (HAL)**: Power, refresh scheduling, partial queueing
2. **Phase 2 (Renderer)**: Cairo setup, layer management, ZMQ
3. **Phase 3 (App)**: Event loop, sensor integration
4. **Phase 4 (Testing)**: Integration tests, latency profiling

---

## Open Questions Awaiting Your Input

Before we start coding, help clarify:

1. **Hints sophistication**: Should HAL learn patterns (stateful) or keep hints simple?
2. **Cairo frame rate**: Expected refresh rate? (50ms? 100ms?)
3. **ZMQ topology**: HAL listens on PULL, or more complex routing?
4. **Config**: JSON file or programmatic in display_app?
5. **Robustness**: Reconnection/retry strategy if HAL or Renderer crashes?

---

## Document Artifacts

Two detailed documents in `/var/run/user/1000/`:
- `display_architecture_plan.md` - Original brainstorm (detailed options + tradeoffs)
- `display_architecture_refined.md` - Your feedback incorporated (this is the design to code from)

Both are read-only reference materials. Not committed yet - ready when you say go!

---

**Status**: ✅ Brainstorm complete, architecture solid, ready for implementation phase!

