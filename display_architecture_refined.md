# GoFar Display Subsystem - Refined Architecture

**Status**: Design phase - refined based on user feedback  
**Last Updated**: February 2026

---

## Key Refinements from Brainstorm

### 1. Renderer → HAL Communication: ZMQ from Start
- **Decision**: Use ZMQ messaging for Renderer ↔ HAL
- **Rationale**: 
  - Enables future process isolation (each on separate core/node)
  - Cleaner API contract between layers
  - Both layers can be tested independently
  - Non-blocking communication fits hardware timing constraints

**Messaging Pattern**:
```
Renderer → HAL (PUSH):
  {
    "frame": "<base64-encoded-400x300-image>",
    "regions": [
      {"x": 0, "y": 0, "w": 400, "h": 30},  // statusbar
      {"x": 100, "y": 100, "w": 20, "h": 20}  // cursor
    ],
    "is_4gray": false,  // true for full refresh, false for partial B/W
    "timestamp_ns": 1707123456789
  }

HAL → Renderer (status events via XPUB):
  {
    "event": "update_queued",
    "partial_count": 5,
    "next_full_refresh_in": 10,  // updates remaining before forced full
    "power_state": "awake"
  }
  
  OR
  
  {
    "event": "refresh_complete",
    "refresh_type": "partial",  // or "full"
    "duration_ms": 500,
    "power_state": "awake"  // or "entering_sleep"
  }
```

### 2. Dirty Region Discovery: Cairo Primitives + HAL Hints
- **Decision**: Renderer uses Cairo to track regions, provides hints to HAL
- **Approach**:
  
```
Renderer Layer Model:
  typedef struct {
    const char *name;           // "cursor", "statusbar", etc.
    uint16_t x, y, w, h;        // Layer bounds
    cairo_surface_t *surface;   // Per-layer Cairo surface (optional)
    bool dirty;                 // Mark dirty when layer changes
    bool always_full;           // Some layers force full refresh
  } display_layer_t;

Renderer tracks:
  1. Which layers changed (boolean flags)
  2. Cairo region/path intersection queries for overlap detection
  3. Passes both to HAL as hints in the ZMQ message:
     {
       "frame": "...",
       "regions": [...],
       "hints": {
         "num_layers_changed": 2,
         "suggests_full_refresh": false,
         "has_overlapping_regions": true
       }
     }

HAL uses hints to:
  - Decide if partial update is worth it vs full refresh
  - Merge overlapping regions more intelligently
  - Learn pattern of updates (e.g., cursor moves frequently → don't batch)
```

**Why This Works**:
- Renderer computes dirties at graphics layer (fast, accurate)
- HAL still has final say on refresh strategy (maintains control)
- HAL can use hints for heuristics ("If hints say full refresh would be better, and we're near due date, do it now")
- Clear separation: Renderer = graphics, HAL = hardware strategy

### 3. HAL: Smart + Configurable (Hybrid)
- **Decision**: HAL is intelligent BUT accepts configuration hints
- **Architecture**:

```c
typedef struct {
  uint32_t idle_sleep_ms;              // Sleep after N ms idle
  uint32_t full_refresh_interval_ms;   // OR force full after time
  uint32_t max_partials_before_full;   // OR force full after N partials
  bool accept_renderer_hints;          // Listen to renderer suggestions
  display_power_mode_t power_mode;     // PERFORMANCE | BALANCED | POWER_SAVING
} display_hal_config_t;

enum display_power_mode_t {
  PERFORMANCE,      // Favor responsiveness over power
  BALANCED,         // Default
  POWER_SAVING      // Minimize power consumption
};
```

**HAL Decision Logic**:
```
When renderer sends update:
  1. Queue the update
  2. Check if should refresh now:
     - Are we idle? (could sleep after)
     - Are we approaching full_refresh_interval?
     - Are we at max_partials?
     - Do hints suggest full refresh would be cleaner?
  3. If YES to any above:
     - Schedule refresh (partial or full based on heuristics)
     - Issue SPI commands
  4. If NO:
     - Queue region, return
  5. Check power state:
     - If idle_time > threshold, schedule sleep
     - Publish event with next_full_refresh_in counter
```

### 4. Display Layers: Dynamic Registry
- **Decision**: Layers registered at runtime (not fixed array)
- **API**:

```c
// In display_renderer.h
typedef void (*layer_render_fn_t)(
  cairo_t *cr,
  void *layer_data,
  const display_layer_t *layer_info
);

typedef struct {
  const char *name;
  uint16_t x, y, w, h;
  layer_render_fn_t render_fn;
  void *render_data;
  bool always_full;
} display_layer_t;

// Lifecycle
int display_renderer_register_layer(
  display_renderer_t *renderer,
  const display_layer_t *layer
);

int display_renderer_unregister_layer(
  display_renderer_t *renderer,
  const char *layer_name
);

int display_renderer_mark_layer_dirty(
  display_renderer_t *renderer,
  const char *layer_name
);

// Query layers
const display_layer_t *display_renderer_get_layer(
  display_renderer_t *renderer,
  const char *layer_name
);

int display_renderer_list_layers(
  display_renderer_t *renderer,
  const display_layer_t **out_layers,
  size_t *out_count
);
```

**Example Usage**:
```c
// App registers UI layers on startup
display_layer_t statusbar = {
  .name = "statusbar",
  .x = 0, .y = 0, .w = 400, .h = 30,
  .render_fn = render_statusbar,
  .render_data = &ui_state,
  .always_full = false
};
display_renderer_register_layer(renderer, &statusbar);

display_layer_t cursor = {
  .name = "cursor",
  .x = 100, .y = 100, .w = 20, .h = 20,
  .render_fn = render_cursor,
  .render_data = &cursor_state,
  .always_full = false
};
display_renderer_register_layer(renderer, &cursor);
```

---

## Refined Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│  ZMQ Message Broker (gofar XPUB/XSUB)                      │
│  - XSUB for inbound commands                               │
│  - XPUB for outbound events                                │
└──────────────────┬──────────────────────────────────────────┘
                   │
        ┌──────────┼──────────────┬─────────────────┐
        │          │              │                 │
    ┌───▼──┐   ┌───▼──┐     ┌────▼──────┐    ┌────▼──────┐
    │Input │   │ Telem│     │  Renderer  │    │Display Events
    │Events│   │Events│     │ (PUB/PUSH) │    │ (XPUB)
    └──────┘   └──────┘     └────┬───────┘    └───────────┘
                                 │
                                 │ PUSH/PULL (frames + regions)
                                 │
                            ┌────▼──────────────┐
                            │ display_hal.c     │
                            │ (listening PULL)  │
                            │ - Power mgmt      │
                            │ - Refresh sched   │
                            │ - Partial queue   │
                            └────┬──────────────┘
                                 │ SPI Commands
                                 │
                            ┌────▼──────────┐
                            │ e-Paper HW    │
                            │ EPD_4in2_V2   │
                            └───────────────┘
```

---

## Updated Message Protocols

### Renderer → HAL (PUSH/PULL)

**Request** (Renderer PUSH):
```json
{
  "seq": 12345,                    // Sequence for matching replies
  "frame": "<base64-frame>",       // 400x300 pixels
  "regions": [
    {"x": 0, "y": 0, "w": 400, "h": 30},      // statusbar
    {"x": 150, "y": 120, "w": 20, "h": 20}    // cursor
  ],
  "is_4gray": false,               // false=partial B/W, true=full 4-gray
  "layers_changed": ["statusbar", "cursor"],
  "hints": {
    "num_regions": 2,
    "total_pixels": 1200,
    "suggests_merge": true,
    "overlapping": false
  },
  "timestamp_ns": 1707123456789000
}
```

**Response** (HAL PUSH back or via XPUB event):
```json
{
  "event": "update_queued",
  "seq": 12345,                    // Match request
  "refresh_type": "queued_partial",// pending_partial | pending_full
  "partial_count": 5,
  "partials_until_full": 10,
  "power_state": "awake",
  "estimated_refresh_time_ms": 150
}
```

### HAL → Renderer/App (XPUB events)

**When refresh completes**:
```json
{
  "event": "refresh_complete",
  "refresh_type": "partial",       // "partial" | "full"
  "region_count": 2,
  "duration_ms": 250,
  "power_state": "awake",
  "total_partial_count": 5
}
```

**When entering/exiting sleep**:
```json
{
  "event": "power_state_change",
  "from": "awake",
  "to": "sleeping",
  "reason": "idle_timeout",
  "wakeup_triggers": ["any_update"]
}
```

**Stats published periodically**:
```json
{
  "event": "stats",
  "uptime_sec": 3600,
  "total_updates": 450,
  "partial_refreshes": 400,
  "full_refreshes": 50,
  "auto_sleeps": 12,
  "power_state": "awake",
  "next_forced_full_in": 8
}
```

---

## Updated File Structure

```
src/display/
├── hal/
│   ├── display_hal.h              # Public API (ZMQ-based)
│   ├── display_hal.c              # Core implementation
│   ├── display_hal_config.h       # Configuration types
│   ├── display_hal_power.c        # Power management helpers
│   ├── display_hal_refresh.c      # Refresh scheduling helpers
│   ├── display_region.c           # Region merging/computation
│   └── tests/
│       ├── test_display_hal.c
│       ├── test_display_power.c
│       ├── test_display_refresh.c
│       └── test_region_merge.c
│
├── renderer/
│   ├── display_renderer.h         # Public API
│   ├── display_renderer.c         # Main loop + layer mgmt
│   ├── display_renderer_cairo.c   # Cairo surface management
│   ├── display_renderer_layers.c  # Layer registry + tracking
│   ├── display_messages.h         # ZMQ message types (JSON)
│   ├── display_messages.c         # Message serialization
│   └── tests/
│       ├── test_renderer.c
│       ├── test_renderer_layers.c
│       ├── test_renderer_cairo.c
│       └── test_renderer_zmq.c
│
├── app/
│   ├── display_app.h              # Lifecycle API
│   ├── display_app.c              # Event orchestration
│   ├── display_main.c             # CLI entry point
│   ├── ui_state.h                 # UI state machine
│   └── tests/
│       └── test_display_app.c
│
└── tests/
    ├── test_display_integration.c
    └── fixtures/
        └── test_patterns.h        # Cairo test renderers
```

---

## API Changes from Original Plan

### display_hal.h

```c
#ifndef DISPLAY_HAL_H
#define DISPLAY_HAL_H

#include <stdint.h>
#include <stdbool.h>
#include "display_hal_config.h"

typedef struct display_hal display_hal_t;

// Initialization (now takes ZMQ endpoint)
display_hal_t *display_hal_init(
  const char *zmq_endpoint  // e.g., "ipc:///tmp/gofar_hal_data"
);

void display_hal_close(display_hal_t *hal);

// Configuration (before running)
int display_hal_configure(
  display_hal_t *hal,
  const display_hal_config_t *config
);

// Main loop (blocking)
int display_hal_run(display_hal_t *hal);

// Request graceful shutdown
int display_hal_request_stop(display_hal_t *hal);

// State queries
display_state_t display_hal_get_state(display_hal_t *hal);
int display_hal_get_stats(display_hal_t *hal, display_stats_t *out);

#endif
```

### display_renderer.h

```c
#ifndef DISPLAY_RENDERER_H
#define DISPLAY_RENDERER_H

#include <stdint.h>
#include <stdbool.h>
#include <cairo.h>

typedef struct display_renderer display_renderer_t;
typedef struct display_layer_t display_layer_t;

// Render function signature
typedef void (*layer_render_fn_t)(
  cairo_t *cr,
  void *layer_data,
  const display_layer_t *layer_info
);

typedef struct display_layer_t {
  const char *name;
  uint16_t x, y, w, h;
  layer_render_fn_t render_fn;
  void *render_data;
  bool always_full;
} display_layer_t;

// Initialization
display_renderer_t *display_renderer_init(
  const char *zmq_pub_endpoint,   // Where I receive render commands
  const char *zmq_sub_endpoint,   // Where I send status/events
  const char *hal_endpoint        // Where HAL is listening (PULL)
);

void display_renderer_close(display_renderer_t *renderer);

// Layer management
int display_renderer_register_layer(
  display_renderer_t *renderer,
  const display_layer_t *layer
);

int display_renderer_unregister_layer(
  display_renderer_t *renderer,
  const char *layer_name
);

int display_renderer_mark_layer_dirty(
  display_renderer_t *renderer,
  const char *layer_name
);

const display_layer_t *display_renderer_get_layer(
  display_renderer_t *renderer,
  const char *layer_name
);

// Main loop
int display_renderer_run(display_renderer_t *renderer);
int display_renderer_request_stop(display_renderer_t *renderer);

// Queries
int display_renderer_get_stats(
  display_renderer_t *renderer,
  display_renderer_stats_t *out
);

#endif
```

---

## Communication Flow: End-to-End Example

### Scenario: Input encoder rotates (cursor moves)

```
1. Input Hardware Event:
   Encoder moved 5 clicks
   
2. Input Daemon → ZMQ:
   {
     "type": "encoder",
     "delta": 5,
     "timestamp_ns": 1707123456789
   }

3. Renderer receives on SUB socket:
   - Parses message
   - Updates cursor_x += 5
   - Marks "cursor" layer dirty
   
4. Renderer's main loop:
   - Detects cursor layer dirty
   - Computes dirty bounds: (cursor_x, cursor_y, 20, 20)
   - Redraws cursor to Cairo surface
   - Compares with previous frame
   - Builds ZMQ message with regions
   
5. Renderer → HAL (PUSH):
   {
     "seq": 12345,
     "frame": "<base64>",
     "regions": [{"x": cursor_x, "y": cursor_y, "w": 20, "h": 20}],
     "is_4gray": false,
     "layers_changed": ["cursor"],
     "hints": {"overlapping": false, "suggests_merge": false}
   }

6. HAL receives (PULL):
   - Queues the region
   - Checks: is partial_count < max_partials? Yes (5 < 15)
   - Schedules partial refresh immediately (cursor = high priority)
   - Issues SPI commands to display
   - Publishes via XPUB:
   
   {
     "event": "update_queued",
     "seq": 12345,
     "refresh_type": "partial",
     "partial_count": 6,
     "partials_until_full": 9,
     "power_state": "awake"
   }

7. Display updates physically (100-300ms)
   User sees cursor move

8. HAL publishes when done:
   {
     "event": "refresh_complete",
     "refresh_type": "partial",
     "duration_ms": 150,
     "power_state": "awake",
     "total_partial_count": 6
   }

9. Renderer receives event:
   - Updates internal state
   - Publishes to app/UI layer if needed
```

### Latency Breakdown:
```
Input event → ZMQ: 1ms
Renderer processes: 2-5ms
Renderer → HAL (ZMQ): 0.5ms
HAL queues + schedules: 1ms
SPI commands: 1-2ms
Display refresh (partial, small region): 150-300ms
─────────────────────────────────────
Perceived latency: 3-10ms (user sees change quickly)
Total refresh time: 150-300ms (acceptable for UI)
```

---

## Key Wins with This Design

1. **Clear Contracts**: 
   - Renderer talks ZMQ → HAL (testable separately)
   - HAL never calls back to Renderer (no re-entrancy)
   - Each can evolve independently

2. **Smart HAL**:
   - Handles all e-ink complexity (power, refresh scheduling)
   - Listens to Renderer hints but makes own decisions
   - Configurable behavior for different power/performance tradeoffs

3. **Renderer Focus**:
   - Pure graphics + Cairo drawing
   - Tracks dirty regions via layers
   - Doesn't need to understand e-ink hardware

4. **Dynamic Flexibility**:
   - App can register/unregister layers at runtime
   - Custom render functions for each layer
   - Can add new UI sections without changing renderer

5. **Future-Proof**:
   - ZMQ allows process separation later
   - Can add new subscribers to HAL events (monitoring, logging)
   - Can scale to multiple displays with single renderer

---

## Open Implementation Questions

Before we start coding, clarify:

1. **Hint Sophistication**: 
   - Should HAL learn patterns? (e.g., "cursor updates are frequent, don't batch them")
   - Or keep hints simple/stateless?

2. **Cairo Performance**:
   - Redraw full layer each frame, or track dirty rects within layer?
   - Expected frame rate? (50ms? 100ms?)

3. **ZMQ Socket Topology**:
   - HAL runs PULL socket waiting for frames?
   - Or should it be more like a pipeline (HAL as broker)?

4. **Config File**:
   - Should HAL config be loaded from JSON/YAML on startup?
   - Or programmatically set by display_app?

5. **Error Handling**:
   - If HAL crashes, should Renderer retry/reconnect?
   - Vice versa?

---

**REFINED PLAN COMPLETE**. Ready for implementation phase!

