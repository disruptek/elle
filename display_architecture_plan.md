# GoFar Display Subsystem Architecture - Brainstorm & Plan

**Status**: Design phase (read-only exploration)  
**Date**: February 2026  
**Scope**: 4.2" e-paper display driver with partial update + 4-gray support

---

## Executive Summary

The display subsystem has **three distinct layers**:

1. **HAL (Hardware Abstraction Layer)** - Direct e-paper hardware control
   - Manages SPI, power, refresh modes, memory windows
   - Automatic power state management (sleep/wake)
   - Tracks refresh timing (full refresh enforcement)
   - Internal framebuffer + partial update queue

2. **Renderer** - Cairo-based 2D graphics drawing
   - Separate process communicating via ZMQ
   - Receives abstract rendering commands (draw frame, buttons, etc.)
   - Produces image buffers + update directives
   - Publishes when ready for display

3. **App/Business Logic** - Display orchestration  
   - Listens to sensor events (telemetry, input, etc.)
   - Sends rendering requests to Renderer
   - Receives display state events (refresh done, low power)
   - Updates UI as needed

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│  ZMQ Broker (gofar XPUB/XSUB)                                  │
└─────────────────────────────────────────────────────────────────┘
                   ▲                              ▲
                   │                              │
         ┌─────────┴──────────────┬───────────────┴────────┐
         │                        │                        │
    ┌────▼──────┐          ┌──────▼──────┐         ┌───────▼────┐
    │   Input   │          │  Telemetry │         │   Display  │
    │  Handler  │          │   Daemon    │         │    Renderer│
    │ (existing)│          │ (existing)  │         │  (new app) │
    └───────────┘          └─────────────┘         └───────┬────┘
                                                           │ (renders)
                                                           ▼
                                    ┌──────────────────────────────┐
                                    │  display_renderer (daemon)   │
                                    │  - Cairo context             │
                                    │  - Framebuffer (400x300)     │
                                    │  - Region tracking           │
                                    └──────┬───────────────────────┘
                                           │ (sends frames)
                                           ▼
                                    ┌──────────────────────────────┐
                                    │   Display HAL Layer          │
                                    │ (Hardware Abstraction)       │
                                    │ - display_hal_t              │
                                    │ - Power mgmt (auto idle)     │
                                    │ - Refresh tracking           │
                                    │ - Partial update queue       │
                                    │ - Framebuffer + LUT          │
                                    └──────┬───────────────────────┘
                                           │ (SPI commands)
                                           ▼
                                    ┌──────────────────────────────┐
                                    │   e-Paper Hardware           │
                                    │   4.2" 400x300 B/W+Gray      │
                                    │   Partial + 4-gray capable   │
                                    └──────────────────────────────┘
```

---

## Design Principles

### 1. **Separation of Concerns**

- **HAL**: *Sympathy for hardware* - handles all e-ink quirks
  - Power transitions (wake/sleep)
  - Refresh mode selection (partial B/W vs full 4-gray)
  - Memory window tracking
  - Display timing constraints
  - Automatic full refresh enforcement

- **Renderer**: *Sympathy for graphics* - high-level drawing abstraction  
  - Cairo 2D context
  - Layer tracking (cursor, status, content regions)
  - Dirty region computation
  - Image buffer management
  - Protocol with HAL is **clean and simple** (no graphics complexity leaking down)

- **App/UI Logic**: *Sympathy for developer* - no display plumbing knowledge
  - Just publishes render requests ("draw status bar at 100x50")
  - Receives display state events
  - No knowledge of regions, power states, or refresh modes

### 2. **Hardware Sympathy**

The HAL handles the hard parts:

```c
// Power Management (automatic)
- Tracks update frequency
- Auto-sleeps after 30s idle (configurable)
- Auto-wakes on next update (transparent)
- Publishes power state events

// Refresh Enforcement
- Counts partial updates
- Forces full refresh after N partials (e.g., every 10-15 updates)
- Transparent to caller (done between render cycles)

// Partial Update Optimization
- Queues regions if multiple updates arrive before next refresh
- Merges overlapping rectangles
- Falls back to full screen if updates exceed partial limit

// 4-Gray Support
- Full refresh uses 4-gray mode (smoother gradients)
- Partial updates forced to B/W only (hardware limitation)
- Renderer aware of this mode split
```

### 3. **Latency Sympathy**

For cursor movement (high-frequency partial updates):

```
User turns encoder → Input event sent to ZMQ
  ↓ (1-2ms)
Renderer receives event, Cairo draws new cursor position
  ↓ (1-5ms depending on region size)
Renderer sends frame + dirty region to HAL via ZMQ
  ↓ (0.5ms ZMQ roundtrip)
HAL queues partial update, schedules refresh if idle
  ↓ (1-2ms SPI command overhead)
Display updates (100-500ms depending on region size)

TOTAL LATENCY: ~3-10ms from encoder to display reflecting change
(Display refresh is slow, but queue + scheduling is fast)
```

This is **acceptable** for UI - perception is good because:
1. Partial regions update fast
2. Full refresh only happens periodically
3. Low SPI latency before refresh trigger

---

## Layer 1: Display HAL (`src/display/hal/`)

### Purpose
Direct hardware control with e-ink-aware optimizations.

### Public API (`display_hal.h`)

```c
typedef struct display_hal display_hal_t;
typedef struct display_region {
  uint16_t x, y, w, h;  // Pixel bounds
} display_region_t;

// Initialization
display_hal_t *display_hal_init(void);
void display_hal_close(display_hal_t *hal);

// Image update (main entry point)
// - Caller provides full 400x300 framebuffer
// - Specifies which regions changed
// - HAL decides partial vs full refresh
int display_hal_update_regions(
  display_hal_t *hal,
  const uint8_t *image_buf,           // Full 400x300 image
  size_t image_size,
  const display_region_t *regions,    // Dirty regions
  size_t num_regions,
  bool is_4gray                        // Full refresh uses 4-gray, partials are B/W
);

// State queries
typedef enum {
  DISPLAY_STATE_AWAKE,
  DISPLAY_STATE_SLEEPING,
  DISPLAY_STATE_REFRESHING
} display_state_t;

display_state_t display_hal_get_state(display_hal_t *hal);
int display_hal_get_stats(display_hal_t *hal, display_stats_t *out);

// Configuration
typedef struct {
  uint32_t idle_sleep_ms;        // Sleep after N ms idle (0 = never)
  uint32_t full_refresh_interval; // Full refresh after N partial updates
  bool auto_power_management;     // Enable auto sleep/wake
} display_hal_config_t;

int display_hal_configure(display_hal_t *hal, const display_hal_config_t *cfg);
```

### Internal Implementation (`display_hal.c`)

```c
struct display_hal {
  // Hardware state
  int spi_fd;
  uint8_t *framebuffer;       // 400x300 pixels (15KB)
  uint8_t *lut_buffer;        // 4-gray LUT buffer
  
  // Power management
  time_t last_update_time;
  display_state_t current_state;
  uint32_t idle_sleep_ms;
  bool auto_power_enabled;
  
  // Refresh tracking
  uint32_t partial_count;     // Since last full refresh
  uint32_t full_refresh_interval;
  
  // Partial update queue
  display_region_t *pending_regions;
  size_t pending_count;
  
  // Statistics
  uint64_t updates_partial;
  uint64_t updates_full;
  uint64_t auto_sleeps;
};
```

### Key Functions

```c
// Internal power management
static int display_hal_wake(display_hal_t *hal);
static int display_hal_sleep(display_hal_t *hal);
static void display_hal_check_idle(display_hal_t *hal);

// Internal refresh orchestration
static int display_hal_refresh_partial(display_hal_t *hal, const display_region_t *r);
static int display_hal_refresh_full(display_hal_t *hal);

// Merge adjacent regions
static void display_hal_merge_regions(display_region_t *regions, size_t *count);

// Using exported e-Paper functions
// (your modified EPD_4in2_V2.c exports these):
void EPD_4IN2_V2_SetWindows(UWORD Xstart, UWORD Ystart, UWORD Xend, UWORD Yend);
void EPD_4IN2_V2_TurnOnDisplay_Partial(void);
void EPD_4IN2_V2_TurnOnDisplay_4Gray(void);
void EPD_4IN2_V2_PartialDisplay(UBYTE *Image, UWORD x, UWORD y, UWORD w, UWORD l);
```

### Constraints Handled

- **Only B/W for partials**: HAL dithers 4-gray to B/W when doing partial
- **Full refresh every N**: Transparent periodic full refresh between partial updates
- **Power efficiency**: Auto-sleep on idle, wake on first update
- **Low SPI latency**: Queue regions, batch if possible

---

## Layer 2: Display Renderer (`src/display/renderer/`)

### Purpose
Cairo-based 2D graphics + frame compositing. Separate daemon process.

### Architecture

```c
struct display_renderer {
  // Cairo context
  cairo_surface_t *surface;     // 400x300 pixels
  cairo_t *cr;                  // Drawing context
  
  // Framebuffer
  uint8_t *current_frame;       // What's on-screen
  uint8_t *dirty_frame;         // What we're drawing
  
  // Region tracking
  display_layer_t layers[16];   // Named regions (cursor, status, etc.)
  display_region_t *dirty_regions;
  size_t dirty_count;
  
  // ZMQ
  void *zmq_ctx;
  void *zmq_sub;                // Subscribe to render requests
  void *zmq_pub;                // Publish render complete events
  void *hal_req;                // Request/Reply with HAL (or just direct)
};

typedef struct {
  const char *name;             // e.g., "cursor", "statusbar"
  uint16_t x, y, w, h;          // Fixed bounds
  bool always_full;             // Layer requires full refresh (not partial)
} display_layer_t;
```

### Renderer Process

```
Main Loop:
  1. Poll ZMQ for render request (timeout 50ms)
  2. If request received:
     - Parse command ("draw_status_bar", "draw_cursor", etc.)
     - Use Cairo to draw into dirty_frame
     - Mark affected regions as dirty
  3. Compare dirty_frame vs current_frame
  4. Compute dirty regions (or use hints from request)
  5. If regions changed:
     - Send to display HAL (via ZMQ or direct call)
     - Wait for HAL ack
     - Publish "frame_updated" event
     - Swap current_frame ← dirty_frame
  6. If full refresh completed:
     - Listen for HAL "full_refresh_done" event
     - Update internal 4-gray buffer
```

### Public API (`display_renderer.h`)

```c
typedef struct display_renderer display_renderer_t;

// Lifecycle
display_renderer_t *display_renderer_init(const char *zmq_endpoint);
int display_renderer_run(display_renderer_t *renderer);
void display_renderer_close(display_renderer_t *renderer);

// Command structure (serialized as JSON via ZMQ)
typedef struct {
  const char *command;          // "draw_frame", "draw_text", etc.
  const char *layer;            // Optional: "cursor", "statusbar"
  // ... command-specific fields
  // Parsed from JSON by renderer
} display_command_t;
```

### Example Commands (ZMQ JSON protocol)

```json
// Draw status bar
{
  "cmd": "draw_statusbar",
  "layer": "statusbar",
  "speed_kph": 25.5,
  "voltage_mv": 3800,
  "temp_c": 45.2,
  "mode": "cruise"
}

// Draw cursor at new position
{
  "cmd": "draw_cursor",
  "layer": "cursor",
  "x": 150,
  "y": 100,
  "style": "circle"
}

// Full frame update
{
  "cmd": "full_frame",
  "image_base64": "iVBORw0KGg...",  // or raw bytes
  "force_4gray": true
}

// Clear layer
{
  "cmd": "clear_layer",
  "layer": "cursor"
}
```

### Renderer ↔ HAL Interface

**Option A: Direct Function Calls** (if renderer is linked with HAL)
```c
display_hal_update_regions(hal, frame, size, regions, count, is_4gray);
```

**Option B: ZMQ Messaging** (if separate processes)
```
Renderer PUSH/REP to HAL Request socket:
  { "frame": base64(...), "regions": [...], "is_4gray": true }
HAL replies:
  { "status": "ok", "partial_count": 5 }
```

We'll start with **Option A** (direct) for simplicity, can evolve to Option B later.

---

## Layer 3: Display App (`src/display/app/`)

### Purpose
Orchestrate display updates from sensor events. High-level logic only.

### Pattern: Same as Input/EUC/GPS Daemons

**Structure**:
```
src/display/
├── hal/
│   ├── display_hal.h
│   ├── display_hal.c
│   └── tests/
├── renderer/
│   ├── display_renderer.h
│   ├── display_renderer.c
│   ├── messages/
│   │   └── display_messages.h
│   └── tests/
├── app/
│   ├── display_app.h
│   ├── display_app.c
│   ├── display_main.c
│   └── tests/
└── tests/
    └── test_display_integration.c
```

### Display App Config

```c
typedef struct {
  const char *i2c_device;           // "/dev/i2c-1" (for potential future hw)
  const char *zmq_pub_endpoint;     // Where renderer listens
  const char *zmq_sub_endpoint;     // Where app publishes render requests
  uint32_t update_interval_ms;      // Typical UI refresh rate (100ms?)
  bool verbose;
} display_app_config_t;
```

### Display App: Listen & Render

```c
// Main loop (simplified):
while (!g_shutdown) {
  // Listen to sensor events (telemetry, input)
  input_event_t input_event = receive_from_zmq();
  euc_telemetry_t telem = receive_from_zmq();
  
  // Decide what to render
  if (input_event.encoder_moved) {
    cursor_x += input_event.delta;
    send_render_command("draw_cursor", cursor_x, cursor_y);
  }
  
  if (telem.speed_changed) {
    send_render_command("draw_statusbar", telem);
  }
  
  // Renderer handles HAL communication
  // App just sends high-level commands
}
```

---

## Design Deep-Dives

### Issue 1: Renderer ↔ HAL Interface (THE KEY QUESTION)

**The Challenge**: How does Renderer know when to use partial vs full refresh? When to batch regions?

**Option A: HAL Decides Internally**
```c
// Renderer just sends every frame update
display_hal_update_regions(hal, frame, regions, region_count, is_4gray);

// HAL is smart:
// - Queues regions
// - Decides if partial or full based on count
// - Handles power management
// - Transparent to caller
```
**Pros**: Simplest renderer, HAL is powerful  
**Cons**: HAL bears all the complexity

**Option B: Renderer Coordinates**
```c
// Renderer asks HAL for advice
display_state_t state = display_hal_query_preferred_mode(hal, regions, count);
if (state == PREFER_FULL_REFRESH) {
  // Draw full 4-gray frame
  display_hal_update_regions(hal, frame, NULL, 0, true);
} else {
  // Send partial B/W updates
  display_hal_update_regions(hal, frame, regions, count, false);
}
```
**Pros**: Explicit, renderer has control  
**Cons**: Renderer needs display knowledge

**RECOMMENDATION**: **Option A** (HAL decides).  
Reasoning:
- Keeps renderer focus on *drawing*, not *scheduling*
- HAL can optimize based on hardware timing (e.g., "device is busy, queue this")
- Simpler renderer → easier to test + evolve
- Matches pattern of other daemons (input_hal, euc_hal)

---

### Issue 2: 4-Gray vs B/W Rendering

**Hardware Reality**: 
- Full refresh can use 4-gray (looks nice)
- Partial updates **only support B/W** (hardware limit)

**Options**:

**Option A: Renderer manages mode**
```c
// Renderer has two surfaces:
cairo_surface_t *surface_bw;      // B/W for partials
cairo_surface_t *surface_4gray;   // 4-gray for full

// When sending update:
if (is_full_refresh) {
  // Draw to 4gray surface, convert grayscale properly
  send_4gray_frame();
} else {
  // Draw to B/W surface, dither/posterize
  send_bw_frame();
}
```

**Option B: HAL dithers on demand**
```c
// Renderer always renders in 4-gray
// HAL has dither routine:
static uint8_t dither_4gray_to_bw(uint8_t gray) {
  return gray > 127 ? 0xFF : 0x00;
}
// Applied when doing partial
```

**RECOMMENDATION**: **Option A** (Renderer is aware).  
Reasoning:
- Better visual quality (renderer can optimize B/W dither)
- Cairo can render two modes if needed
- HAL stays simple (just updates, doesn't transform)
- Explicit > implicit

---

### Issue 3: Region Tracking & Dirty Rectangle Computation

**Problem**: Renderer needs to know which pixels changed to optimize partial updates.

**Options**:

**Option A: Explicit dirty regions from caller**
```c
// App sends render commands with region hints:
{
  "cmd": "draw_cursor",
  "layer": "cursor",
  "x": 150, "y": 100,
  "width": 20, "height": 20   // ← renderer knows exact bounds
}

// Renderer uses this to mark dirty region
```

**Option B: Automatic dirty tracking**
```c
// Renderer compares prev_frame vs new_frame pixel-by-pixel
// Computes bounding rect of changed pixels
// But this is expensive for full frame
```

**Option C: Layer-based tracking**
```c
// Renderer has fixed layers:
typedef struct {
  const char *name;
  uint16_t x, y, w, h;
  bool dirty;
} display_layer_t;

// When command specifies layer, mark it dirty
// Send union of dirty layer bounds
```

**RECOMMENDATION**: **Option C** (Layers) with **Option A** fallback.  
Architecture:
```c
#define NUM_LAYERS 8

display_layer_t layers[] = {
  { "statusbar",  0,   0, 400,  30, false },
  { "content",    0,  30, 400, 240, false },
  { "cursor",   100, 100,  20,  20, false },
  { "notifications", 300, 10, 100, 20, false },
  // ...
};

// When renderer draws:
draw_statusbar_to_layer("statusbar");
layers[LAYER_STATUSBAR].dirty = true;

// Compute regions:
display_region_t regions[NUM_LAYERS];
for (int i = 0; i < NUM_LAYERS; i++) {
  if (layers[i].dirty) {
    regions[region_count++] = {
      layers[i].x, layers[i].y, layers[i].w, layers[i].h
    };
  }
}

// Send to HAL
display_hal_update_regions(hal, frame, regions, region_count, is_4gray);
```

This gives:
- **Fast layer invalidation** (not pixel-by-pixel)
- **Low overhead** (just boolean flags)
- **Good partial update bounds** (region = layer union)
- **Simple semantics** for caller

---

### Issue 4: ZMQ Protocol (Renderer ↔ App)

**Pattern**: Follows gofar conventions

```
App → Renderer:
  XPUB endpoint: ipc:///tmp/gofar_display_render
  Topic: "cmd"
  Message: JSON render command
  
Renderer → App:
  XPUB endpoint: ipc:///tmp/gofar_display_events
  Topic: "frame_updated" | "refresh_done" | "power_state"
  Message: JSON event
```

**Example Flow**:

```json
// App publishes render request
["cmd", {
  "cmd": "draw_status",
  "voltage_mv": 3800,
  "speed_kph": 25.5
}]

// Renderer processes, draws, sends to HAL
// Renderer publishes event
["frame_updated", {
  "timestamp_us": 1707123456789,
  "partial_count": 3,
  "next_full_refresh_in": 7
}]

// HAL publishes when full refresh done
["refresh_done", {
  "timestamp_us": 1707123456800,
  "duration_ms": 2500
}]
```

---

## Implementation Roadmap

### Phase 1: HAL Foundation (Week 1)
- [ ] `src/display/hal/display_hal.h` - Public API
- [ ] `src/display/hal/display_hal.c` - Core implementation
  - [ ] Power management (wake/sleep timers)
  - [ ] Partial update queuing
  - [ ] Full refresh enforcement
  - [ ] Region merging
- [ ] Unit tests (mock e-Paper functions)
- [ ] Use your modified `EPD_4in2_V2.c` exports

### Phase 2: Renderer Foundation (Week 2)
- [ ] `src/display/renderer/display_renderer.h` - Public API
- [ ] `src/display/renderer/display_renderer.c` - Core loop
  - [ ] Cairo surface setup (B/W + 4-gray)
  - [ ] Layer management
  - [ ] ZMQ pub/sub
  - [ ] Frame compositing
- [ ] Message protocol (`src/display/messages/display_messages.h`)
- [ ] Integration tests (HAL + Renderer)

### Phase 3: App & CLI (Week 3)
- [ ] `src/display/app/display_app.h` - Lifecycle API
- [ ] `src/display/app/display_app.c` - Event loop
- [ ] `src/display/app/display_main.c` - CLI entry point
- [ ] Integration with other subsystems (input, telemetry)

### Phase 4: Testing & Optimization (Week 4)
- [ ] End-to-end tests (all three layers)
- [ ] Latency profiling (cursor updates)
- [ ] Power profile measurement
- [ ] Documentation

---

## File Structure Summary

```
src/display/
├── hal/
│   ├── display_hal.h              # Public API
│   ├── display_hal.c              # Implementation (500-600 LOC)
│   └── tests/
│       ├── test_display_hal.c     # Unit tests
│       └── test_display_power.c   # Power management tests
├── renderer/
│   ├── display_renderer.h         # Public API
│   ├── display_renderer.c         # Cairo integration (400-500 LOC)
│   ├── display_renderer_cairo.c   # Cairo-specific helpers
│   ├── messages/
│   │   └── display_messages.h     # ZMQ message protocol
│   └── tests/
│       ├── test_renderer.c        # Unit tests
│       └── test_renderer_cairo.c  # Cairo integration tests
├── app/
│   ├── display_app.h              # Lifecycle API
│   ├── display_app.c              # Event loop (300-400 LOC)
│   ├── display_main.c             # CLI entry point
│   └── tests/
│       └── test_display_app.c     # Integration tests
└── tests/
    ├── test_display_integration.c # Full stack test
    └── fixtures/
        └── test_images.h          # Cairo test patterns
```

---

## Key Design Decisions (Summary)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Graphics Library | Cairo | Mature, excellent abstraction, you liked PoC |
| Layer Architecture | 3-layer (HAL/Renderer/App) | Clear separation, each layer testable |
| Power Management | Automatic in HAL | Transparent, efficient, hardware-aware |
| Refresh Scheduling | HAL decides | Renderer doesn't need display knowledge |
| Partial Updates | Explicit regions + layer hints | Balance between control and simplicity |
| Region Tracking | Layer-based (8 fixed regions) | Fast, low overhead, good for fixed UI layouts |
| ZMQ Protocol | PUB/SUB (commands + events) | Async, decoupled, non-blocking |
| 4-Gray Handling | Renderer aware (two Cairo surfaces) | Better quality, renderer controls dithering |
| SPI Access | Direct via HAL (no separate SPI daemon) | Simpler architecture, adequate latency |

---

## Open Questions for User

1. **Frame compositing**: Should renderer always work in full 400x300, or can it support partial Cairo contexts per-layer for efficiency?

2. **Cairo library**: Is `libcairo` already available on RPi target? Should we assume it's installed?

3. **Blocking behavior**: Should `display_hal_update_regions()` block until refresh is queued, or return immediately? (Affects API design)

4. **Partial update limits**: What's the maximum number of partial updates before enforcing full refresh? (e.g., 10, 15, 20?)

5. **Layer count**: Is 8 fixed layers reasonable for the UI, or should we make it dynamic/configurable?

6. **Idle sleep timeout**: Suggested defaults? (e.g., 30s idle → sleep, wake on next update?)

---

**BRAINSTORM COMPLETE**. Ready to refine and start implementation!

