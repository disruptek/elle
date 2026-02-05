;;; SDL2 Game Loop Example
;;;
;;; Demonstrates full FFI integration with SDL2 library:
;;; - Window and renderer creation
;;; - Event loop handling
;;; - Graphics rendering (rectangles)
;;; - Input event processing
;;; - Cleanup and resource management
;;;
;;; To run:
;;; (load "examples/sdl2-game.lisp")
;;;
;;; Requirements:
;;; - SDL2 development libraries installed
;;; - libSDL2.so in system library path

;;; Load SDL2 header and auto-generate bindings
(load-header-with-lib "/usr/include/SDL2/SDL.h" 
                       "/usr/lib/libSDL2.so")

;;; Initialize SDL2 with video subsystem
;;; SDL_Init(Uint32 flags)
(sdl-init SDL_INIT_VIDEO)

;;; Create application window
;;; SDL_CreateWindow(const char *title, int x, int y, int w, int h, Uint32 flags)
(let window (sdl-create-window "Elle SDL2 Game" 
                               SDL_WINDOWPOS_CENTERED
                               SDL_WINDOWPOS_CENTERED
                               800 600
                               0))

;;; Create renderer for the window
;;; SDL_CreateRenderer(SDL_Window *window, int index, Uint32 flags)
;;; index: -1 means first available renderer
;;; flags: 0 means no special flags
(let renderer (sdl-create-renderer window -1 0))

;;; Game loop control flag
(let running #t)

;;; Main game loop - runs until quit event
(while running
  ;; Create event structure on the stack
  ;; In a full implementation, we'd define SDL_Event as a C struct
  (let event (make-struct SDL_Event))
  
  ;; Poll for events (non-blocking)
  ;; SDL_PollEvent(SDL_Event *event) returns 1 if event received, 0 otherwise
  (if (sdl-poll-event! event)
    ;; Match event type
    ;; This is a simplified pattern - real code would handle SDL_QUIT, SDL_KEYDOWN, etc.
    (match (struct-get event 'type)
      ;; SDL_QUIT event
      (SDL_QUIT (set! running #f))
      ;; Other events - ignore for now
      (_ nil)))
  
  ;; Clear screen with black color
  ;; SDL_SetRenderDrawColor(SDL_Renderer *renderer, Uint8 r, g, b, a)
  (sdl-set-render-draw-color renderer 0 0 0 255)
  
  ;; Clear renderer (fill with background color)
  ;; SDL_RenderClear(SDL_Renderer *renderer)
  (sdl-render-clear renderer)
  
  ;; Create and configure rectangle for drawing
  ;; In a full implementation, SDL_Rect would be a defined C struct
  (let rect (make-struct SDL_Rect))
  (struct-set! rect 'x 100)
  (struct-set! rect 'y 100)
  (struct-set! rect 'w 100)
  (struct-set! rect 'h 100)
  
  ;; Set render color to red
  (sdl-set-render-draw-color renderer 255 0 0 255)
  
  ;; Draw filled rectangle
  ;; SDL_RenderFillRect(SDL_Renderer *renderer, const SDL_Rect *rect)
  (sdl-render-fill-rect renderer rect)
  
  ;; Present rendered content to screen
  ;; SDL_RenderPresent(SDL_Renderer *renderer)
  (sdl-render-present renderer))

;;; Cleanup: Destroy renderer
;;; SDL_DestroyRenderer(SDL_Renderer *renderer)
(sdl-destroy-renderer renderer)

;;; Cleanup: Destroy window
;;; SDL_DestroyWindow(SDL_Window *window)
(sdl-destroy-window window)

;;; Cleanup: Quit SDL2
;;; SDL_Quit(void)
(sdl-quit)

;;; Notes:
;;; - SDL2 uses event-driven architecture with an event loop
;;; - Window and renderer are C pointers managed by Elle via FFI
;;; - Events are polled on demand (non-blocking)
;;; - Colors are RGBA: (red green blue alpha)
;;; - Coordinates are in window-relative pixels
;;; - All C objects are properly cleaned up in reverse order of creation
;;; - In a real game, you'd add FPS limiting and more complex event handling
