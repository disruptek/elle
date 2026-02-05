;;; Auto-generated Elle bindings from SDL2 header
;;;  Library: SDL2

;;; Constants

(define SDL_INIT_VIDEO 4)
(define SDL_INIT_AUDIO 16)
(define SDL_INIT_TIMER 1)
(define SDL_WINDOWPOS_CENTERED 805240832)

;;; Function bindings

(define (sdl-init arg0)
  (call-c-function "SDL2" "SDL_Init" (:uint) :int
    arg0))

(define (sdl-quit )
  (call-c-function "SDL2" "SDL_Quit" () :void
    ))

(define (sdl-create-window arg0 arg1 arg2 arg3 arg4 arg5)
  (call-c-function "SDL2" "SDL_CreateWindow" (:string :int :int :int :int :uint) :pointer
    arg0 arg1 arg2 arg3 arg4 arg5))

(define (sdl-destroy-window arg0)
  (call-c-function "SDL2" "SDL_DestroyWindow" (:pointer) :void
    arg0))

(define (sdl-get-window-surface arg0)
  (call-c-function "SDL2" "SDL_GetWindowSurface" (:pointer) :pointer
    arg0))

(define (sdl-fill-rect arg0 arg1 arg2)
  (call-c-function "SDL2" "SDL_FillRect" (:pointer :pointer :uint) :int
    arg0 arg1 arg2))

(define (sdl-update-window-surface arg0)
  (call-c-function "SDL2" "SDL_UpdateWindowSurface" (:pointer) :int
    arg0))

(define (sdl-delay arg0)
  (call-c-function "SDL2" "SDL_Delay" (:uint) :void
    arg0))
