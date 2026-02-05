;;; Auto-generated Elle bindings from GTK4 header
;;;  Library: gtk-4

;;; Enum: GtkWindowType
(define GTK_WINDOW_TOPLEVEL 0)
(define GTK_WINDOW_POPUP 1)

;;; Function bindings

(define (gtk-init! arg0)
  (call-c-function "gtk-4" "gtk_init" (:pointer :pointer) :void 
    arg0))

(define (gtk-window-new arg0)
  (call-c-function "gtk-4" "gtk_window_new" (:int) :pointer
    arg0))

(define (gtk-widget-get-allocated-width arg0)
  (call-c-function "gtk-4" "gtk_widget_get_allocated_width" (:pointer) :int
    arg0))

(define (gtk-window-set-title arg0 arg1)
  (call-c-function "gtk-4" "gtk_window_set_title" (:pointer :string) :void
    arg0 arg1))

(define (gtk-widget-show arg0)
  (call-c-function "gtk-4" "gtk_widget_show" (:pointer) :void
    arg0))

(define (gtk-widget-hide arg0)
  (call-c-function "gtk-4" "gtk_widget_hide" (:pointer) :void
    arg0))
