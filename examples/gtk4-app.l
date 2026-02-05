;;; GTK4 Application Example
;;;
;;; Demonstrates full FFI integration with GTK4 library:
;;; - Header file parsing and auto-binding
;;; - Window creation and configuration
;;; - Widget layout and event callbacks
;;; - Lisp closures as C callbacks
;;;
;;; To run:
;;; (load "examples/gtk4-app.lisp")
;;;
;;; Requirements:
;;; - GTK4 development libraries installed
;;; - libgtk-4.so.1 in system library path

;;; Load GTK4 header and auto-generate bindings
;;; This parses the C header file and creates Lisp function wrappers
(load-header-with-lib "/usr/include/gtk-4/gtk.h" 
                       "/usr/lib/libgtk-4.so.1")

;;; Create GTK application object
;;; gtk_application_new(const char *application_id, GApplicationFlags flags)
(let app (gtk-application-new "com.example.HelloElleGTK" 0))

;;; Define the application's activate callback
;;; This is called when the application starts
(define (on-activate app-ptr)
  ;; Create main application window
  ;; gtk_application_window_new(GtkApplication *app)
  (let window (gtk-application-window-new app-ptr))
  
  ;; Set window title
  ;; gtk_window_set_title(GtkWindow *window, const char *title)
  (gtk-window-set-title window "Hello from Elle!")
  
  ;; Set default window size
  ;; gtk_window_set_default-size(GtkWindow *window, int width, int height)
  (gtk-window-set-default-size window 400 300)
  
  ;; Create vertical box container for layout
  ;; gtk_box_new(GtkOrientation orientation, int spacing)
  (let box (gtk-box-new GTK_ORIENTATION_VERTICAL 0))
  
  ;; Set margins on the box (padding)
  (gtk-widget-set-margin-top box 12)
  (gtk-widget-set-margin-bottom box 12)
  (gtk-widget-set-margin-start box 12)
  (gtk-widget-set-margin-end box 12)
  
  ;; Create and add label widget
  ;; gtk_label_new(const char *str)
  (let label (gtk-label-new "Welcome to Elle!"))
  (gtk-box-append box label)
  
  ;; Create button widget with label
  ;; gtk_button_new_with_label(const char *label)
  (let button (gtk-button-new-with-label "Click me"))
  
  ;; Connect button click signal to Elle callback
  ;; g_signal_connect(GObject *instance, const char *detailed_signal,
  ;;                  GCallback c_handler, gpointer data)
  ;; 
  ;; The make-c-callback wraps an Elle closure to be called from C
  (g-signal-connect! button "clicked" 
    (make-c-callback 
      (fn [widget data]
        ;; This closure is called when button is clicked
        (print "Button clicked from Elle!")
        ;; Close the window
        (gtk-window-close window))
      :args (:pointer :pointer)
      :return :void)
    nil)
  
  ;; Add button to box
  (gtk-box-append box button)
  
  ;; Set the box as the window's child widget
  ;; gtk_window_set_child(GtkWindow *window, GtkWidget *child)
  (gtk-window-set-child window box)
  
  ;; Show all widgets
  ;; gtk_widget_show(GtkWidget *widget)
  (gtk-widget-show window))

;;; Connect the activate signal to our callback
;; g_signal_connect(GObject *instance, const char *detailed_signal,
;;                  GCallback c_handler, gpointer data)
(g-signal-connect! app "activate" 
  (make-c-callback on-activate :args (:pointer :pointer) :return :void) 
  nil)

;;; Run the application
;; g_application_run(GApplication *application, int argc, char **argv)
(g-application-run app 0 nil)

;;; Notes:
;;; - GTK uses GObject reference counting for memory management
;;; - Elle automatically handles C memory when objects are returned from functions
;;; - Callbacks (via make-c-callback) stay alive while the app runs
;;; - Signal connections use g_signal_connect! with Elle closures as handlers
;;; - Widget hierarchy: Application -> Window -> Box -> Label/Button
