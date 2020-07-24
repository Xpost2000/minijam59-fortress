(in-package :mjgame)
;; Resolution picker or something...

(defparameter *resolution-picker-width* 800)
(defparameter *resolution-picker-height* 600)
;; I mean this is 2020, I'd be a bit remiss if I didn't
;; at least offer some modern conveniences such as resolution selection...

(defun calculate-frame-delta-time (start end) (* (- end start) 0.0001))

(defun handle-basic-launcher ()
  (let ((resolutions (remove-duplicates (get-display-modes-for)
                                        :test #'display-mode-resolution-=))
        (delta-time 0.0))
    (print resolutions)
    (sdl2:with-window (resolution-picker-window
                       :title "Resolution Picker"
                       :w *resolution-picker-width*
                       :h *resolution-picker-height*)
      #+windows ;; Weird slime thing on windows...
      (progn
        (sdl2:hide-window resolution-picker-window)
        (sdl2:show-window resolution-picker-window))

      (let ((input (make-instance 'input-handler))
            (renderer (make-instance 'renderer :screen resolution-picker-window)))
        (let* ((splash-image-texture (load-image renderer "launcher-splash.png"))
               (test-font (load-font renderer "resources/fonts/Anonymous Pro.ttf" 64)))
          (sdl2:with-event-loop (:method :poll)
            (:mousemotion (:x mouse-x :y mouse-y)
                         (input-handle-mouse-motion input mouse-x mouse-y))
            (:mousebuttonup (:button button :state state :clicks clicks)
                           (input-handle-mouse-button-up input button state clicks))
            (:mousebuttondown (:button button :state state :clicks clicks)
                             (input-handle-mouse-button-down input button state clicks))
            (:keydown (:keysym keysym)
                     (input-handle-key-down input keysym))
            (:keyup (:keysym keysym)
                   (input-handle-key-up input keysym))
            (:idle ()
                   (let ((start-time (sdl2:get-ticks)))
                     (clear-color renderer (color 30 30 40 255))

                     (draw-line renderer
                                (vec2 100 100)
                                (vec2 200 300))

                     (draw-filled-rectangle renderer (rectangle 200 300 200 200))
                     (draw-texture renderer
                                   splash-image-texture
                                   :dest (rectangle 120 100 300 226))

                     (draw-texture renderer
                                   splash-image-texture
                                   :dest (rectangle 250 180 300 226)
                                   :angle 180
                                   :color +color-red+)
                     (draw-string renderer "Hello World!" test-font (vec2 0 0))

                     (if (is-key-down input :scancode-a)
                         (print "A down!"))
                     (if (is-key-down input :scancode-escape)
                         (sdl2:push-event :quit))

                     (present-frame renderer)
                     (setf delta-time
                           (calculate-frame-delta-time start-time
                                                       (sdl2:get-ticks)))
                     (input-new-frame input)))
            (:quit ()
                   (renderer-destroy renderer)
                   ;; set flags for real game window and go.
                   (setf *screen-width* 1024)
                   (setf *screen-height* 768)
                   (setf *screen-fullscreen-mode* nil)
                   t)))))))
