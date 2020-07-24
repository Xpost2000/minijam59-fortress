(in-package :mjgame)
;; Resolution picker or something...

;; I mean this is 2020, I'd be a bit remiss if I didn't
;; at least offer some modern conveniences such as resolution selection...

(defparameter *avaliable-resolutions* '())

(defclass launcher (window)
  ((splash-image-texture :accessor splash-image)
   (test-font :accessor font)))

(defmethod window-setup ((launcher launcher))
  (setf (font launcher)
        (load-font (renderer launcher)
                   "resources/fonts/Anonymous Pro.ttf" 64))
  (setf (splash-image launcher)
        (load-image (renderer launcher) "launcher-splash.png"))
  (setf *avaliable-resolutions*
        (remove-duplicates (get-display-modes-for)
                           :test #'display-mode-resolution-=)))

(defmethod window-frame ((launcher launcher) delta-time)
  (clear-color (renderer launcher) (color 30 30 40 255))

  (draw-line (renderer launcher)
             (vec2 100 100)
             (vec2 200 300))

  (draw-filled-rectangle (renderer launcher) (rectangle 200 300 200 200))
  (draw-texture (renderer launcher) (splash-image launcher)
                :dest (rectangle 120 100 300 226))

  (draw-texture (renderer launcher)
                (splash-image launcher)
                :dest (rectangle 250 180 300 226)
                :angle 180
                :color +color-red+)
  (draw-string (renderer launcher) "Hello World!" (font launcher) (vec2 0 0))

  (if (is-key-pressed (input launcher) :scancode-a)
      (print "A down!"))
  (if (is-key-down (input launcher) :scancode-escape)
      (sdl2:push-event :quit)))

(defmethod window-quit ((launcher launcher))
  (renderer-destroy (renderer launcher))
  ;; set flags for real game window and go.
  (setf *screen-width* 1024)
  (setf *screen-height* 768)
  (setf *screen-fullscreen-mode* nil))
