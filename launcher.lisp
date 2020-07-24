(in-package :mjgame)
;; Resolution picker or something...

(defparameter *resolution-picker-width* 800)
(defparameter *resolution-picker-height* 600)
;; I mean this is 2020, I'd be a bit remiss if I didn't
;; at least offer some modern conveniences such as resolution selection...

(defstruct vec2 x y)

(defun vec2->sdl-point (vec)
  (sdl2:make-point
   (vec2-x vec)
   (vec2-y vec)))

(defstruct rectangle x y w h)

(defun rectangle->sdl-rect (rectangle)
  (sdl2:make-rect
   (rectangle-x rectangle)
   (rectangle-y rectangle)
   (rectangle-w rectangle)
   (rectangle-h rectangle)))

(defun rectangle-position (rectangle)
  (make-vec2 :x (rectangle-x rectangle)
             :y (rectangle-y rectangle)))
(defun rectangle-size (rectangle)
  (make-vec2 :x (rectangle-w rectangle)
             :y (rectangle-h rectangle)))

(defstruct color r g b a)

(defparameter +color-white+ (make-color :r 255 :g 255 :b 255 :a 255))
(defparameter +color-red+ (make-color :r 255 :g 0 :b 0 :a 255))
(defparameter +color-blue+ (make-color :r 0 :g 0 :b 255 :a 255))
(defparameter +color-green+ (make-color :r 0 :g 255 :b 0 :a 255))
(defparameter +color-black+ (make-color :r 0 :g 0 :b 0 :a 0))

(defun set-draw-color (renderer color)
  (sdl2:set-render-draw-color renderer
                              (color-r color)
                              (color-g color)
                              (color-b color)
                              (color-a color)))

(defun glyph-y-advance (glyph-table character)
  (let ((character (gethash character glyph-table)))
    (sdl2:texture-height character)))

(defun glyph-x-advance (glyph-table character)
  (let ((character (gethash character glyph-table)))
    (sdl2:texture-width character)))

(defun render-glyph-texture (renderer font character)
  (let* ((surface (sdl2-ttf:render-utf8-blended
                   font
                   (string (code-char
                            (if (= character 0) 32 character)))
                   255 255 255 255)))
    (sdl2:create-texture-from-surface renderer surface)))

(defun build-font-glyph-table (renderer font)
  (let ((glyph-table (make-hash-table)))
    (dotimes (character 128)
      (setf (gethash (code-char character) glyph-table)
            (render-glyph-texture renderer font character)))
    glyph-table))

(defun draw-texture (renderer texture &key
                                        dest
                                        (src nil)
                                        (angle 0.0)
                                        (center nil)
                                        (flip nil)
                                        (color +color-white+))
  (let ((src-sdl-rect (if src (rectangle->sdl-rect src) (cffi:null-pointer)))
        (dest-sdl-rect (if dest (rectangle->sdl-rect dest) (cffi:null-pointer)))
        (center-sdl-point (if center (vec2->sdl-point center) (cffi:null-pointer))))
    (sdl2:set-texture-color-mod texture (color-r color) (color-g color) (color-b color))
    (sdl2:set-texture-alpha-mod texture (color-a color))
    (sdl2:render-copy-ex renderer texture
                         :source-rect src-sdl-rect
                         :dest-rect dest-sdl-rect
                         :angle angle
                         :center center
                         :flip flip)
    (when center (sdl2:free-point center-sdl-point))
    (when dest (sdl2:free-rect dest-sdl-rect))
    (when src (sdl2:free-rect src-sdl-rect))))

(defun draw-character (renderer character glyph-table position &optional (color +color-white+))
  (let* ((character-glyph (gethash character glyph-table))
         (character-rect (make-rectangle
                          :x (vec2-x position) :y (vec2-y position)
                          :w (sdl2:texture-width character-glyph)
                          :h (sdl2:texture-height character-glyph))))
    (draw-texture renderer character-glyph
                  :dest character-rect
                  :color color)))

(defun draw-string (renderer string glyph-table position &optional (color +color-white+))
  (let ((start-position position)
        (current-position position))
    (dotimes (index (length string))
      (let ((character (char string index)))
        (case character
          (#\Tab (incf (vec2-x current-position) 4))
          (#\Newline (progn
                       (incf (vec2-y current-position))
                       (setf (vec2-x current-position)
                             (vec2-x start-position))))
          (otherwise
           (draw-character renderer character glyph-table current-position color)
           (incf (vec2-x current-position) (glyph-x-advance glyph-table character))))))))

(defun draw-filled-rectangle (renderer rectangle &optional (color +color-white+))
  (set-draw-color renderer color)
  (let ((draw-rect (rectangle->sdl-rect rectangle)))
    (sdl2:render-fill-rect renderer draw-rect)
    (sdl2:free-rect draw-rect)))

(defun draw-rectangle (renderer rectangle &optional (color +color-white+))
  (set-draw-color renderer color)
  (let ((draw-rect (rectangle->sdl-rect rectangle)))
    (sdl2:render-draw-rect renderer draw-rect)
    (sdl2:free-rect draw-rect)))

(defun draw-line (renderer start end &optional (color +color-white+))
  (set-draw-color renderer color)
  (sdl2:render-draw-line renderer
                         (vec2-x start)
                         (vec2-y start)
                         (vec2-x end)
                         (vec2-y end)))

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

      (sdl2:with-renderer (renderer
                           resolution-picker-window
                           :flags '(:accelerated))
        (let* ((splash-image-surface (sdl2-image:load-image "launcher-splash.png"))
               (splash-image-texture (sdl2:create-texture-from-surface renderer splash-image-surface))
               (test-font (sdl2-ttf:open-font "resources/fonts/Anonymous Pro.ttf" 64))
               (test-font-glyphs (build-font-glyph-table renderer test-font)))
          (sdl2:with-event-loop (:method :poll)
            (:mousemotion (:x mouse-x
                           :y mouse-y)
                          )
            (:mousebuttondown (:button button
                               :state state
                               :clicks clicks)
                              )
            (:keydown (:keysym keysym)
                      )
            (:keyup (:keysym keysym)
                    )
            (:idle ()
                   (let ((start-time (sdl2:get-ticks)))
                     (sdl2:set-render-draw-color renderer 30 30 40 255)
                     (sdl2:render-clear renderer)

                     (draw-line renderer
                                (make-vec2 :x 100 :y 100)
                                (make-vec2 :x 200 :y 300))

                     (draw-filled-rectangle renderer
                                            (make-rectangle
                                             :x 200 :y 300
                                             :w 200 :h 200))

                     (draw-texture renderer
                                   splash-image-texture
                                   :dest (make-rectangle
                                          :x 120
                                          :y 100
                                          :w 300
                                          :h 226))

                     (draw-texture renderer
                                   splash-image-texture
                                   :dest (make-rectangle
                                          :x 250
                                          :y 180
                                          :w 300
                                          :h 226)
                                   :angle 180
                                   :color +color-red+)

                     (draw-string renderer "Hello World!" test-font-glyphs (make-vec2 :x 0 :y 0))

                     (sdl2:render-present renderer)
                     (setf delta-time
                           (calculate-frame-delta-time start-time
                                                       (sdl2:get-ticks)))))
            (:quit ()
                   (sdl2-ttf:close-font test-font)
                   (sdl2:destroy-texture splash-image-texture)
                   (sdl2:free-surface splash-image-surface)
                   ;; set flags for real game window and go.
                   (setf *screen-width* 1024)
                   (setf *screen-height* 768)
                   (setf *screen-fullscreen-mode* nil)
                   t)))))))
