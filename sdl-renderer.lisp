(in-package :mjgame)

(defstruct color r g b a)
(defun color (r g b a) (make-color :r r :g g :b b :a a))

(defparameter +color-white+ (make-color :r 255 :g 255 :b 255 :a 255))
(defparameter +color-red+ (make-color :r 255 :g 0 :b 0 :a 255))
(defparameter +color-blue+ (make-color :r 0 :g 0 :b 255 :a 255))
(defparameter +color-green+ (make-color :r 0 :g 255 :b 0 :a 255))
(defparameter +color-black+ (make-color :r 0 :g 0 :b 0 :a 0))

(defclass font-resource ()
  ((font :reader font
         :initarg :font
         :initform nil)
   (glyph-cache :accessor glyph-cache
                :initarg :glyph-cache
                :initform (make-hash-table))
   (font-height :reader font-height
                :initarg :font-height
                :initform 16)))
(defun get-glyph (font character)
  (gethash character (glyph-cache font)))

(defun render-glyph-texture (renderer font character)
  (let* ((surface (sdl2-ttf:render-utf8-blended
                   font
                   (string (code-char
                            (if (= character 0) 32 character)))
                   255 255 255 255)))
    (sdl2:create-texture-from-surface renderer surface)))

(defmethod initialize-instance :after ((font font-resource) &key renderer)
  (dotimes (character 128)
    (setf (gethash (code-char character) (glyph-cache font))
          (render-glyph-texture renderer (font font) character))))

(defun destroy-font (font)
  (sdl2-ttf:close-font (font font))
  (loop for v being the hash-value in (glyph-cache font) do
    (sdl2:destroy-texture v)))

(defun char-y-advance (font character)
  (let ((glyph (gethash character (glyph-cache font))))
    (sdl2:texture-height glyph)))

(defun char-x-advance (font character)
  (let ((glyph (gethash character (glyph-cache font))))
    (sdl2:texture-width glyph)))

(defstruct camera (position (vec2 0 0)))
(defun camera-transform-point (camera original)
  (vec2 (- (vec2-x original) (vec2-x (camera-position camera)))
        (- (vec2-y original) (vec2-y (camera-position camera)))))

(defclass renderer ()
  ((renderer :accessor renderer :initarg :renderer)
   (camera :accessor camera
           :initarg camera
           :initform (make-camera))
   (screen :accessor screen :initarg :screen)
   (screen-width :accessor screen-width
                 :initarg :screen-width
                 :initform 0)
   (screen-height :accessor screen-height
                  :initarg :screen-height
                  :initform 0)
   (fonts :accessor fonts
          :initarg :fonts
          :initform (make-hash-table))
   (images :accessor images
           :initarg images
           :initform (make-hash-table))))

(defun find-font (renderer font-path)
  (let ((font (gethash font-path (fonts renderer))))
    (unless font 
      (error (format nil "Could not find font ~a" font-path)))
    font))

(defun find-image (renderer image-path)
  (let ((image (gethash font-path (images renderer))))
    (unless image 
      (error (format nil "Could not find image ~a" image-path)))
    font))

(defmethod initialize-instance :after ((renderer renderer) &key)
  (multiple-value-bind (width height) (sdl2:get-window-size (screen renderer))
    (setf (renderer renderer) (sdl2:create-renderer (screen renderer) -1 '(:accelerated)))
    (setf (screen-width renderer) width)
    (setf (screen-height renderer) height)))

(defun renderer-destroy (renderer)
  (loop for v being the hash-value in (images renderer) do
    (sdl2:destroy-texture v))
  (loop for v being the hash-value in (fonts renderer) do
    (destroy-font v))
  (sdl2:destroy-renderer (renderer renderer)))

;; does not hash by name & size... Should be hashing by size as well...
(defun load-font (renderer font-path height)
  (let ((font (make-instance 'font-resource
                             :renderer (renderer renderer)
                             :font-height height
                             :font (sdl2-ttf:open-font font-path height))))
    (setf (gethash font-path (fonts renderer)) font)))

(defun load-image (renderer image-name)
  (let* ((surface (sdl2-image:load-image image-name))
         (texture (sdl2:create-texture-from-surface (renderer renderer) surface)))
    (sdl2:free-surface surface)
    (setf (gethash image-name (images renderer)) texture)))

(defun set-draw-color (renderer color)
  (sdl2:set-render-draw-color (renderer renderer)
                              (color-r color)
                              (color-g color)
                              (color-b color)
                              (color-a color)))

(defun present-frame (renderer)
  (sdl2:render-present (renderer renderer)))

(defun set-blend-mode (renderer mode)
  (sdl2:set-render-draw-blend-mode (renderer renderer) mode))

(defun clear-color (renderer color)
  (set-draw-color renderer color)
  (sdl2:render-clear (renderer renderer)))

(defun draw-texture (renderer texture &key
                                        dest
                                        (src nil)
                                        (angle 0.0)
                                        (center nil)
                                        (flip nil)
                                        (color +color-white+))
  (let* ((transformed-position (camera-transform-point (camera renderer) (rectangle-position dest)))
         (dest (rectangle (vec2-x transformed-position)
                          (vec2-y transformed-position)
                          (rectangle-w dest)
                          (rectangle-h dest))))
    (let ((src-sdl-rect (if src (rectangle->sdl-rect src) (cffi:null-pointer)))
          (dest-sdl-rect (if dest (rectangle->sdl-rect dest) (cffi:null-pointer)))
          (center-sdl-point (if center (vec2->sdl-point center) (cffi:null-pointer))))
      (sdl2:set-texture-color-mod texture (color-r color) (color-g color) (color-b color))
      (sdl2:set-texture-alpha-mod texture (color-a color))
      (sdl2:render-copy-ex (renderer renderer) texture
                           :source-rect src-sdl-rect
                           :dest-rect dest-sdl-rect
                           :angle angle
                           :center center
                           :flip flip)
      (when center (sdl2:free-point center-sdl-point))
      (when dest (sdl2:free-rect dest-sdl-rect))
      (when src (sdl2:free-rect src-sdl-rect)))))

(defun draw-character (renderer character font position &key (color +color-white+) (size :default))
  (let* ((character-glyph (get-glyph font character)))
    (let* ((scale-factor (if (keywordp size)
                             (when (eq size :default) 1)
                             (/ size (font-height font))))
           (character-rect (make-rectangle
                            :x (vec2-x position)  :y (vec2-y position)
                            :w (* (sdl2:texture-width character-glyph) scale-factor)
                            :h (* (sdl2:texture-height character-glyph) scale-factor))))
      (draw-texture renderer character-glyph
                    :dest character-rect
                    :color color)
      scale-factor)))

(defun draw-string (renderer string font position &key (color +color-white+) (size :default))
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
           (let ((scale-factor (draw-character renderer character font current-position :color color :size size)))
             (incf (vec2-x current-position) (* scale-factor (char-x-advance font character))))))))))

(defun draw-filled-rectangle (renderer rectangle &optional (color +color-white+))
  (set-draw-color renderer color)
  (let* ((transformed-position
           (camera-transform-point (camera renderer)
                                   (rectangle-position rectangle)))
         (rectangle (rectangle (vec2-x transformed-position)
                               (vec2-y transformed-position)
                               (rectangle-w rectangle)
                               (rectangle-h rectangle))))
    (let ((draw-rect (rectangle->sdl-rect rectangle)))
      (sdl2:render-fill-rect (renderer renderer) draw-rect)
      (sdl2:free-rect draw-rect))))

(defun draw-rectangle (renderer rectangle &optional (color +color-white+))
  (set-draw-color renderer color)
  (let* ((transformed-position
           (camera-transform-point (camera renderer)
                                   (rectangle-position rectangle)))
         (rectangle (rectangle (vec2-x transformed-position)
                               (vec2-y transformed-position)
                               (rectangle-w rectangle)
                               (rectangle-h rectangle))))
    (let ((draw-rect (rectangle->sdl-rect rectangle)))
      (sdl2:render-draw-rect (renderer renderer) draw-rect)
      (sdl2:free-rect draw-rect))))

(defun draw-line (renderer start end &optional (color +color-white+))
  (set-draw-color renderer color)
  (let ((start (camera-transform-point (camera renderer) start))
        (end (camera-transform-point (camera renderer) end)))
    (sdl2:render-draw-line (renderer renderer)
                           (vec2-x start)
                           (vec2-y start)
                           (vec2-x end)
                           (vec2-y end))))

(defun set-camera-position (renderer position)
  (setf (camera-position (camera renderer)) position))

(defun reset-camera (renderer)
  (set-camera-position renderer (vec2 0 0)))
