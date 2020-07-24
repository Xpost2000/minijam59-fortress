(in-package :mjgame)

(defstruct display-mode
  format
  width
  height
  refresh-rate)

(defun display-mode-resolution-= (first second)
  (and (= (slot-value first 'width)
          (slot-value second 'width))
       (= (slot-value first 'height)
          (slot-value second 'height))))

(defun create-display-mode (display-index mode-index)
  (multiple-value-bind
        (format width height refresh-rate)
      (sdl2:get-display-mode display-index mode-index)
    (make-display-mode :format format
                       :width width
                       :height height
                       :refresh-rate refresh-rate)))

(defun get-display-modes-for (&optional (display-index 0))
  (let* ((display-mode-count (sdl2:get-num-display-modes display-index))
         (display-modes (make-array display-mode-count
                                    :element-type 'list
                                    :fill-pointer 0)))
    (dotimes (index display-mode-count)
      (vector-push (create-display-mode display-index index) display-modes))
    display-modes))

(defmacro with-initialize-sdl (&rest body)
  `(sdl2:with-init (:video :audio)
     (sdl2-image:init '(:png))
     (sdl2-ttf:init)
     (sdl2-mixer:init :ogg :mod :modplug)
     (sdl2-mixer:open-audio 22050 :s16sys 2 2048)
     ,@body
     (sdl2-mixer:close-audio)
     (sdl2-mixer:quit)
     (sdl2-ttf:quit)
     (sdl2-image:quit)))
