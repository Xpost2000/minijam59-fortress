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
