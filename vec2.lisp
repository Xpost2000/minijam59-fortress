(in-package :mjgame)

(defstruct vec2 x y)
(defun vec2 (x y) (make-vec2 :x x :y y))

(defun vec2->sdl-point (vec)
  (sdl2:make-point
   (vec2-x vec)
   (vec2-y vec)))
