(in-package :mjgame)

(defstruct rectangle x y w h)
(defun rectangle (x y w h) (make-rectangle :x x :y y :w w :h h))

(defun rectangle->sdl-rect (rectangle)
  (sdl2:make-rect
   (rectangle-x rectangle)
   (rectangle-y rectangle)
   (rectangle-w rectangle)
   (rectangle-h rectangle)))

(defun rectangle-position (rectangle)
  (vec2 (rectangle-x rectangle) (rectangle-y rectangle)))
(defun rectangle-size (rectangle)
  (vec2 (rectangle-w rectangle) (rectangle-h rectangle)))
