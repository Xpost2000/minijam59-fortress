(in-package :mjgame)

(defstruct rectangle x y w h)
(defun rectangle (x y w h) (make-rectangle :x x :y y :w w :h h))

(defun rectangle->sdl-rect (rectangle)
  (sdl2:make-rect
   (truncate (rectangle-x rectangle))
   (truncate (rectangle-y rectangle))
   (truncate (rectangle-w rectangle))
   (truncate (rectangle-h rectangle))))

(defun rectangle-position (rectangle)
  (vec2 (rectangle-x rectangle) (rectangle-y rectangle)))
(defun rectangle-size (rectangle)
  (vec2 (rectangle-w rectangle) (rectangle-h rectangle)))

(defun rectangle-intersection (a b)
  (let ((x1 (rectangle-x a)) (x2 (rectangle-x b))
        (y1 (rectangle-y a)) (y2 (rectangle-y b))
        (w1 (rectangle-w a)) (w2 (rectangle-w b))
        (h1 (rectangle-h a)) (h2 (rectangle-h b)))
    (and (and (< x1 (+ x2 w2))
              (< x2 (+ x1 w1)))
         (and (< y1 (+ y2 h2))
              (< y2 (+ y1 h1))))))
