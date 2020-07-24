(in-package :mjgame)

(defstruct vec2 x y)
(defun vec2 (x y) (make-vec2 :x x :y y))

;; should do typechecking?
(defun vec2-mul (a b)
  (vec2 (* (vec2-x a) b)
        (* (vec2-y a) b)))

(defun vec2-sub (a b)
  (vec2 (- (vec2-x a) (vec2-x b))
        (- (vec2-y a) (vec2-y b))))

(defun vec2-add (a b)
  (vec2 (+ (vec2-x a) (vec2-x b))
        (+ (vec2-y a) (vec2-y b))))

(defmacro vec2-decf (a b)
  `(setf ,a (vec2-sub ,a ,b)))

(defmacro vec2-incf (a b)
  `(setf ,a (vec2-add ,a ,b)))

(defun vec2->sdl-point (vec)
  (sdl2:make-point
   (truncate (vec2-x vec))
   (truncate (vec2-y vec))))
