(in-package :mjgame)

(defun projectile-get-bounding-box (projectile)
  (let ((x (vec2-x (projectile-position projectile)))
        (y (vec2-y (projectile-position projectile)))
        (w 1.25)
        (h 1.25))
    (rectangle x
               y
               w
               h)))

(defmethod draw-projectile ((projectile projectile) (renderer renderer))
  (let ((x (vec2-x (projectile-position projectile)))
        (y (vec2-y (projectile-position projectile)))
        (w 1.25)
        (h 1.25))
    (draw-texture renderer *projectile-image*
                  :dest (rectangle (unit x) (unit y) (unit w) (unit h)))))

(defun find-current-room-for-projectile (projectile game)
  (dotimes (y 3)
    (dotimes (x 3)
      (let ((room-rect (room-get-bounding-box (aref (rooms game) y x) x y))
            (projectile-rect (projectile-get-bounding-box projectile)))
        (when (rectangle-intersection room-rect projectile-rect)
          (return-from find-current-room-for-projectile (values x y)))))))

(defun projectile-hit-room-p (projectile room x y)
  (let ((projectile-bounding-box (projectile-get-bounding-box projectile)))
    (or (rectangle-intersection projectile-bounding-box
                                (room-get-side-bounding-box room
                                                            x
                                                            y
                                                            :left))
        (rectangle-intersection projectile-bounding-box
                                (room-get-side-bounding-box room
                                                            x
                                                            y
                                                            :top))
        (rectangle-intersection projectile-bounding-box
                                (room-get-side-bounding-box room
                                                            x
                                                            y
                                                            :bottom))
        (if (not (barrier-door-open room))
            (rectangle-intersection projectile-bounding-box
                                    (room-get-side-bounding-box room
                                                                x
                                                                y
                                                                :right))
            nil))))

(defun projectile-kill (projectile) (setf (dead projectile) t))
(defmethod update-projectile ((projectile projectile) game delta-time)
  (when (not (projectile-dead-p projectile))
    (vec2-incf (projectile-position projectile)
               (vec2-mul (vec2-mul (direction projectile)
                                   (projectile-speed projectile))
                         delta-time))

    (multiple-value-bind (room-x room-y) (find-current-room-for-projectile projectile game)
      (if (and room-x room-y)
          (with-room ((vec2 room-x room-y) room)
                     (when (projectile-hit-room-p projectile room room-x room-y)
                       (projectile-kill projectile))
                     (dotimes (enemy-index (length (enemies game)))
                       (let ((current-enemy (aref (enemies game) enemy-index)))
                         (when (rectangle-intersection (projectile-get-bounding-box projectile)
                                                       (enemy-get-bounding-box current-enemy))
                           (hit-projectile projectile game current-enemy)))))))

    (decf (lifetime projectile) delta-time)))

(defmethod hit-projectile ((projectile projectile) game (thing enemy))
  (print "hit baddie")
  (play-sound (get-random-from-list *explosion-sounds*))
  (ranged-value-decf (health thing) 25)
  (projectile-kill projectile))
