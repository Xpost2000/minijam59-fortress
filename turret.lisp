(in-package :mjgame)

(defun turret-cost (type)
  (case type
    ('turret 250)
    (otherwise 300)))

(defun turret-center-pos (turret)
  (vec2 (+ 2.5 (vec2-x (turret-position turret)))
        (+ 2.5 (vec2-y (turret-position turret)))))

(defun can-fire (turret)
  (<= (fire-cooldown turret) 0.0))

(defun get-bounding-box-turret (turret)
  (rectangle (vec2-x (turret-position turret))
             (vec2-y (turret-position turret))
             5
             5))

(defmethod reload-turret ((turret turret))
  ;; empty
  )
(defmethod update-turret ((turret turret) (game game) delta-time)
  (when (not (can-fire turret))
    (decf (fire-cooldown turret) delta-time)))

(defmethod draw-turret ((turret turret) (renderer renderer))
  (let ((angle (* (atan
                   (- (vec2-y (direction turret)))
                   (- (vec2-x (direction turret)))) (/ 180 PI))))
    (draw-texture renderer (getf *turret-images* :standard)
                  :dest (rectangle (unit (vec2-x (turret-position turret)))
                                   (unit (vec2-y (turret-position turret)))
                                   (unit 5.1)
                                   (unit 2.5))
                  :angle angle
                  :color (if (active turret)
                             (color 0 255 255 255)
                             (color 255 255 255 255)))
    #+-(draw-line renderer
                  (vec2 (unit (vec2-x (turret-position turret)))
                        (unit (vec2-y (turret-position turret))))
                  (vec2-add (vec2 (unit (vec2-x (turret-position turret)))
                                  (unit (vec2-y (turret-position turret))))
                            (vec2-mul (direction turret) (unit 5)))))
  )

(defun turret-aim-at (turret position)
  (setf (direction turret) (vec2-normalize (vec2-sub position (turret-center-pos turret)))))

(defmethod fire-turret ((turret turret) (game game))
  (when (can-fire turret)
    (let* ((direction (direction turret))
           (tip-position (vec2-mul direction 1.25)))
      (vector-push (make-instance 'projectile
                                  :direction direction
                                  :position (vec2-add tip-position (turret-center-pos turret)))
                   (projectiles game))
      (play-sound (get-random-from-list *pew-sounds*))
      (setf (fire-cooldown turret) *default-turret-fire-cooldown*))))
