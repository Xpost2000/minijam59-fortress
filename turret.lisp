(in-package :mjgame)

(defun get-bounding-box-turret (turret)
  (rectangle (vec2-x (turret-position turret))
             (vec2-y (turret-position turret))
             5
             5))

(defmethod reload-turret ((turret turret))
  ;; empty
  )
(defmethod update-turret ((turret turret) (game game) delta-time)
  
  )

(defmethod draw-turret ((turret turret) (renderer renderer))
  (draw-filled-rectangle renderer 
                         (rectangle (unit (vec2-x (turret-position turret)))
                                    (unit (vec2-y (turret-position turret)))
                                    (unit 5)
                                    (unit 5))
                         (if (active turret)
                             (color 0 255 255 255)
                             (color 255 0 255 255)))
  )

(defmethod fire-turret ((turret turret) (game game) position)
  (let ((direction (vec2-normalize (vec2-sub position (turret-position turret)))))
    (error "PEW!")
    ))
