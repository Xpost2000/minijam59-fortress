(in-package :mjgame)

(defmethod draw-projectile ((projectile projectile) (renderer renderer))
  (let ((x (vec2-x (projectile-position projectile)))
        (y (vec2-y (projectile-position projectile)))
        (w 2)
        (h 2))
    (draw-filled-rectangle renderer 
                           (rectangle (unit x)
                                      (unit y)
                                      (unit w)
                                      (unit h))
                               (color 255 255 255 255))))

(defmethod update-projectile ((projectile projectile) game delta-time)
  )

(defmethod hit-projectile ((projectile projectile)
                           game
                           (thing enemy))
  )
