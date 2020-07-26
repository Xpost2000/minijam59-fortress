(in-package :mjgame)

(defmethod reload-turret ((turret turret))
  ;; empty
  )
(defmethod update-turret ((turret turret) (game game) delta-time)
  )

(defmethod draw-turret ((turret turret) (renderer renderer))
  )

(defmethod fire-turret ((turret turret) (game game))
  (error "fire-turret"))
