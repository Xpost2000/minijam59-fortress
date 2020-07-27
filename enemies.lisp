(in-package :mjgame)

(defun enemy-get-bounding-box (enemy)
  (rectangle (location-x (location enemy))
             (location-y (location enemy))
             (vec2-x (size enemy))
             (vec2-y (size enemy))))

(defun align-enemy-to-room-floor (enemy)
  (let ((room-y (* (location-room-y (location enemy))
                   *room-max-height*)))
    (setf (location-y (location enemy))
          (- (- (+ room-y *room-max-height*) *room-floor-height*)
             (vec2-y (size enemy))))))

;; recalculate room position
;; I should probably check the array dimensions itself
;; but I'm not changing the size of the world so eh.
(defun find-current-room-for-enemy (enemy game)
  (dotimes (y 3)
    (dotimes (x 3)
      (let ((room-rect (room-get-bounding-box (aref (rooms game) y x) x y))
            (enemy-rect (enemy-get-bounding-box enemy)))
        (when (rectangle-intersection enemy-rect room-rect)
          (location-update-room-position (location enemy) x y)
          (return-from find-current-room-for-enemy))))))

(defun in-last-room (enemy) (= (location-room-x (location enemy)) 2))
(defun can-attack (enemy) (<= (attack-cooldown enemy) 0))
(defun enemy-advance (enemy amount delta-time)
  (incf (location-x (location enemy)) (* delta-time amount)))

;; just incase we actually want to specialize certain things?
(defmethod enemy-attack ((enemy enemy) player)
  (ranged-value-decf (health player) *enemy-default-attack-damage*)
  (setf (attack-cooldown enemy) *enemy-default-attack-cooldown*))

(defmethod enemy-attack ((enemy eye-droid) player)
  (play-sound (get-random-from-list *hurt-sounds*))
  (ranged-value-decf (health player) 2)
  (setf (attack-cooldown enemy) 0.15))

(defmethod update-enemy ((enemy enemy) (game game) delta-time)
  (with-room ((location-room-position->vec2 (location enemy)) room)
             ;; handle collision with room stuff here.
             ;; and whatever.
             (let ((right-side-of-room (room-get-side-bounding-box room
                                                                   (location-room-x (location enemy))
                                                                   (location-room-y (location enemy))
                                                                   :right)))
               (if (and (in-last-room enemy)
                        (rectangle-intersection (enemy-get-bounding-box enemy) right-side-of-room))
                   (when (can-attack enemy) (enemy-attack enemy (player game)))
                   (when (barrier-door-open room)
                     (enemy-advance enemy 4 delta-time)))
               (decf (attack-cooldown enemy) delta-time))
             (align-enemy-to-room-floor enemy)
             (find-current-room-for-enemy enemy game)))

(defun enemy-type->keyword (type)
  (case type
    ('eye-droid :eye-droid)
    ('enemy :bad-rob)
    ('ball-f :ball-f)))

;; (defmethod draw-enemy ((enemy eye-droid) (renderer renderer))
;;   (let ((x (location-x (location enemy)))
;;         (y (location-y (location enemy)))
;;         (w (vec2-x (size enemy)))
;;         (h (vec2-y (size enemy))))
;;     (draw-texture renderer
;;                   (getf *enemy-images*
;;                         :eye-droid
;;                         #+-(enemy-type->keyword (type-of enemy)))
;;                   :dest (rectangle x y w h))))

;; (defmethod draw-enemy ((enemy ball-f) (renderer renderer))
;;   (let ((x (location-x (location enemy)))
;;         (y (location-y (location enemy)))
;;         (w (vec2-x (size enemy)))
;;         (h (vec2-y (size enemy))))
;;     (draw-texture renderer
;;                   (getf *enemy-images*
;;                         :eye-droid
;;                         #+-(enemy-type->keyword (type-of enemy)))
;;                   :dest (rectangle x y w h))))


(defmethod draw-enemy ((enemy enemy) (renderer renderer))
  (let ((x (location-x (location enemy)))
        (y (location-y (location enemy)))
        (w (vec2-x (size enemy)))
        (h (vec2-y (size enemy))))
    (draw-texture renderer
                  ;; (getf *turret-images* :standard)
                  (getf *enemy-images* (enemy-type->keyword (type-of enemy)))
                  :dest (rectangle (unit x)
                                   (unit y)
                                   (unit w)
                                   (unit h)))))
