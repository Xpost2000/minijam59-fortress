(in-package :mjgame)

;; avoiding circular dependency.

;; (not (barrier-door-open)) would have been better.
(defmethod close-door ((room game-room))
  (when (barrier-door-open room)
    (setf (barrier-door-open room) nil)))

(defmethod open-door ((room game-room))
  (when (not (barrier-door-open room))
    (setf (barrier-door-open room) t)))

(defmethod toggle-door ((room game-room))
  (if (barrier-door-open room)
      (close-door room)
      (open-door room)))

(defmethod room-using-power? ((room game-room))
  (not (barrier-door-open room)))

(defmacro incf-if (condition dest value)
  `(incf ,dest (if ,condition ,value 0)))
(defmethod room-power-cost ((room game-room))
  (let ((power-cost 0))
    (incf-if (barrier-door-open room) power-cost 5)
    power-cost))

(defmethod disable-all-devices ((room game-room))
  (open-door room))

(defmethod update-room ((room game-room) (game game) delta-time)
  (when (<= (ranged-value-current (energy (player game)))
            (ranged-value-min (energy (player game))))
    (disable-all-devices room))

  (when (room-using-power? room)
    (incf (power-usage-timer room) delta-time)

    (when (>= (power-usage-timer room) *power-deduction-wait-time*)
      (ranged-value-decf (energy (player game)) 5)
      (setf (power-usage-timer room) 0.0))))

(defmethod draw-room ((room game-room) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-filled-rectangle renderer 
                           (rectangle (unit room-x) (unit room-y)
                                      (unit (- *room-width* 1))
                                      (unit (- *room-max-height* 1)))
                           +color-red+)
    (draw-filled-rectangle renderer 
                           (rectangle (unit room-x)
                                      (unit (+ room-y (- *room-max-height* *room-floor-height*)))
                                      (unit (- *room-width* 1))
                                      (unit (- *room-floor-height* 1)))
                           +color-blue+)

    ;; door boundary
    (unless (barrier-door-open room)
      (draw-filled-rectangle renderer 
                             (rectangle (unit (- (+ room-x *room-width*) *door-width*))
                                        (unit room-y)
                                        (unit *door-width*)
                                        (unit (- *room-max-height* 1)))
                             +color-green+)
      )))
