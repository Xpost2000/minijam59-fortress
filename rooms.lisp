(in-package :mjgame)

;; ROOM STUFF

(defmethod close-door ((room danger-room)))
(defmethod open-door ((room danger-room)))
(defmethod toggle-door ((room danger-room)))

(defmethod close-door ((room game-room))
  (when (barrier-door-open room)
    (setf (barrier-door-open room) nil)))

(defmethod open-door ((room game-room))
  (when (not (barrier-door-open room))
    (setf (barrier-door-open room) t)))

(defmethod toggle-door ((room game-room))
  (play-sound *close-door-sound*)
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

  (dotimes (turret-index (length (turrets room)))
    (update-turret (aref (turrets room) turret-index) game delta-time))

  (delete-if #'turret-dead-p (turrets room))

  (when (room-using-power? room)
    (incf (power-usage-timer room) delta-time)

    (when (>= (power-usage-timer room) *power-deduction-wait-time*)
      (ranged-value-decf (energy (player game)) 5)
      (setf (power-usage-timer room) 0.0))))

(defun room-get-side-bounding-box (room x y direction)
  ;; x y are technically zero indexed. Un zero them for stuff to work better.
  ;; I technically shouldn't do this here though.... Ugh.
  (let ((x (* x *room-width*))
        (y (* y *room-max-height*)))
    (case direction
      (:left (rectangle x y 1 *room-max-height*))
      (:right (rectangle (1- (+ x *room-width*)) y 1 *room-max-height*))
      (:top (rectangle x y *room-width* 1))
      (:bottom (rectangle x (1- (+ *room-max-height* y)) *room-width* 1)))))

(defun room-get-bounding-box (room x y)
  (rectangle  (* *room-width* x)
              (* *room-max-height* y)
              *room-width*
              *room-max-height*))

(defun add-turret-to-room (room x y turret)
  ;; TODO(check for conflicting turrets!)
  (when (rectangle-intersection (get-bounding-box-turret turret)
                                (room-get-bounding-box room x y))
    (vector-push turret (turrets room))))

(defmethod draw-room :after (room renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (dotimes (turret-index (length (turrets room)))
      (draw-turret (aref (turrets room) turret-index) renderer))
    (unless (barrier-door-open room)
      (draw-filled-rectangle renderer 
                             (rectangle (unit (- (+ room-x *room-width*) *door-width*))
                                        (unit room-y)
                                        (unit *door-width*)
                                        (unit (- *room-max-height* 1)))
                             (color 255 255 0 100)))))

(defmethod draw-room ((room danger-room) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :danger-zone)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))

    (draw-texture renderer *player-base*
                  :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 14)) (* 64 5) (* 64 5))) 
    ;; TODO make head float up and down
    (draw-texture renderer (getf *player-faces* :neutral)
              :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 9)) (* 64 5) (* 64 5)))))

(defmethod draw-room ((room danger-room-b) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :danger-zone-b)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))

    (draw-texture renderer *player-base*
                  :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 14)) (* 64 5) (* 64 5))) 
    ;; TODO make head float up and down
    (draw-texture renderer (getf *player-faces* :morpheous)
              :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 9)) (* 64 5) (* 64 5)))))


(defmethod draw-room ((room danger-room-c) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :danger-zone-c)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))

    (draw-texture renderer *player-base*
                  :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 14)) (* 64 5) (* 64 5))) 
    ;; TODO make head float up and down
    (draw-texture renderer (getf *player-faces* :omega)
              :dest (rectangle (unit (+ room-x 35)) (unit (+ room-y 9)) (* 64 5) (* 64 5)))))

(defmethod draw-room ((room entrance-room) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :vault-entrance)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))))

(defmethod draw-room ((room breach-entrance-room) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :breached-wall)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))))

(defmethod draw-room ((room game-room) renderer x y)
  (let ((room-x (* *room-width* x))
        (room-y (* *room-max-height* y)))
    (draw-texture renderer (getf *room-images* :bunker)
                  :dest (rectangle (unit room-x) (unit room-y)
                                   (unit *room-width*)
                                   (unit *room-max-height*)))))
