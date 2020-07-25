(in-package :mjgame)
;; Or anything that looks like an entity.
;; things?

;; overseer?
(defclass player ()
  ((location :accessor location
             :initarg :location
             :initform (vec2 0 0)) ;; index into array of rooms.
   (health :accessor health
           :initform (ranged 0 100 :max))
   (energy :accessor energy
           :initform (ranged 0 100 :max))))

;; Incase I actually make this a real "entity" thing
(defclass screen-fade ()
  ((timer :accessor timer
          :initform 0.0)
   (fade-length :accessor fade-length
           :initarg :fade-length
           :initform 0.0)
   (active :accessor active
           :initarg :active
           :initform nil)
   (direction :accessor direction
              :initarg :direction
              :initform :fade-out)
   (fade-color :accessor fade-color
          :initarg :fade-color
          :initform +color-black+)
   ;; callback
   (on-finish :accessor on-finish
              :initarg on-finish)))

(defun start-fade (fader
                   &key
                     length
                     (linger-length 0.0)
                     (direction :fade-out)
                     (color +color-black+)
                     on-finish)
  (unless (active fader)
    (setf (active fader) t)
    (setf (direction fader) direction)
    (setf (fade-length fader) length)
    (setf (on-finish fader) on-finish)
    (setf (fade-color fader) color)
    (setf (timer fader) (+ (fade-length fader) linger-length))))

;; if I have time I guess.
#+- (defmacro fade-out-and-in-transition (&key
                                        color
                                        fade-in-length
                                        fade-in-linger-length
                                        ;;on-fade-in-finish

                                        (fade-out-length fade-in-length)
                                        (fade-out-linger-length fade-in-linger-length)
                                        on-fade-out-finish)
  )

(defun screen-fade-draw (fader renderer)
  ;; no renderlayer or command buffer to defer stuff. so this...
  (reset-camera renderer)
  (when (active fader)
    (let ((alpha (cond
                   ((eql (direction fader) :fade-in)
                    (round (* 255
                              (clamp (/ (timer fader) (fade-length fader))
                                     0 1))))
                   ((eql (direction fader) :fade-out)
                    (round (* 255
                              (clamp (/ (- (fade-length fader) (timer fader))
                                        (fade-length fader))
                                     0 1)))))))
      (draw-filled-rectangle renderer
                             (rectangle 0 0
                                        (screen-width renderer)
                                        (screen-height renderer))
                             (color (color-r (fade-color fader))
                                    (color-g (fade-color fader))
                                    (color-b (fade-color fader))
                                    alpha)))))

(defun screen-fade-update (fader delta-time)
  (when (active fader)
    (decf (timer fader) delta-time)
    (when (<= (timer fader) 0.0)
      (setf (active fader) nil)
      (when (functionp (on-finish fader)) 
        (funcall (on-finish fader))))))

;; all are in units
(defparameter *room-width* 50)
(defparameter *room-max-height* 30)
(defparameter *room-floor-height* 4) ;; from floor

(defparameter *door-width* 2)

(defparameter *power-deduction-wait-time* 3.5) ;; seconds

;; Should I lerp animate the door?
(defclass game-room ()
  ((power-usage-timer :accessor power-usage-timer
                      :initform 0.0)
   (barrier-door-open :accessor barrier-door-open
                      :initform t)))

(defgeneric draw-room (room renderer x y))
(defgeneric room-using-power? (room))
(defgeneric room-power-cost (room))
(defgeneric update-room (room game-state delta-time))
(defgeneric close-door (room))
(defgeneric open-door (room))
(defgeneric toggle-door (room))
(defgeneric disable-all-devices (room))

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
