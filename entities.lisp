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
(defun player-dead-p (player)
  (<= (ranged-value-current (health player)) 0))

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

(defstruct location
  ;; room location not sure if technically using?
  (room-x 0)
  (room-y 0)
  ;; units
  (x 0)
  (y 0))

(defun location-update-position (location x y)
  (setf (location-x location) x)
  (setf (location-y location) y))
(defun location-update-room-position (location x y)
  (setf (location-room-x location) x)
  (setf (location-room-y location) y))

(defun location->vec2 (location)
  (vec2 (location-x location)
        (location-y location)))
(defun location-room-position->vec2 (location)
  (vec2 (location-room-x location)
        (location-room-y location)))

;; Enemies basically just walk in a straight lane to the
;; end so y is near irrelevant.
(defparameter *enemy-default-attack-damage* 5)
(defparameter *enemy-default-attack-cooldown* 1.5)
(defclass enemy ()
  ((location :accessor location
             :initarg :location
             :initform (make-location))
   (health :accessor health
           :initarg :health
           :initform (ranged 0 100 :max))
   (defense :accessor defense
            :initarg :defense
            :initform (ranged 0 50 :max))
   (attack-cooldown :accessor attack-cooldown
                    :initform 0.0)
   (size :accessor size
         :initarg :size
         :initform (vec2 3.5 4.55))))

(defun enemy-dead-p (enemy)
  (<= (ranged-value-current (health enemy)) 0))

(defgeneric enemy-attack (enemy player))
(defgeneric update-enemy (enemy game delta-time))
(defgeneric draw-enemy (enemy renderer))
