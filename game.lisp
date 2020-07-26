(in-package :mjgame)

(defparameter *game-window* nil) ;; expose to repl to test resolution stuff
(defparameter *game-font* nil)

(defparameter *test-sound-chunk* nil)

(defstruct ranged-value
  (current 0)
  (min 0)
  (max 0))

(defun ranged (min max &optional current)
  (make-ranged-value :min min
                     :max max
                     :current (if (keywordp current)
                                  (ecase current
                                    ((:min :minimum) min)
                                    ((:max :maximum) max))
                                  current)))
(defmacro ranged-value-incf (ranged val)
  `(setf ,ranged (ranged-value-add ,ranged ,val)))
(defmacro ranged-value-decf (ranged val)
  `(setf ,ranged (ranged-value-subtract ,ranged ,val)))

(defun ranged-value-subtract (ranged val)
  (ranged (ranged-value-min ranged)
          (ranged-value-max ranged)
          (clamp (- (ranged-value-current ranged) val)
                 (ranged-value-min ranged)
                 (ranged-value-max ranged))))

(defun ranged-value-add (ranged val)
  (ranged (ranged-value-min ranged)
          (ranged-value-max ranged)
          (clamp (+ (ranged-value-current ranged) val)
                 (ranged-value-min ranged)
                 (ranged-value-max ranged))))

(defconstant +max-enemies-in-game+ 2048)
(defconstant +max-projectiles-in-game+ 2048)

;; should change every round or so?
(defconstant +default-enemy-spawn-cooldown+ 2.5)

(defclass game (window)
  ((camera :accessor game-camera
           :initform (make-camera))
   (fader :accessor fader
          :initform (make-instance 'screen-fade)) 
   (state :accessor state
          :initform :mainmenu)
   (points :accessor points :initform 0)
   (score :accessor score :initform 0)
   (enemy-spawn-cooldown :accessor enemy-spawn-cooldown
                         :initform 0)
   (remaining-enemies-to-spawn :accessor remaining-enemies-to-spawn
                               :initform 0)

   (current-round :accessor current-round
                  :initform 1)
   (round-started :accessor round-started
                  :initform nil)

   (enemies :accessor enemies
            :initform (make-array
                       +max-enemies-in-game+
                       :element-type 'enemy
                       :fill-pointer 0))
   (projectiles :accessor projectiles
                :initform (make-array
                           +max-projectiles-in-game+
                           :element-type 'projectile
                           :fill-pointer 0))
   (rooms :accessor rooms)
   (player :accessor player
           :initform (make-instance 'player))))

;; image constants
;; not storing as part of game state
(defvar *player-base*) ;; altar thing?
(defvar *player-faces*) ;; player face

(defvar *room-images*)
(defvar *main-menu-image*)
(defvar *can-move*)

(defvar *enemy-images*)

;; TODO game reset!
(defun can-spawn-enemy (game)
  (<= (enemy-spawn-cooldown game) 0))

;; requires context to make a more fair random.
(defun random-type-of-enemy (game) 'enemy)
(defun random-room-floor ()
  (random 3))

(defun spawn-enemy (game &key floor (type 'enemy))
  (setf (enemy-spawn-cooldown game) +default-enemy-spawn-cooldown+)
  (decf (remaining-enemies-to-spawn game))
  (vector-push (make-instance type :location (make-room-location :y floor)) (enemies game)))

(defun start-round (game)
  (unless (round-started game)
    (print "END ROUND")
    (setf (round-started game) t)
    (setf (remaining-enemies-to-spawn game) 15)))

(defun end-round (game)
  (when (round-started game)
    (print "START ROUND!")
    (reset-turret-active-status-on-all-rooms game)
    (incf (current-round game))
    (setf (round-started game) nil)))

(defmethod window-setup ((game game))
  (setf *can-move* t)
  ;; this context is nasty...
  (setf *game-window* game)
  (setf *game-font* (load-font (renderer game)
                               "resources/fonts/PxPlus_IBM_VGA8.ttf" 98))

  (setf *test-sound-chunk* (load-sound "resources/sound/wave_finished.wav"))

  (setf *main-menu-image* (load-image (renderer game) "resources/images/title.png"))
  (setf *player-base* (load-image (renderer game) "resources/images/player-altar.png"))
  (setf *player-faces* (list
                        :neutral (load-image (renderer game) "resources/images/player-face-neutral.png")
                        :hurt (load-image (renderer game) "resources/images/player-face-hurt.png")))
  (setf *room-images* (list
                       :breached-wall
                       (load-image (renderer game) "resources/images/room-wall-breach.png")
                       :danger-zone
                       (load-image (renderer game) "resources/images/room-danger.png")
                       :bunker
                       (load-image (renderer game) "resources/images/room-bunker.png")
                       :vault-entrance
                       (load-image (renderer game) "resources/images/vault_entrance_a.png")))

  (setf *enemy-images* (list
                        :robtherobot
                        (load-image (renderer game) "resources/images/evil-rob-the-robot.png")))

  ;; build rooms
  (setf (rooms game)
        (make-array '(3 3)
                    :initial-contents
                    (list
                     (list (make-instance 'entrance-room) (make-instance 'game-room) (make-instance 'danger-room))
                     (list (make-instance 'breach-entrance-room) (make-instance 'game-room) (make-instance 'danger-room))
                     (list (make-instance 'breach-entrance-room) (make-instance 'game-room) (make-instance 'danger-room)))
                    ))

  ;; Default turrets are always in the room with the "player"
  ;; as a fallback anyways.
  ;; (add-turret-to-room (aref (rooms game) 0 1) (make-instance 'turret :position (vec2 55 5)))
  ;; (add-turret-to-room (aref (rooms game) 0 0) (make-instance 'turret :position (vec2 10 5)))

  (add-turret-to-room (aref (rooms game) 0 2) 2 0 (make-instance 'turret :position (vec2 (* *room-width* 2.2) 10)))
  (add-turret-to-room (aref (rooms game) 1 2) 2 1 (make-instance 'turret :position (vec2 (* *room-width* 2.2) (+ *room-max-height* 4))))
  (add-turret-to-room (aref (rooms game) 2 2) 2 2 (make-instance 'turret :position (vec2 (* *room-width* 2.2) (+ (* 2 *room-max-height*) 4))))
)

(defun draw-filled-bar-range (renderer
                              value
                              &key
                                position
                                size
                                fill-color
                                background-color)
  (draw-filled-rectangle
   renderer
   (rectangle (vec2-x position)
              (vec2-y position)
              (vec2-x size)
              (vec2-y size))
   background-color)

  (let ((percentage-to-draw (/ (ranged-value-current value)
                               (ranged-value-max value))))
      (draw-filled-rectangle
       renderer
       (rectangle (vec2-x position)
                  (vec2-y position)
                  (floor (* (vec2-x size)
                            percentage-to-draw))
                  (vec2-y size))
       fill-color)))


(defun mainmenu-frame (game delta-time)
  (clear-color (renderer game) (color 0 0 0 255))
  (reset-camera (renderer game)) 
  ;; background scene
  (let ((light-power
          (clamp
           (round (* 128 (+ 1.2 (sin (/ (sdl2:get-ticks) 240)))))
           0 129)))
    (draw-texture (renderer game) (getf *room-images* :danger-zone)
                  :dest (rectangle 0 0 (width game) (height game))
                  :color (color light-power light-power light-power 255))
    (draw-texture (renderer game) *player-base*
                  :dest (rectangle 940 360 (* 64 5) (* 64 5))) 
    ;; TODO make head float up and down
    (draw-texture (renderer game) (getf *player-faces* :neutral)
              :dest (rectangle 940 230 (* 64 5) (* 64 5)))

    (draw-filled-rectangle (renderer game)
                           (rectangle 0 0 (width game) (height game))
                           (color 0 0 0 light-power)))
  ;; end of background scene
  (draw-texture (renderer game) *main-menu-image*
                :dest (rectangle
                       (- (/ (width game) 2) (* 150 1.25))
                       0 (* 300 1.25)
                       (* 225 1.25)))
  (draw-string (renderer game)
               "Enter To Start Game" *game-font*
               (vec2 445 470)
               :size 36)
  (draw-string (renderer game)
               "Escape To Exit to Windows" *game-font*
               (vec2 445 515)
               :size 36)

  (cond
    ((is-key-pressed (input game) :scancode-escape) (sdl2:push-event :quit))
    ((is-key-pressed (input game) :scancode-return) (setf (state game) :gameplay))))

(defun gameover-frame (game delta-time)
  (clear-color (renderer game) (color 10 10 10 255))
  (reset-camera (renderer game)) 
  (draw-string (renderer game)
               "GAME OVER" *game-font* (vec2 380 200)
               :color +color-red+
               :size 128)
  (draw-string (renderer game)
               "Enter To Restart Game" *game-font*
               (vec2 405 365)
               :size 36)
  (draw-string (renderer game)
               "Escape To Return To Main Menu" *game-font*
               (vec2 405 415)
               :size 36)
  (cond
    ((is-key-pressed (input game) :scancode-escape) (setf (state game) :mainmenu))
    ((is-key-pressed (input game) :scancode-return) (setf (state game) :gameplay))))

(defun get-player-move-direction (game)
  (when *can-move*
    (cond
      ((is-key-pressed (input game) :scancode-up)
       (vec2 0 -1))
      ((is-key-pressed (input game) :scancode-down)
       (vec2 0 1))
      ((is-key-pressed (input game) :scancode-right)
       (vec2 1 0))
      ((is-key-pressed (input game) :scancode-left)
       (vec2 -1 0))
      (t nil))))

(defun clamp-player-location (player)
  (multiple-value-bind (clamp-x changed-x-p) (clamp (vec2-x (location player)) 0 2)
    (multiple-value-bind (clamp-y changed-y-p) (clamp (vec2-y (location player)) 0 2)
      (values (vec2 clamp-x clamp-y)
              (not (or changed-x-p changed-y-p))))))

(defun update-renderer-camera (game)
  (set-camera-position (renderer game) (camera-position (game-camera game))))

(defun move-camera-to-player-location (game)
  (setf (camera-position (game-camera game))
        (vec2
         (unit (- (* (vec2-x (location (player game))) *room-width*) 1.5))
         (unit (- (* (vec2-y (location (player game))) *room-max-height*) 1)))))

;; position is a vector
(defmacro with-room ((position room) &rest body)
  (let ((x-pos (gensym))
        (y-pos (gensym)))
    `(let* ((x-pos (vec2-x ,position))
            (y-pos (vec2-y ,position))
            (,room (aref (rooms game) y-pos x-pos)))
       ,@body)))

(defun game-mouse-position (game)
  (vec2 (+ (mouse-x (input game)) (vec2-x (camera-position (game-camera game))))
        (+ (mouse-y (input game)) (vec2-y (camera-position (game-camera game))))))
(defun game-mouse-position->unit (game)
  (let ((original (game-mouse-position game)))
    (vec2 (pixel->unit (vec2-x original))
          (pixel->unit (vec2-y original)))))

(defun refund-and-destroy-turret-at (game room x y position)
  (let ((turret-under-mouse (first-turret-under-position game room (game-mouse-position->unit game))))
    (when turret-under-mouse (delete (aref (turrets room) turret-under-mouse) (turrets room)))))
(defun buy-and-build-turret-at (game room x y position)
  (let ((turret-under-mouse (first-turret-under-position game room (game-mouse-position->unit game))))
    (unless turret-under-mouse (add-turret-to-room room x y (make-instance 'turret :position (game-mouse-position->unit game))))))

(defun reset-turret-active-status-on-all-rooms (game)
  (dotimes (y 3)
    (dotimes (x 3)
      (with-room ((vec2 x y) room)
                 (reset-turret-active-status room)))))

(defun find-room-for-position (position game)
  (dotimes (y 3)
    (dotimes (x 3)
      (let ((room-rect (room-get-bounding-box (aref (rooms game) y x) x y))
            (rect (rectangle (vec2-x position) (vec2-y position) 1 1)))
        (when (rectangle-intersection room-rect rect)
          (return-from find-room-for-position (values x y))))))
  (values 0 0))

(defun first-turret-under-position (game room position)
  (position-if
   #'(lambda (item)
       (rectangle-intersection
          (rectangle (vec2-x position) (vec2-y position) 0 0)
        (get-bounding-box-turret item)))
   (turrets room)))

(defun room-has-enemy (game location)
  (dotimes (enemy-index (length (enemies game)))
    (let ((current-enemy (aref (enemies game) enemy-index)))
      (when (vec2-= (location-room-position->vec2 (location current-enemy)) location)
        (return-from room-has-enemy t))))
  (return-from room-has-enemy nil))

(defun draw-minimap (game renderer position)
  (let ((rect-width 80)
        (rect-height 50))
    (dotimes (y 3)
      (dotimes (x 3)
        (let ((rect (rectangle (+ (vec2-x position) (* x rect-width))
                               (+ (vec2-y position) (* y rect-height))
                               rect-width
                               rect-height)))
        (with-room ((location (player game)) player-room)
                   (with-room ((vec2 x y) room)
                              (cond ((and (eq room player-room) (room-has-enemy game (vec2 x y)))
                                     (draw-filled-rectangle renderer rect +color-green+))
                                    ((room-has-enemy game (vec2 x y))
                                     (draw-filled-rectangle renderer rect +color-red+))
                                    ((eq room player-room)
                                     (draw-rectangle renderer rect +color-green+))
                                    (t (draw-rectangle renderer rect +color-red+))))))))))

(defun gameplay-frame (game delta-time)
  (clear-color (renderer game) (color 10 10 20 255))
  ;; Game play elements
  
  (update-renderer-camera game)
  (move-camera-to-player-location game)

  ;; player "movement"
  (let ((move-dir (get-player-move-direction game)))
    (when move-dir
      (vec2-incf (location (player game)) move-dir)
      (multiple-value-bind (new-location moved-p) (clamp-player-location (player game)) 
        (vec2-decf (location (player game)) move-dir)  ;; stupid hack to undo movement for looks
        (when moved-p
          (reset-turret-active-status-on-all-rooms game)
          (setf *can-move* nil)
          (start-fade (fader game)
                      :length 0.15
                      ;; :linger-length 0.4 Should rename to delay?
                      :color +color-black+ 
                      :direction :fade-out
                      :on-finish #'(lambda ()
                                     (setf (location (player game)) new-location)
                                     (start-fade (fader game)
                                                 :length 0.25
                                                 :linger-length 0.4
                                                 :color +color-black+
                                                 :direction :fade-in
                                                 :on-finish #'(lambda ()
                                                                (setf *can-move* t)))))))))


  ;; player input

  ;; DEBUG!
  (when (is-key-pressed (input game) :scancode-e)
    (dotimes (enemy-index (length (enemies game)))
      (setf (ranged-value-current (health (aref (enemies game) enemy-index))) 0)))
  (when (is-key-pressed (input game) :scancode-r)
    (setf (ranged-value-current (energy (player game))) 100))

  (when (is-key-pressed (input game) :scancode-escape)
    (setf (state game) :mainmenu))
  (if (and (round-started game) (or (> (remaining-enemies-to-spawn game) 0) (length (enemies game))))
      (progn
        (if (can-spawn-enemy game)
            (progn
              (spawn-enemy game :floor (random-room-floor)
                                :type (random-type-of-enemy game)))
            (decf (enemy-spawn-cooldown game) delta-time))

        (with-room ((location (player game)) room)
                   (when (is-key-pressed (input game) :scancode-a)
                     (play-sound *test-sound-chunk*)
                     (ranged-value-decf (health (player game)) 15))
                   (when (is-key-pressed (input game) :scancode-space)
                     (toggle-door room))
                   ;; check mouse stuff I guess.

                   (let ((turret-under-mouse (first-turret-under-position game room (game-mouse-position->unit game))))
                     (cond ((and turret-under-mouse (is-mouse-left-down (input game)))
                            (reset-turret-active-status-on-all-rooms game)
                            (setf (active (aref (turrets room) turret-under-mouse)) t))
                           ((is-mouse-left-down (input game))
                            (let ((active-turret (position-if #'turret-active-p (turrets room))))
                              (when active-turret
                                (fire-turret (aref (turrets room) active-turret) game (game-mouse-position->unit game)))))))))
      (end-round game))
 
  (multiple-value-bind (x y) (find-room-for-position (game-mouse-position->unit game) game)
    (unless (round-started game)
      (with-room ((vec2 x y) room)
                 (cond
                   ((is-mouse-right-clicked (input game))
                    (refund-and-destroy-turret-at game room x y (game-mouse-position->unit game)))
                   ((is-mouse-left-clicked (input game))
                    (buy-and-build-turret-at game room x y (game-mouse-position->unit game)))))))

  (when (is-key-pressed (input game) :scancode-space)
    (start-round game))

  (dotimes (y 3)
    (dotimes (x 3)
      (with-room ((vec2 x y) room)
                 (update-room room game delta-time) 
                 (draw-room room (renderer game) x y))))

  ;; eh I should for each this
  (dotimes (projectile-index (length (projectiles game)))
    (update-projectile (aref (projectiles game) projectile-index) game delta-time)
    (draw-projectile (aref (projectiles game) projectile-index) (renderer game)))
  (delete-if #'projectile-dead-p (projectiles game))

  (dotimes (enemy-index (length (enemies game)))
    (update-enemy (aref (enemies game) enemy-index) game delta-time)
    (draw-enemy (aref (enemies game) enemy-index) (renderer game)))
  ;; fixed 200 points I suppose.
  (incf (score game) (* 200 (count-if #'enemy-dead-p (enemies game))))
  (delete-if #'enemy-dead-p (enemies game))
  

  ;; UI
  (reset-camera (renderer game)) 
  (let ((light-power
          (clamp
           (round (* 128 (+ 1.2 (sin (/ (sdl2:get-ticks) 240)))))
           0 129)))
    (draw-filled-rectangle (renderer game)
                           (rectangle 0 0 (width game) (height game))
                           (color 0 0 0 light-power)))

  (draw-filled-bar-range (renderer game) (health (player game))
                         :position (vec2 10 10)
                         :size (vec2 600 30)
                         :fill-color +color-green+
                         :background-color +color-red+)
  (draw-string (renderer game)
               (format nil "(~a, ~a)"
                       (ranged-value-current (health (player game)))
                       (ranged-value-max (health (player game))))
               *game-font*
               (vec2 20 15)
               :size 24
               :color +color-black+)

  (draw-filled-bar-range (renderer game) (energy (player game))
                         :position (vec2 10 50)
                         :size (vec2 600 30)
                         :fill-color +color-blue+
                         :background-color +color-red+)

  (draw-minimap game (renderer game) (vec2 1020 10))

  (if (round-started game)
      (draw-string (renderer game)
                   (format nil "(~a, ~a)"
                           (ranged-value-current (energy (player game)))
                           (ranged-value-max (energy (player game))))
                   *game-font*
                   (vec2 20 55)
                   :size 24))

  (draw-string (renderer game)
               (format nil "room (~a, ~a)"
                       (vec2-x (location (player game)))
                       (vec2-y (location (player game))))
               *game-font*
               (vec2 630 0)
               :size 30)
  (draw-string (renderer game)
               (format nil "POINTS : ~a" (points game))
               *game-font*
               (vec2 700 90)
               :size 40)
  (draw-string (renderer game)
               (format nil "SCORE : ~a" (score game))
               *game-font*
               (vec2 700 45)
               :size 40)

  (with-room ((location (player game)) room)
             (draw-string (renderer game)
                          (write-to-string (aref (turrets room) 0))
                          *game-font*
                          (vec2 0 518)
                          :size 16)
             (draw-string (renderer game)
                          (write-to-string (length (turrets room)))
                          *game-font*
                          (vec2 0 534)
                          :size 16))

  (when (player-dead-p (player game))
    (start-fade (fader game)
                :length 1.25
                :color (color 255 0 0 255)
                :direction :fade-out
                :on-finish #'(lambda ()
                               (setf (state game) :gameover)
                               (start-fade (fader game)
                                           :length 1.25
                                           :color (color 255 0 0 255)
                                           :direction :fade-in)))))

(defmethod window-frame ((game game) delta-time)
  (set-blend-mode (renderer game) :blend)
  (case (state game)
    (:gameplay (gameplay-frame game delta-time))
    (:gameover (gameover-frame game delta-time))
    (:mainmenu (mainmenu-frame game delta-time)))
  ;; I don't particularly like this but whatever.
  (screen-fade-draw (fader game) (renderer game))
  (screen-fade-update (fader game) delta-time))

(defmethod window-quit ((game game))
  (renderer-destroy (renderer game))
  (show-simple-message-box
   :information
   "Thanks for playing!"
   "Goodbye and have a nice day!"))
