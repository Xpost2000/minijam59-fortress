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

(defclass game (window)
  ((camera :accessor game-camera
           :initform (make-camera))
   (fader :accessor fader
          :initform (make-instance 'screen-fade)) 
   (state :accessor state
          :initform :mainmenu)
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

;; TODO game reset!
(defmethod window-setup ((game game))
  (setf *can-move* t)
  ;; this context is nasty...
  (setf *game-window* game)
  (setf *game-font* (load-font (renderer game)
                               "resources/fonts/PxPlus_IBM_VGA8.ttf" 98))

  (setf *test-sound-chunk* (sdl2-mixer:load-wav "resources/sound/wave_finished.wav"))
  (print *test-sound-chunk*)

  (setf *main-menu-image* (load-image (renderer game) "resources/images/title.png"))
  (setf *player-base* (load-image (renderer game) "resources/images/player-altar.png"))
  (setf *player-faces* (list
                        :neutral (load-image (renderer game) "resources/images/player-face-neutral.png")
                        :hurt (load-image (renderer game) "resources/images/player-face-hurt.png")))
  (setf *room-images* (list
                         (load-image (renderer game) "resources/images/room-danger.png")
                         (load-image (renderer game) "resources/images/room-bunker.png")
                         (load-image (renderer game) "resources/images/vault_entrance_a.png")))

  ;; build rooms
  (setf (rooms game)
        (make-array '(3 3)
                    :initial-contents
                    (list
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room))
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room))
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room)))
                    ))
  ;; room y is the only argument that actually affects our enemy.
  ;; I can make it so room-x also does something
  ;; (vector-push (make-instance 'enemy :location (make-room-location :y 0)) (enemies game))
  ;; (vector-push (make-instance 'enemy :location (make-room-location :y 1)) (enemies game))
  ;; (vector-push (make-instance 'enemy :location (make-room-location :y 2)) (enemies game))
  (add-turret-to-room (aref (rooms game) 0 1) (make-instance 'turret :position (vec2 55 5)))
  (add-turret-to-room (aref (rooms game) 0 0)
                      (make-instance 'turret :position (vec2 10 5))))

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
    (draw-texture (renderer game) (elt *room-images* 0)
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
                                                                (setf *can-move* t)))))
          ))))


  ;; player input

  ;; DEBUG!
  (when (is-key-pressed (input game) :scancode-r)
    (setf (ranged-value-current (energy (player game))) 100))

  (when (is-key-pressed (input game) :scancode-escape)
    (setf (state game) :mainmenu))

  (with-room ((location (player game)) room)
    (when (is-key-pressed (input game) :scancode-a)
      (sdl2-mixer:play-channel -1 *test-sound-chunk* 0) 
      (ranged-value-decf (health (player game)) 15))
    (when (is-key-pressed (input game) :scancode-space)
      (toggle-door room))
    ;; check mouse stuff I guess.

    (let ((turret-under-mouse (position-if
                               #'(lambda (item)
                                   (rectangle-intersection
                                    (let ((mouse-x (vec2-x (game-mouse-position game)))
                                          (mouse-y (vec2-y (game-mouse-position game))))
                                      (rectangle (pixel->unit mouse-x)
                                                 (pixel->unit mouse-y) 0 0))
                                    (get-bounding-box-turret item)))
                               (turrets room))))
      (cond ((and turret-under-mouse (is-mouse-left-down (input game)))

             (dotimes (y 3)
               (dotimes (x 3)
                 (with-room ((vec2 x y) room)
                            (reset-turret-active-status room))))
             (setf (active (aref (turrets room) turret-under-mouse)) t))
            ((is-mouse-left-down (input game))
             (let ((active-turret (position-if #'turret-active-p (turrets room))))
               (when active-turret
                 (fire-turret (aref (turrets room) active-turret) game (game-mouse-position->unit game))))))))

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
  (delete-if #'enemy-dead-p (enemies game))
  (draw-filled-rectangle
   (renderer game)
   (let ((mouse-x (vec2-x (game-mouse-position game)))
         (mouse-y (vec2-y (game-mouse-position game))))
     (rectangle mouse-x
                mouse-y 16 16)))

  ;; UI
  (reset-camera (renderer game)) 
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
  (draw-string (renderer game)
               (format nil "(~a, ~a)"
                       (ranged-value-current (energy (player game)))
                       (ranged-value-max (energy (player game))))
               *game-font*
               (vec2 20 55)
               :size 24)

  (draw-string (renderer game)
               (format nil "room (~a, ~a)"
                       (vec2-x (location (player game)))
                       (vec2-y (location (player game))))
               *game-font*
               (vec2 630 0)
               :size 30)
  (draw-string (renderer game)
               "*INSERT SCORE* THEN *POINTS TO BUY STUFF*"
               *game-font*
               (vec2 700 45)
               :size 40)

  (let ((mouse-x (mouse-x (input game)))
        (mouse-y (mouse-y (input game))))
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
      (draw-string (renderer game)
                   (write-to-string (vec2 (pixel->unit mouse-x)
                                          (pixel->unit mouse-y)))
                   *game-font*
                   (vec2 0 500)
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
  (sdl2-mixer:free-chunk *test-sound-chunk*)
  (show-simple-message-box
   :information
   "Thanks for playing!"
   "Goodbye and have a nice day!"))
