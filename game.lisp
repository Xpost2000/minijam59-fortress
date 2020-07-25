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

(defclass game (window)
  ((camera :accessor game-camera
           :initform (make-camera))
   (fader :accessor fader
          :initform (make-instance 'screen-fade)) 
   (state :accessor state
          :initform :gameplay)
   (rooms :accessor rooms)
   (player :accessor player
           :initform (make-instance 'player))))

(defmethod window-setup ((game game))
  ;; this context is nasty...
  (setf *game-window* game)
  (setf *game-font* (load-font (renderer game)
                               "resources/fonts/PxPlus_IBM_VGA8.ttf" 98))

  (setf *test-sound-chunk* (sdl2-mixer:load-wav "resources/sound/wave_finished.wav"))
  (print *test-sound-chunk*)

  ;; build rooms
  (setf (rooms game)
        (make-array '(3 3)
                    :initial-contents
                    (list
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room))
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room))
                     (list (make-instance 'game-room) (make-instance 'game-room) (make-instance 'game-room)))
         )))

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
  )

(defun gameover-frame (game delta-time)
  (clear-color (renderer game) (color 10 10 10 255))
  (draw-string (renderer game)
               "GAME OVER" *game-font* (vec2 0 0)
               :color +color-green+))

(defparameter *can-move* t)

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
       ,@body
       )))

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
        (setf (location (player game)) new-location)
        (when moved-p
          (setf *can-move* nil)
          (start-fade (fader game)
                      :length 0.15
                      ;; :linger-length 0.4 Should rename to delay?
                      :color +color-black+ 
                      :direction :fade-out
                      :on-finish #'(lambda ()
                                     (start-fade (fader game)
                                                 :length 0.25
                                                 :linger-length 0.2
                                                 :color +color-black+
                                                 :direction :fade-in
                                                 :on-finish #'(lambda ()
                                                                (setf *can-move* t)))))
          ))))


  ;; player input

  ;; DEBUG!
  (when (is-key-pressed (input game) :scancode-r)
    (setf (ranged-value-current (energy (player game))) 100))

  (with-room ((location (player game)) room)
    (when (is-key-pressed (input game) :scancode-a)
      (sdl2-mixer:play-channel -1 *test-sound-chunk* 0) 
      (ranged-value-decf (health (player game)) 15))
    (when (is-key-pressed (input game) :scancode-space)
      (toggle-door room)))

  (dotimes (y 3)
    (dotimes (x 3)
      (with-room ((vec2 x y) room)
                 (update-room room game delta-time) 
                 (draw-room room (renderer game) x y))))
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

  (when (< (ranged-value-current (health (player game))) 0)
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
