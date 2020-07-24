(in-package :mjgame)

(defparameter *game-font* nil)
(defparameter *game-window* nil) ;; context
(defun unit (x) (* (/ (width *game-window*) 50) x))

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
(defun ranged-value-subtract (ranged val)
  (ranged (ranged-value-min ranged)
          (ranged-value-max ranged)
          (- (ranged-value-current ranged) val)))
(defun ranged-value-add (ranged val)
  (ranged (ranged-value-min ranged)
          (ranged-value-max ranged)
          (+ (ranged-value-current ranged) val)))

(defclass game (window)
  ((fader :accessor fader
          :initform (make-instance 'screen-fade)) 
   (state :accessor state
          :initform :gameplay)
   (player :accessor player
           :initform (make-instance 'player))))

(defmethod window-setup ((game game))
  ;; this context is nasty...
  (setf *game-window* game)
  (setf *game-font* (load-font (renderer game)
                               "resources/fonts/PxPlus_IBM_VGA8.ttf" 98)))

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

(defun gameplay-frame (game delta-time)
  (clear-color (renderer game) (color 10 10 20 255))

  (if (is-key-pressed (input game) :scancode-a)
      (setf (health (player game))
            (ranged-value-subtract (health (player game)) 15)))

  (draw-filled-bar-range (renderer game) (health (player game))
                         :position (vec2 10 10)
                         :size (vec2 800 60)
                         :fill-color +color-green+
                         :background-color +color-red+)

  (draw-filled-bar-range (renderer game) (energy (player game))
                         :position (vec2 10 80)
                         :size (vec2 800 60)
                         :fill-color +color-blue+
                         :background-color +color-red+)

  (draw-filled-rectangle (renderer game)
                         (rectangle (unit 2)
                                    (unit 2)
                                    (unit 8)
                                    (unit 8)))

  (when (< (ranged-value-current (health (player game))) 0)
    (start-fade (fader game) :length 3.0
                :direction :fade-out
                :on-finish #'(lambda () (setf (state game) :gameover)))))

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
