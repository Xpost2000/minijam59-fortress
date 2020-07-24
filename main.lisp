;; a bit weird but okay?
(in-package :mjgame)

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)
(defparameter *screen-fullscreen-mode* nil)

(defparameter *resolution-picker-width* 800)
(defparameter *resolution-picker-height* 600)

(defun main ()
  (with-initialize-sdl
      ;; (open-window launcher :title "Game Launcher"
      ;;                       :width *resolution-picker-width*
      ;;                       :height *resolution-picker-height*)

      (open-window game :title "minijam 59 : FORTRESS"
                             :width *screen-width*
                             :height *screen-height*
                             :fullscreen *screen-fullscreen-mode*)

        ;; (let ((game (make-instance 'game
        ;;                            :fullscreen *screen-fullscreen-mode*
        ;;                            :name "minijam 59 : FORTRESS"
        ;;                            :width *screen-width*
        ;;                            :height *screen-height*)))
        ;;   (window-run game))
    ))
