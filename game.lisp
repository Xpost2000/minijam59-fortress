;; day 0 is just setting up platform and basic framework stuff.
;; this is just a really barebones framework that can draw stuff
;; play audio and some input maybe
;; day 1 is actual game stuff though.
(in-package :mjgame)
;;
;; I actually need this to be resolution independent as the first thing
;; otherwise I'll totally be boned...
;; Also I'm quite new to common lisp and I mostly did C so it's very
;; imperative-ish, or it's not very CLOS-y
;;
(defparameter *screen-width* nil)
(defparameter *screen-height* nil)
(defparameter *screen-fullscreen-mode* nil)

(defun main()
  (sdl2:with-init (:video)
    (sdl2-image:init '(:png))
    (sdl2-ttf:init)
    (sdl2-mixer:init :wave :ogg)
    (sdl2-mixer:open-audio 44100 :s16sys 2 4096)
    (handle-basic-launcher) 
    (sdl2:with-window (game-window
                       :title "Minijam 59 : Fortress"
                       :w *screen-width*
                       :h *screen-height*)
      #+windows ;; Weird slime thing on windows...
      (progn
        (sdl2:hide-window game-window)
        (sdl2:show-window game-window))

      (sdl2:set-window-fullscreen game-window *screen-fullscreen-mode*)
      (sdl2:with-event-loop (:method :poll)
        (:mousemotion (:x mouse-x
                       :y mouse-y)
                      )
        (:mousebuttondown (:button button
                           :state state
                           :clicks clicks)
                          )
        (:keydown (:keysym keysym)
                  )
        (:keyup (:keysym keysym)
                )
        (:idle ()
               )
        (:quit () (show-simple-message-box
                   :information
                   "Thanks for playing!"
                   "Goodbye and have a nice day!"))))
    (sdl2-mixer:close-audio)
    (sdl2-mixer:quit)
    (sdl2-ttf:quit)
    (sdl2-image:quit)))
