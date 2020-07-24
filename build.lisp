;; build n die
(in-package :mjgame)

(defun build-game ()
  #+(and sbcl windows)
  (progn
    (sb-ext:save-lisp-and-die "dist/mj59game.exe"
                              :toplevel #'main
                              :executable t
                              :application-type :gui)))
