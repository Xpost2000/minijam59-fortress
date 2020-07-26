(in-package :mjgame)

(defparameter *loaded-sounds* (make-hash-table))
(defparameter *loaded-music* (make-hash-table))
(defun stop-channel (channel) (sdl2-mixer:halt-channel channel))
(defun stop-music () (sdl2-mixer:halt-music))
;; chunk object
(defun play-sound (sound &optional (channel -1)) (sdl2-mixer:play-channel channel sound 0))
;; music object
(defun play-music (music) (sdl2-mixer:play-music music))
(defun load-music (name)
  (if (gethash name *loaded-music*)
      (gethash name *loaded-music*)
      (setf (gethash name *loaded-music*) (sdl2-mixer:load-music name))))
(defun load-sound (name)
  (if (gethash name *loaded-sounds*)
      (gethash name *loaded-sounds*)
      (setf (gethash name *loaded-sounds*) (sdl2-mixer:load-wav name))))
