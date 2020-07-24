(in-package :mjgame)
;; Minor thingy since cl-sdl2 doesn't seem to expose it,
;; probably not very important.

;; type should be a keyword
(defun show-simple-message-box (type title message &optional (window nil))
  (let ((message-type
          (case type
            (:error #x10)
            (:warning #x20)
            (:information #x40)
            (otherwise 0))))
    (sdl2::sdl-show-simple-message-box
     message-type title message window)))
