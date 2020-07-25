(in-package :mjgame)

(defun start-fade (fader
                   &key
                     length
                     (linger-length 0.0)
                     (direction :fade-out)
                     (color +color-black+)
                     on-finish)
  (unless (active fader)
    (setf (active fader) t)
    (setf (direction fader) direction)
    (setf (fade-length fader) length)
    (setf (on-finish fader) on-finish)
    (setf (fade-color fader) color)
    (setf (timer fader) (+ (fade-length fader) linger-length))))

;; if I have time I guess.
#+- (defmacro fade-out-and-in-transition (&key
                                        color
                                        fade-in-length
                                        fade-in-linger-length
                                        ;;on-fade-in-finish

                                        (fade-out-length fade-in-length)
                                        (fade-out-linger-length fade-in-linger-length)
                                        on-fade-out-finish)
  )

(defun screen-fade-draw (fader renderer)
  ;; no renderlayer or command buffer to defer stuff. so this...
  (reset-camera renderer)
  (when (active fader)
    (let ((alpha (cond
                   ((eql (direction fader) :fade-in)
                    (round (* 255
                              (clamp (/ (timer fader) (fade-length fader))
                                     0 1))))
                   ((eql (direction fader) :fade-out)
                    (round (* 255
                              (clamp (/ (- (fade-length fader) (timer fader))
                                        (fade-length fader))
                                     0 1)))))))
      (draw-filled-rectangle renderer
                             (rectangle 0 0
                                        (screen-width renderer)
                                        (screen-height renderer))
                             (color (color-r (fade-color fader))
                                    (color-g (fade-color fader))
                                    (color-b (fade-color fader))
                                    alpha)))))

(defun screen-fade-update (fader delta-time)
  (when (active fader)
    (decf (timer fader) delta-time)
    (when (<= (timer fader) 0.0)
      (setf (active fader) nil)
      (when (functionp (on-finish fader)) 
        (funcall (on-finish fader))))))
