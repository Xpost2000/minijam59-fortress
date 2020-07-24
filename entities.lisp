(in-package :mjgame)
;; Or anything that looks like an entity.
;; things?

;; overseer?
(defclass player ()
  ((health :accessor health
           :initform (ranged 0 100 :max))
   (energy :accessor energy
           :initform (ranged 0 100 :max))))

;; Incase I actually make this a real "entity" thing
(defclass screen-fade ()
  ((timer :accessor timer
          :initform 0.0)
   (fade-length :accessor fade-length
           :initarg :fade-length
           :initform 0.0)
   (active :accessor active
           :initarg :active
           :initform nil)
   (direction :accessor direction
              :initarg :direction
              :initform :fade-out)
   (fade-color :accessor fade-color
          :initarg :fade-color
          :initform +color-black+)
   ;; callback
   (on-finish :accessor on-finish
              :initarg on-finish)))

(defun start-fade (fader
                   &key
                     length
                     (direction :fade-out)
                     (color +color-black+)
                     on-finish)
  (unless (active fader)
    (setf (active fader) t)
    (setf (direction fader) direction)
    (setf (fade-length fader) length)
    (setf (on-finish fader) on-finish)
    (setf (fade-color fader) color)
    (setf (timer fader) (fade-length fader))))

(defun screen-fade-draw (fader renderer)
  ;; no renderlayer or command buffer to defer stuff. so this...
  (reset-camera renderer)
  (when (active fader)
    (let ((alpha (cond
                   ((eql (direction fader) :fade-in)
                    (round (* 255 (/ (timer fader) (fade-length fader)))))
                   ((eql (direction fader) :fade-out)
                    (round (* 255 (/ (- (fade-length fader) (timer fader))
                                     (fade-length fader))))))))
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
