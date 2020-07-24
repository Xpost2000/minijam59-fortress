(in-package :mjgame)

(defun calculate-frame-delta-time (start end) (* (- end start) 0.0001))

;; eh
;; I'm sure there's a better way to do this.
;; but idk how to read cepl or trial's stuff.
(defclass window ()
  ((name :accessor name
         :initarg :name
         :initform "window")
   (width :accessor width
          :initarg :width
          :initform 1024)
   (height :accessor height
           :initarg :height
           :initform 768)
   (fullscreen :accessor fullscreen
               :initarg :fullscreen
               :initform nil)
   (input :accessor input
          :initarg :input
          :initform (make-instance 'input-handler))
   (window-instance :accessor window
                    :initarg window-instance)
   (renderer :accessor renderer
             :initarg :renderer)))

(defgeneric window-frame (window delta-time))
(defgeneric window-setup (window))
(defgeneric window-quit (window))

(defun window-run (window)
  (sdl2:with-window (window-instance
                     :title (name window)
                     :w (width window)
                     :h (height window))
    #+windows ;; Weird slime thing on windows...
    (progn
      (sdl2:hide-window window-instance)
      (sdl2:show-window window-instance))
    (setf (window window) window-instance)
    (setf (renderer window) (make-instance 'renderer :screen window-instance))
    (sdl2:set-window-fullscreen window-instance (fullscreen window))
    (window-setup window)
    (let ((delta-time 0.0))
      (sdl2:with-event-loop (:method :poll)
        (:mousemotion (:x mouse-x :y mouse-y)
                      (input-handle-mouse-motion (input window) mouse-x mouse-y))
        (:mousebuttonup (:button button :state state :clicks clicks)
                        (input-handle-mouse-button-up (input window) button state clicks))
        (:mousebuttondown (:button button :state state :clicks clicks)
                          (input-handle-mouse-button-down (input window) button state clicks))
        (:keydown (:keysym keysym)
                  (input-handle-key-down (input window) keysym))
        (:keyup (:keysym keysym)
                (input-handle-key-up (input window) keysym))
        (:idle ()
               (let ((start-time (sdl2:get-ticks)))
                 (window-frame window delta-time)
                 (present-frame (renderer window))
                 (input-new-frame (input window))
                 (setf delta-time (calculate-frame-delta-time
                                   start-time (sdl2:get-ticks)))))
        (:quit () (window-quit window) t)))))

(defmacro with-window ((name type
                        &key (title "title")
                          (width 1024)
                          (height 768)
                          (fullscreen nil))
                       &rest body)
  `(let ((,name (make-instance ',type
                               :name ,title
                               :width ,width
                               :height ,height
                               :fullscreen ,fullscreen)))
     ,@body))
(defmacro open-window (type
                       &key
                         (title "title")
                         (width 1024)
                         (height 768)
                         (fullscreen nil))
  (let ((window-symbol (gensym "window")))
    `(with-window (window-symbol
                   ,type
                   :title ,title
                   :width ,width
                   :height ,height
                   :fullscreen ,fullscreen)
                (window-run window-symbol))))
