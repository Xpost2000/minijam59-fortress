(in-package :mjgame)

(defstruct key-state
  (clicks 0)
  (down nil))

(defstruct mouse-state
  (position (vec2 0 0))
  (left-down nil)
  (right-down nil)
  (middle-down nil))

(defstruct input-state
  (mouse (make-mouse-state))
  (keys (make-hash-table)))

(defclass input-handler ()
  ((current-state :accessor current-state
                  :initarg :current-state
                  :initform (make-input-state))
   (last-state :accessor last-state
               :initarg :last-state
               :initform (make-input-state))))

(defun input-handle-key-down (input key)
  (if (gethash (sdl2:scancode-value key)
               (input-state-keys
                (current-state input)))
      (setf (key-state-down (gethash (sdl2:scancode-value key)
                                     (input-state-keys
                                      (current-state input)))) t)
      (setf (gethash (sdl2:scancode-value key)
                     (input-state-keys (current-state input)))
            (make-key-state :clicks 0 :down t))))

(defun input-handle-key-up (input key)
  (let ((key-slot (gethash (sdl2:scancode-value key)
                           (input-state-keys
                            (current-state input)))))
    (if key-slot
        (setf (key-state-down key-slot) nil)
        (setf (gethash (sdl2:scancode-value key)
                       (input-state-keys (current-state input)))
              (make-key-state :clicks 0 :down nil)))))

;; BAD!
(defun input-handle-mouse-button-up (input button state clicks)
  (case button
    (1
     (setf (mouse-state-left-down
            (input-state-mouse
             (current-state input))) nil))
    (2
     (setf (mouse-state-middle-down
            (input-state-mouse
             (current-state input))) nil))
    (3
     (setf (mouse-state-right-down
            (input-state-mouse
             (current-state input))) nil))))

(defun input-handle-mouse-button-down (input button state clicks)
  (case button
    (1
     (setf (mouse-state-left-down
            (input-state-mouse
             (current-state input))) t))
    (2
     (setf (mouse-state-middle-down
            (input-state-mouse
             (current-state input))) t))
    (3
     (setf (mouse-state-right-down
            (input-state-mouse
             (current-state input))) t))))

(defun input-handle-mouse-motion (input x y)
  (setf (mouse-state-position (input-state-mouse (current-state input))) (vec2 x y)))

(defun input-new-frame (input)
  (setf (last-state input) (copy-structure (current-state input)))
  (setf (current-state input) (make-input-state)))

(defun mouse-delta-y (input)
  ;; TODO
  )
(defun mouse-delta-x (input)
  ;; TODO
  )

(defun mouse-y (input)
  (vec2-y (mouse-state-position (current-state input))))
(defun mouse-x (input)
  (vec2-x (mouse-state-position (current-state input))))
(defun mouse-position (input)
  (vec2 (mouse-x input) (mouse-y input)))

(defun last-is-key-down (input scancode)
  (let ((key (gethash (sdl2:scancode-key-to-value scancode)
                      (input-state-keys (last-state input)))))
    (when key (key-state-down key))))

(defun is-key-down (input scancode) 
  (let ((key (gethash (sdl2:scancode-key-to-value scancode)
                      (input-state-keys (current-state input)))))
    (when key (key-state-down key))))

;; why does this not work properly?
;; ???????????????????????????
;; how in the world does this manage to crash?
;; (defun is-key-pressed (input scancode)
;;   (let ((last-down (last-is-key-down input scancode))
;;         (current-down (is-key-pressed input scancode)))
;;     (and last-down current-down)
;;     (cond
;;       ((and last-down current-down) nil)
;;       ((and (not last-down) current-down) t)
;;       ((and last-down (not current-down)) nil))
;;   ))

(defun is-mouse-left-down (input)
  (mouse-state-left-down (input-state-mouse (current-state input))))
(defun is-mouse-middle-down (input)
  (mouse-state-middle-down (input-state-mouse (current-state input))))
(defun is-mouse-right-down (input)
  (mouse-state-right-down (input-state-mouse (current-state input))))

(defun mouse-buttons (input)
  (values (is-mouse-left-down input)
          (is-mouse-middle-down input)
          (is-mouse-right-down input)))
