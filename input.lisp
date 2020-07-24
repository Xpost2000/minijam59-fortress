(in-package :mjgame)

(defstruct mouse-state
  (position (vec2 0 0))
  (left-down nil)
  (right-down nil)
  (middle-down nil))

(defstruct input-state
  (mouse (make-mouse-state))
  (keys (make-hash-table)))

(defun input-state-is-key-down (input-state key)
  (gethash (sdl2:scancode-key-to-value key) (input-state-keys input-state)))

;; convenience...
(defun input-state-mouse-left-down (input-state)
  (mouse-state-left-down (input-state-mouse input-state)))
(defun input-state-mouse-middle-down (input-state)
  (mouse-state-middle-down (input-state-mouse input-state)))
(defun input-state-mouse-right-down (input-state)
  (mouse-state-right-down (input-state-mouse input-state)))

(defun input-state-mouse-x (input-state)
  (vec2-x (mouse-state-position (input-state-mouse input-state))))
(defun input-state-mouse-y (input-state)
  (vec2-y (mouse-state-position (input-state-mouse input-state))))
(defun input-state-mouse-position (input-state)
  (mouse-state-position (input-state-mouse input-state)))

(defclass input-handler ()
  ((current-state :accessor current-state
                  :initarg :current-state
                  :initform (make-input-state))
   (last-state :accessor last-state
               :initarg :last-state
               :initform (make-input-state))))

(defun input-handle-key-down (input key)
  ;; is copy elision a thing here
  (setf (gethash (sdl2:scancode-value key)
                 (input-state-keys (current-state input))) t))

(defun input-handle-key-up (input key)
  ;; is copy elision a thing here
  (setf (gethash (sdl2:scancode-value key)
                 (input-state-keys (current-state input))) nil))

;; BAD!
(defmacro find-button ((button button-in) &rest body)
  `(let ((,button (case ,button-in
                    (1 'left-down) (2 'middle-down) (3 'middle-down))))
     ,@body))
(defun input-handle-mouse-button-up (input button state clicks)
  (find-button (button button)
               (setf (slot-value (input-state-mouse
                                  (current-state input)) button) nil)))

(defun input-handle-mouse-button-down (input button state clicks)
  (find-button (button button)
               (setf (slot-value (input-state-mouse
                                  (current-state input)) button) t)))

(defun input-handle-mouse-motion (input x y)
  (setf (mouse-state-position
         (input-state-mouse (current-state input))) (vec2 x y)))

;; Had to google this, apparently there are no deep copies of
;; structs? (I understand for classes but structs are also "references?")
(defun clone-input-struct (original)
  (make-input-state
   :mouse
   (make-mouse-state
    :position (slot-value (input-state-mouse original) 'position)
    :left-down (slot-value (input-state-mouse original) 'left-down)
    :middle-down (slot-value (input-state-mouse original) 'middle-down)
    :right-down (slot-value (input-state-mouse original) 'right-down))
   :keys
   (alexandria:copy-hash-table (input-state-keys original))))

(defun input-new-frame (input)
  (setf (last-state input) (clone-input-struct (current-state input))))

(defun mouse-delta-y (input)
  ;; TODO
  )
(defun mouse-delta-x (input)
  ;; TODO
  )

(defun mouse-x (input)
  (input-state-mouse-x (current-state input)))
(defun mouse-y (input)
  (input-state-mouse-y (current-state input)))
(defun mouse-position (input)
  (input-state-mouse-position (current-state input)))

(defun is-key-down (input scancode) 
  (input-state-is-key-down (current-state input) scancode))

(defun is-key-pressed (input scancode)
  (and (input-state-is-key-down (last-state input) scancode)
       (not (input-state-is-key-down (current-state input) scancode))))

(defun is-mouse-left-down (input)
  (input-state-mouse-left-down (current-state input)))
(defun is-mouse-middle-down (input)
  (input-state-mouse-middle-down (current-state input)))
(defun is-mouse-right-down (input)
  (input-state-mouse-right-down (current-state input)))

(defun mouse-buttons (input)
  (values (is-mouse-left-down input)
          (is-mouse-middle-down input)
          (is-mouse-right-down input)))
