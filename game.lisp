(in-package :mjgame)

(defclass game (window) ())

(defmethod window-setup ((game game))
  )

(defmethod window-frame ((game game) delta-time)
  )

(defmethod window-quit ((game game))
  (renderer-destroy (renderer game))
  (show-simple-message-box
   :information
   "Thanks for playing!"
   "Goodbye and have a nice day!"))
