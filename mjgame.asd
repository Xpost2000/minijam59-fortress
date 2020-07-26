(asdf:defsystem #:mjgame
  :name "mjgame"
  :version "jam version"
  :author "Jerry Zhu / Xpost2000"
  :license "MIT License"

  :serial t
  :depends-on (#:sdl2
               #:sdl2-image
               #:sdl2-mixer
               #:sdl2-ttf)

  :components ((:file "package")
               (:file "utils")
               (:file "show-messagebox")
               (:file "vec2")
               (:file "audio")
               (:file "input"
                :depends-on ("vec2"))
               (:file "rectangle"
                :depends-on ("vec2"))
               (:file "sdl-renderer"
                :depends-on ("rectangle"
                             "vec2"))
               (:file "window"
                :depends-on ("input"
                             "sdl-renderer"))
               (:file "launcher"
                :depends-on ("window"
                             "utils"))
               (:file "entities")
               (:file "game"
                :depends-on ("window"
                             "entities"))
               (:file "projectile"
                :depends-on ("game"
                             "entities"))
               (:file "turret"
                :depends-on ("game"
                             "entities"))
               (:file "rooms"
                :depends-on ("game"
                             "entities"
                             "turret"))
               (:file "screenfade"
                :depends-on ("entities"))
               (:file "enemies"
                :depends-on ("game"
                             "entities"))
               (:file "main"
                :depends-on ("launcher"
                             "game"))
               (:file "build")))
