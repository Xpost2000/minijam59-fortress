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
               (:file "game"
                :depends-on ("window"))
               (:file "main"
                :depends-on ("launcher"
                             "game"))
               (:file "build")))
