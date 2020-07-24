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
               (:file "launcher"
                :depends-on ("sdl-renderer"
                             "utils"
                             "input"))
               (:file "game"
                :depends-on ("launcher"
                             "input"))
               (:file "build")))
