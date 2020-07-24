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
               (:file "launcher")
               (:file "game")
               (:file "build")))
