FILES=build.lisp\
	  game.lisp\
	  input.lisp\
	  launcher.lisp\
	  main.lisp\
	  mjgame.asd\
	  package.lisp\
	  rectangle.lisp\
	  sdl-renderer.lisp\
	  show-messagebox.lisp\
	  utils.lisp\
	  vec2.lisp\
	  entities.lisp\
	  screenfade.lisp\
	  turret.lisp\
	  enemies.lisp\
	  rooms.lisp\
	  window.lisp

dist/mj59game.exe: $(FILES)
	sbcl --load "mjgame.asd" --eval "(ql:quickload :mjgame)" --eval "(mjgame:build-game)"
