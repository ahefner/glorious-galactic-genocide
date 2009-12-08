
;;;; System definition

(defun cflags () (list "-I/usr/include/freetype2" "-std=c99"))

(defun c-sources ()
  "C source code modules"
  (list "src/sys.c"
        "src/text-render.c"))

(defun lisp-compile-sources ()
  "Lisp source needed during compile-time (macros, package definitions, etc.)"
  (list "src/package.lisp"
        "src/macrology.lisp"
        "src/globals.lisp"))

(defun lisp-sources ()
  "Lisp sources to be linked into the final executable"
  (list "src/package.lisp"
        "src/globals.lisp"
        "src/util.lisp"
        "src/math.lisp"
        "src/vid-sdl-opengl.lisp"
        "src/text-layout.lisp"
        "src/uim-defs.lisp"
        '("src/uim.lisp"     "src/uim-defs.lisp")
        "src/star-names.lisp"
        '("src/sim-defs.lisp" "src/math.lisp")
        '("src/sim.lisp" "src/sim-defs.lisp" "src/math.lisp")
        '("src/mapgen.lisp" "src/sim-defs.lisp" "src/math.lisp")
        '("src/starmap.lisp" "src/uim-defs.lisp" "src/math.lisp" "src/sim-defs.lisp")
        '("src/gamebar.lisp" "src/uim-defs.lisp")
        "src/main.lisp"))

(defun shared-libraries ()
  (list "SDL" "SDL_image"
        "GL"
        "freetype" "z"))

