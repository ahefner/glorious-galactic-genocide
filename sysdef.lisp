(in-package :cl-user)

;;;; System definition

(pushnew :with-gui *features*)

(defun cflags () (list #-win32 "-I/usr/include/freetype2"
		       #+win32 "-Ic:/MinGW/include/freetype2"
		       "-DGLEW_STATIC=1" "-Isrc/glew" "-std=c99"))

(defun ld-flags ()
  "Non-library linker flags"
  (list ;;#+win32 "-Wl,-subsystem,windows"
	))

(defun c-sources ()
  "C source code modules."
  (list "src/sys.c"
	"src/glew/glew.c"
        "src/audio.c"
        "src/text-render.c"))

(defun lisp-compile-sources ()
  "Lisp source needed at compile time (macros, package definitions, etc.) for all source files."
  (list "src/package.lisp"
        "src/macrology.lisp"
        "src/globals.lisp"))

(defun lisp-sources ()
  "Lisp sources to be linked into the final executable."
  (list
   "sysdef.lisp"               ; For runtime recompile feature..
   "src/package.lisp"
   "src/globals.lisp"
   "src/util.lisp"
   "src/math.lisp"
   '("src/vid-sdl-opengl.lisp" #|depends on:|# "src/math.lisp")
   "src/text-layout.lisp"
   "src/uim-defs.lisp"
   '("src/uim.lisp"         #|depends on:|# "src/uim-defs.lisp" "src/math.lisp")
   '("src/line-editor.lisp" #|depends on:|# "src/uim-defs.lisp")
   '("src/event-loop.lisp"  #|depends on:|# "src/uim-defs.lisp")
   "src/star-names.lisp"
   '("src/sim-defs.lisp"    #|depends on:|# "src/math.lisp")
   '("src/sim.lisp"         #|depends on:|# "src/sim-defs.lisp" "src/math.lisp")
   '("src/mapgen.lisp"      #|depends on:|# "src/sim-defs.lisp" "src/math.lisp")
   '("src/techs.lisp"       #|depends on:|# "src/sim-defs.lisp" "src/math.lisp")
   '("src/gamebar.lisp"     #|depends on:|# "src/uim-defs.lisp" "src/math.lisp" "src/sim-defs.lisp")
   '("src/starmap.lisp"     #|depends on:|# "src/uim-defs.lisp" "src/math.lisp" "src/sim-defs.lisp")
   '("src/ui-research.lisp" #|depends on:|# "src/uim-defs.lisp" "src/math.lisp" "src/sim-defs.lisp")
   '("src/ships.lisp"       #|depends on:|# "src/math.lisp")
   '("src/designer.lisp"    #|depends on:|# "src/uim-defs.lisp" "src/math.lisp" "src/sim-defs.lisp")
   "src/main.lisp"))

(defun shared-libraries ()
  (list "SDL" "SDL_image"
        #-win32 "GL"
	#+win32 "opengl32"
        "vorbisfile"
        "freetype"
	#-win32 "z"
	#+win32 "zlib1"))

