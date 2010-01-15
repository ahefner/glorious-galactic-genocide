(in-package :g1)

(ffi:clines "#include \"sys.h\"")
(ffi:clines "#include \"GL/gl.h\"")

(defun asset-base () ".")                ; TODO FIXME!!

(defun apath (filename-or-symbol &optional (extension "png"))       ; What file should I live in?
  (typecase filename-or-symbol
    (string (format nil "~A/data/~A" (asset-base) filename-or-symbol))
    (symbol (format nil "~A/data/~A.~A" 
                    (asset-base)
                    (string-downcase (string filename-or-symbol)) 
                    extension))))

(defun load-assets ()
  (setf *stars00* (load-texture-file (apath "stars00.png"))
        *stars01* (load-texture-file (apath "stars01.png"))
        *stars02* (load-texture-file (apath "stars02.png"))
        *stars03* (load-texture-file (apath "stars03.png"))
        *slider160* (load-texture-file (apath "slider-160.png"))
        *gamebar-fill* (load-texture-file (apath "gamebar-fill.png"))
        *panel-fill* (load-texture-file (apath "panel-fill.png"))
 
        *button-a* (make-button-style
                    :baseline 16
                    :pressed (make-bar-style :left  (img :button-a-pressed-left)
                                             :right (img :button-a-pressed-right)
                                             :fill  (load-texture-file (apath "button-a-pressed-fill.png")))
                    :released (make-bar-style :left  (img :button-a-released-left)
                                              :right (img :button-a-released-right)
                                              :fill  (load-texture-file (apath "button-a-released-fill.png"))))
 ))

(defun diffvec (vector)
  (let ((v (make-array (1- (length vector)))))
    (loop for index from 0 below (1- (length vector))
          do (setf (aref v index) (- (aref vector (1+ index)) (aref vector index))))
    v))

(let ((total-frames 0)
      (times (make-array 500)))
 (defun repaint (uic)
   (when (and (>= total-frames (length times))
              (zerop (mod total-frames (length times))))
     (let* ((min (reduce #'min times))
            (max (reduce #'max times))
            (fps (/ (length times) 1.0 (/ (- max min) internal-time-units-per-second))))
       ;;(print (diffvec times))
       ;;(format t "~&Frames per second: ~D~%" fps)
       #+NIL (throw 'bailout :byebye)))
   (setf (aref times (mod total-frames (length times))) (get-internal-real-time))
   (incf total-frames)

   (paint-begin)
   (gadget-paint *gadget-root* uic)
   (check-gl-error)
   
   (paint-finish)))

(defun start-swank-server ()
  (require 'asdf)
  (eval (read-from-string "(push '(MERGE-PATHNAMES \".sbcl/systems/\" (USER-HOMEDIR-PATHNAME)) asdf:*central-registry*)"))
  (eval (read-from-string "(asdf:oos 'asdf:load-op :swank)"))
  (eval (read-from-string "(swank:create-server :port 0)")))

(defun main ()
  ;; Old ECL hacks. Hopefully shouldn't be necessary anymore.
  #+NIL
  (setf system::*lisp-initialized* t
        system::*break-enable* t)

  ;; Disable goddamned floating point traps.
  (ext:trap-fpe 'floating-point-underflow nil)

  (handler-bind
    (#+NIL
     (serious-condition
      (lambda (c)
        ;;(format t "~&~A~%" c)
        (format t "~&Better luck next time!~%")
        (system::invoke-debugger c)
        (ext:quit 1))))
    
    (%main)))

(defun %main ()

  (setf *global-owner* (make-instance 'global-owner))

  (setf *package* (find-package :g1))

  (unless (zerop (c :int "sys_init(\"Glorious Galactic Genocide!\")")) 
    (print "System init failed. Oh, poo.")
    (ext:quit))

  (format t "~&GL Vendor: ~A~%GL Renderer: ~A~%GL Version: ~A~%~D texture units.~%"
          (c :cstring "glGetString(GL_VENDOR)")
          (c :cstring "glGetString(GL_RENDERER)")
          (c :cstring "glGetString(GL_VERSION)")
          (gl-get-integer (cx :int "GL_MAX_TEXTURE_UNITS")))

  (macrolet ((field (name)
               `(progn
                  (format t "~&~A: ~:D~%" ,name 
                   (ffi:c-inline 
                    () () :int
                    ,(format nil "{ GLint tmp=-1; glGetProgramivARB(GL_FRAGMENT_PROGRAM_ARB, (~A), &tmp); @(return 0)=tmp; }" name)
                    :one-liner nil))
                  (check-gl-error ,name :warn t))))
    (field "GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB")
    (field "GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB")
    (field "GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB")
    #|
    (field "GL_MAX_PROGRAM_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB")
    (field "GL_MAX_PROGRAM_TEMPORARIES_ARB")
    (field "GL_MAX_PROGRAM_PARAMETERS_ARB")
    (field "GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB")|#)

  
  (load-assets)

  ;; 512 isn't quite wide enough for certain title labels that appear
  ;; in the fleet display (with race, destination, and ETA, in title
  ;; face, it can get wide).
  (setf *packset* (make-packset 1024 512))

  (format t "~&Running test game.~%")
 
  (multiple-value-bind (*universe* *player*) (make-test-universe)
    (setf *gameui* (create-gameui *universe*)
          *gadget-root* *gameui*)
    
;;    (time (uim-sdl-run))
    #+NIL (repaint (initial-uic))
    (time (catch 'bailout (uim-sdl-run))))
  (format t "~&Shutting down.~%")
  (c "sys_shutdown()")

  (format t "~&Bye!~%")
  (ext:quit))

(defun label-color () (pstyle-label-color (style-of *player*)))
(defun lighter-color () (color-lighten (label-color)))

;;;; Runtime recompilation

(defvar *compilation-times* (make-hash-table :test 'equal))

(defvar *program-start-time* (get-universal-time))

(defun ensure-source-file (filename)
  (when (> (file-write-date (pathname filename))
           (gethash filename *compilation-times*
                    (if (find filename (cl-user::lisp-compile-sources))
                        0
                        *program-start-time*)))
    ;; Another totally irrelevant race condition.    
    (format t "~&---- Compiling ~A ----~%" filename)
    (let ((fasl (compile-file filename :print nil :verbose nil
                              ;;:user-cflags (format nil "-I../src/ ~{ ~A~}" (cl-user::cflags))
                              :output-file (format nil "obj/hotpatch_~A.fasl" 
                                                   (pathname-name (pathname filename))))))
      (unless fasl
        (format *trace-output* "~&Error compiling ~A~%" filename)
        (play-sound :chirp)
        (throw 'abort-reload t))
      (load fasl)
      (setf (gethash filename *compilation-times*) (file-write-date (pathname filename))))))

(defun reload-modified-sources ()
  ;; Major limitation - you can't recompile sources that expect to
  ;; link against C function in the executable (as opposed to
  ;; functions in dynamic libraries, which work fine).
  (play-sound :chime-high)
  (require 'cmp)
  ;; Yes, we really need to use EVAL here.  
  (let ((sym (eval (read-from-string "'c::*cc-flags*"))))
    (printl :old-cc-flags (symbol-value sym)
            (printl :new-cc-flags (setf (symbol-value sym)
                                        (format nil "~A -I./src/ ~{ ~A~}" (symbol-value sym) (cl-user::cflags))))))

  (catch 'abort-reload
    (loop for source-spec in (append (cl-user::lisp-compile-sources) (cl-user::lisp-sources))
          as filename = (if (listp source-spec)
                            (first source-spec)
                            source-spec)
          as compile-time-deps = (and (listp source-spec) (rest source-spec))
          do
          
          (print (list :checking filename :deps compile-time-deps))
          (mapc #'ensure-source-file compile-time-deps)
          (ensure-source-file filename)))
    (play-sound :chime-low))





