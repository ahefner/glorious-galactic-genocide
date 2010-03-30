(in-package :g1)

(ffi:clines "#include \"sys.h\"")
(ffi:clines "#include \"GL/glew.h\"")

(defun asset-base () ".")                ; TODO FIXME!!

(defun apath (filename-or-symbol &optional (extension "png"))       ; What file should I live in?
  (typecase filename-or-symbol
    (string (format nil "~A/data/~A" (asset-base) filename-or-symbol))
    (symbol (format nil "~A/data/~A.~A" 
                    (asset-base)
                    (string-downcase (string filename-or-symbol)) 
                    extension))))

(defun load-assets ()
  (setf *button-a* (make-button-style
                    :baseline 16
                    :pressed (make-bar-style :left  (img :button-a-pressed-left)
                                             :right (img :button-a-pressed-right)
                                             :fill  (texture :button-a-pressed-fill))
                    :released (make-bar-style :left  (img :button-a-released-left)
                                              :right (img :button-a-released-right)
                                              :fill  (texture :button-a-released-fill)))
        ))

(defun diffvec (vector)
  (let ((v (make-array (1- (length vector)))))
    (loop for index from 0 below (1- (length vector))
          do (setf (aref v index) (- (aref vector (1+ index)) (aref vector index))))
    v))

(defvar *print-fps* nil)
(defvar *current-fps* nil)
(defvar *show-fps* t)
(defvar *show-fps-cache* nil)

;;; There's some spooky bug with catch/throw in ECL. I had to flail
;;; around changing things to make this work. It will probably break
;;; again soon enough.
(defun throw-from-gadget-run ()
  (throw :abort-gadget-run t))

(let ((times (make-array 500)))
 (defun repaint (uic)
   (when (and (>= *total-frames* (length times))
              (zerop (mod *total-frames* (length times))))
     (let* ((min (reduce #'min times))
            (max (reduce #'max times))
            (fps (/ (length times) 1.0 (/ (- max min) internal-time-units-per-second))))
       ;;(print (diffvec times))
       (setf *current-fps* fps)
       (when *print-fps* (format t "~&Frames per second: ~D~%" fps))
       #+NIL (throw 'bailout :byebye)))
   (setf (aref times (mod *total-frames* (length times))) (get-internal-real-time))
   (incf *total-frames*)   
   (paint-begin)
   ;; Run the UI
   (catch :abort-gadget-run
     (gadget-run *gadget-root* uic))
   (check-gl-error)
   ;; Draw frames per second badge in corner
   (when (and *show-fps* *current-fps*)
     (let ((string (format nil "[~:D fps]" (round *current-fps*))))
       (draw-img (cachef (*show-fps-cache* string :delete free-img)
                   (render-label 'repaint :sans 10 string :align-x :right))
                 (+ -3 (uic-width uic)) (+ -5 (uic-height uic)))))
   (check-gl-error)
   (paint-finish)))

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
        (play-sound :chirp)
        (system::invoke-debugger c)
        (ext:quit 1))))
    
    (%main)))

(defun have-opengl-1.4 () (not (zerop (cx :int "GLEW_VERSION_1_4"))))

(defun %main ()

  (setf *global-owner* (make-instance 'global-owner))

  (setf *package* (find-package :g1))

  (unless (zerop (c :int "sys_init(\"Glorious Galactic Genocide!\")")) 
    (print "System init failed. Oh, poo.")
    (ext:quit))

  (format t "~&GL Vendor: ~A~%GL Renderer: ~A~%GL Version: ~A~%~D texture unit~:P.~%"
          (c :cstring "glGetString(GL_VENDOR)")
          (c :cstring "glGetString(GL_RENDERER)")
          (c :cstring "glGetString(GL_VERSION)")
          (if (not (have-opengl-1.4))
              1
              (gl-get-integer (cx :int "GL_MAX_TEXTURE_UNITS"))))

  (macrolet ((field (name)
               `(progn
                  (format t "~&~A: ~:D~%" ,name 
                   (ffi:c-inline 
                    () () :int
                    ,(format nil "{ GLint tmp=-1; glGetProgramivARB(GL_FRAGMENT_PROGRAM_ARB, (~A), &tmp); @(return 0)=tmp; }" name)
                    :one-liner nil))
                  (check-gl-error ,name :warn t))))
    (when (have-opengl-1.4)
     (field "GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB")
     (field "GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB")
     (field "GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB")
     (field "GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB")
     (field "GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB")
     (field "GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB")))
  
  (load-assets)

  ;; 512 isn't quite wide enough for certain title labels that appear
  ;; in the fleet display (with race, destination, and ETA, in title
  ;; face, it can get wide).
  ;; The larger schematic images demand we upgrade to 1024 vertically, too.
  (setf *packset* (make-packset 1024 1024))

  (format t "~&Running test game.~%")
 
  (multiple-value-bind (*universe* *player*) (make-test-universe)
    ;; This is for debugging from SLIME:
    (setf (symbol-value '*universe*) *universe*
          (symbol-value '*player*) *player*)
    ;; 
    (setf *gameui* (create-gameui *universe*)
          *gadget-root* *gameui*)

    ;; Hack, test editor:
    ;;(setf *gadget-root* (make-instance 'line-editor :led (led "" 0) :screen-vector (v2 10 30) :face :gothic :size 32))
    
;;    (time (uim-sdl-run))
    #+NIL (repaint (initial-uic))
    (time (catch 'bailout (uim-sdl-run))))
  (format t "~&Shutting down.~%")
  (format t "~&Total frames: ~:D (~D)~%" *total-frames* *total-frames*)
  (c "sys_shutdown()")

  (format t "~&Bye!~%")
  (ext:quit))

(defun label-color (&optional (player *player*)) (pstyle-label-color (style-of player)))
(defun lighter-color (&optional (player *player*)) (color-lighten (label-color player)))

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
