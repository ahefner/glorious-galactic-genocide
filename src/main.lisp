(in-package :g1)

(ffi:clines "#include \"sys.h\"")
(ffi:clines "#include \"GL/gl.h\"")

(defun apath (filename-or-symbol)       ; What file should I live in?
  (typecase filename-or-symbol
    (string (format nil "data/~A" filename-or-symbol))
    (symbol (format nil "data/~A.png" (string-downcase (string filename-or-symbol))))))

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
       (format t "~&Frames per second: ~D~%" fps)
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
    ((serious-condition 
      (lambda (c)
        ;;(format t "~&~A~%" c)
        (system::invoke-debugger c)
        (format t "~&Better luck next time.~%")
        (ext:quit 1))))
    
    (setf si:*gc-verbose* t)              ; Doesn't work anymore..

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
  
  (load-assets)

  ;; 512 isn't quite wide enough for certain title labels that appear
  ;; in the fleet display (with race, destination, and ETA, in title
  ;; face, it can get wide).
  (setf *packset* (make-packset 1024 512))

  (format t "~&Running test game.~%")
 
  (multiple-value-bind (*universe* *player*) (make-test-universe)
    (setf *gameui* (create-gameui *universe*)
          *gadget-root* *gameui*
          *label-color* (pstyle-label-color (style-of *player*))
          *lighter-color* (color-lighten *label-color*))
    
;;    (time (uim-sdl-run))
    #+NIL (repaint (initial-uic))
    (time (catch 'bailout (uim-sdl-run))))
  (format t "~&Shutting down.~%")
  (c "sys_shutdown()")

  (format t "~&Bye!~%")
  (ext:quit))

;;;; Runtime recompilation

(defvar *compilation-times* (make-hash-table :test 'equal))

(defun ensure-source-file (filename)
  (when (> (file-write-date (pathname filename))
           (gethash filename *compilation-times* *program-start-time*))
    ;; Another totally irrelevant race condition.
    (setf (gethash filename *compilation-times*) (file-write-date (pathname filename)))
    (format t "~&---- Compiling ~A ----~%" filename)
    (let ((fasl (compile-file filename :print nil :verbose nil
                              ;;:user-cflags (format nil "-I../src/ ~{ ~A~}" (cl-user::cflags))
                              :output-file (format nil "obj/hotpatch_~A.fasl" 
                                                   (pathname-name (pathname filename))))))
      (unless fasl
        (format *trace-output* "~&Error compiling ~A~%" filename)
        (throw 'abort-reload t))
      (load fasl))))

(defun reload-modified-sources ()
  ;; There's something completely insane about ECL that foils my
  ;; attempts to hack in a special variable for user CC flags in.
  #+NIL
  (setf (symbol-value 'c::*user-cc-flags*)
        (format nil "-I../src/ ~{ ~A~}" (cl-user::cflags))
         #+NIL
         (format nil "GAYNIGGERS ~A~{ ~A~}"
                 #-HACKWTF" -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -g -O2 -fPIC  -D_THREAD_SAFE -Dlinux"
                 #+HACKWTF c::*cc-flags* ; Unbound in the executable for whatever reason.
                 (cl-user::cflags)))
  ;;(declare (special c::*user-cc-flags*)) ; Seriously, what the fuck is with ECL?
    ;;(format t "~&CFLAGS should now be: ~A~%" c::*cc-flags*)
    (catch 'abort-reload 
      (loop for source-spec in (append (cl-user::lisp-compile-sources) (cl-user::lisp-sources))
            as filename = (if (listp source-spec)
                              (first source-spec)
                              source-spec)
            as compile-time-deps = (and (listp source-spec) (rest source-spec))
            do

            (print (list :checking filename :deps compile-time-deps))
            (mapc #'ensure-source-file compile-time-deps)
            (ensure-source-file filename))))





