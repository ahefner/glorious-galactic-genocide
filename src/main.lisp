(in-package :g1)

(ffi:clines "#include \"sys.h\"")

(defun apath (filename-or-symbol)       ; What file should I live in?
  (typecase filename-or-symbol
    (string (format nil "data/~A" filename-or-symbol))
    (symbol (format nil "data/~A.png" (string-downcase (string filename-or-symbol))))))

(defun load-assets ()
  (setf *stars00* (load-texture-file (apath "stars00.png"))
        *stars01* (load-texture-file (apath "stars01.png"))
        *stars02* (load-texture-file (apath "stars02.png"))
        *stars03* (load-texture-file (apath "stars03.png"))))

(let ((total-frames 0)
      (times (make-array 300)))
 (defun repaint (uic)
  (setf (aref times (mod total-frames 300)) (get-internal-real-time))
  (when (>= total-frames 300)
    (let* ((min (reduce #'min times))
           (max (reduce #'max times))
           (fps (/ 300.0 (/ (- max min) internal-time-units-per-second))))
      (when (zerop (mod total-frames 300))
        (format t "~&Frames per second: ~D~%" fps))))
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
    (%main)))

(defun %main ()

  (setf si:*gc-verbose* t)

  (setf *package* (find-package :g1))
  (unless (zerop (c :int "sys_init(\"game 1\")")) 
    (print "oh, poo.")
    (ext:quit))
  
  (load-assets)
  
  (setf *packset* (make-packset 512 256))

  (format t "~&Testing event loop.~%")

  (multiple-value-bind (universe *player*) (make-test-universe)
    (setf *gadget-root* (make-instance 'debug-starmap :universe universe))
    (uim-sdl-run))

  (format t "~&Shutting down.~%")
  (c "sys_shutdown()")

  (format t "~&Bye!~%")
  (ext:quit))
