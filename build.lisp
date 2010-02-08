;;(require 'asdf)

(proclaim '(optimize (debug 3) (speed 0) (safety 3) (space 0)))
;;(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0)))

;;;; Load system definition

(load "sysdef.lisp")

;;;; Build system functions

(defun lisp-linked-sources ()
  (ordered-remove-duplicates
   (mapcan (lambda (x)
             (if (listp x)
                 (copy-list x)
                 (list x)))
           (lisp-sources))))

(defun ordered-remove-duplicates (list)
  "Similar to cl:remove-duplicates, but guarantees that the first
occurrence of the item is the one which will be preserved. Uses EQUAL
for comparison. Accepts and produces only lists."
  (let ((table (make-hash-table :test 'equal)))
    (loop for item in list
          unless (gethash item table) collect item
          unless (gethash item table) do (setf (gethash item table) t))))

(defun fail ()
  (format t "~&~%BUILD FAILED.~%")
  (ext:quit 1)
  ;; Attempt to work around ECL stupidity. Doesn't work.
  #+NIL  (ffi:c-inline () () (values) "{ cl_shutdown(); _exit(1); }"))

(compile 'fail)

(setf c::*cc-flags* (format nil "~A~{ ~A~}" c::*cc-flags* (cflags)))

(defun object-pathname (filename)
  (compile-file-pathname filename :type :object))

#+NIL
(defun up-to-date? (filename)
  (and (probe-file (object-pathname filename))
       (>= (file-write-date (object-pathname filename))
           (file-write-date filename))))

(defun newer? (file1 file2)
  ;; (format t "~&newer? ~A vs ~A~%" (file-write-date file1) (file-write-date file2))
  (cond
   ((not (probe-file file2))
    (format t "~&~W doesn't exist!~%" file2))
   ((not (probe-file file1)) nil)
   (t (>= (file-write-date file1) (file-write-date file2)))))

(defun read-unescaping (stream)
  (with-output-to-string (out)
    (loop as char = (read-char stream nil)
          ;;do (format t "~&  read ~W~%" char)
          when (and (eql char #\\)
                    (eql #\Return (peek-char nil stream)))
          do (setf char (read-char stream nil))
          when (null char) do (return)
          do (write-char char out))))

(defun whitespacep (char) (or (char= char #\Space) (char= char #\Tab) (char= char #\Newline) (char= char #\Return)))

(defun split-string (string)
  (loop repeat (length string)
        with accum = nil
        with index = 0
        as start = (position-if-not #'whitespacep string :start index)
        as end = (and start (position-if #'whitespacep string :start start))
        when start do (push (subseq string start end) accum)
        do (setf index end)
        unless index do (return (nreverse accum))))

(defun c-object-deps (filename)
  (let* ((args (append (cflags) (list "-M" (namestring filename))))
         (stream (ext:run-program "cpp" args
                                  :input nil :output :stream :error *error-output*)))
    (handler-case 
        (progn
          (loop as char = (read-char stream nil)
		until (eql char #\:)
		when (null char)
		;; This fails on a certain file in Windows. I'm not
		;; sure why, but easiest to work around it here:
		do (warn "Problem reading CPP output!") (return-from c-object-deps (list filename)))
          (peek-char t stream nil)
          (split-string (read-unescaping stream)))
      (serious-condition (err)
        (format t "~&~A ~{~A~^ ~}~%" "cpp" args)
        (format t "~&Error computing dependencies of ~A:~% ~A~%" filename err)
        (fail)))))

(defun c-file? (filename) (string-equal "c" (pathname-type (pathname filename))))

(defun file-dependencies (pathname)
  (cond
    ((c-file? pathname) (c-object-deps pathname))
    (t (list (namestring pathname)))))

(defun changed-dependencies (pathname extra-deps)
  (remove-if (lambda (dep) (newer? (object-pathname pathname) (pathname dep)))
             (append (file-dependencies pathname) extra-deps)))

;;;; Build process

(defvar *compiler-sources* (make-hash-table :test 'equal))

(defun ensure-compiler-source (filename)
  (format t "~&~%----- Compile-time dependency: ~A -----~%" filename)
  (unless (gethash filename *compiler-sources*)
    (load (compile-file filename :print nil :verbose nil 
                        :output-file (format nil "compile-time-~A.fasl" 
                                             (pathname-name (pathname filename)))))
    (setf (gethash filename *compiler-sources*) t)))

(defun load-compiler-sources ()
  (format t "~&~%--------------- ESTABLISHING COMPILE-TIME ENVIRONMENT ---------------~%~%")
  (loop for filename in (lisp-compile-sources)
        do (ensure-compiler-source filename)))

;(trace changed-dependencies)
;(trace read-unescaping)
;(trace object-pathname)
;(trace c::compiler-cc)
;(trace newer?)

(defvar *use-colors* #-win32 t #+win32 nil)
(defun sgr (&rest modes)
  "Print ANSI color codes. Doesn't work in the Windows console."
  (when *use-colors* (format t "~C[~{~D~^;~}m" #\Esc modes)))

(loop with compiler-sources-loaded = nil
      for source-spec in (append (c-sources) (lisp-sources))
      as filename = (if (listp source-spec)
                        (first source-spec)
                        source-spec)
      as compile-time-deps = (and (listp source-spec) (rest source-spec))
      as deps = (changed-dependencies (pathname filename) compile-time-deps)      
      ;;do (print (list :file filename :compile-time-deps compile-time-deps :deps deps))
      when deps
      do (handler-bind ((c::compiler-note
                         (lambda (condition)
                           (sgr 0)))
                        (c::compiler-warning
                         (lambda (condition)
                           (sgr 0 33)))
                        (c::compiler-error
                         (lambda (condition)
                           (sgr 0 31)))
                        (c::compiler-undefined-variable
                         (lambda (condition)
                           (sgr 1 31))))
           (sgr 0)
           (unless compiler-sources-loaded 
             (setf compiler-sources-loaded t)
             (load-compiler-sources)
             (sgr 0)
             (format t "~&~%--------------- BUILDING SOURCE FILES ---------------~%~%"))
           
           (when (probe-file (object-pathname filename))
             (sgr 0)
             (format t "~&(Recompiling due to changes in ~{~A~^, ~})~%" deps))
           (cond
             ((c-file? filename)
              (sgr 0)
              (format t "~&~%----===---- Compiling ~A ----===----~%" filename)
              (handler-bind
                  ((serious-condition (lambda (foo) (fail))))
                (sgr 0)
                (c::compiler-cc filename (object-pathname filename)))
              #+NIL
              (unless (zerop (print (c::compiler-cc filename (object-pathname filename))))
                (ext:quit 1)))
             (t                         ; Lisp file
              (mapc #'ensure-compiler-source compile-time-deps)
              (sgr 0)
              (format t "~&~%----===---- Compiling ~A ----===----~%" filename)
              (unless (compile-file filename :verbose nil :system-p t :print nil
                                    :c-file (make-pathname
                                             :type "c"
                                             :directory (pathname-directory #p"obj/")
                                             :name (pathname-name (pathname filename))))
                (sgr 0 31)
                (format *trace-output* "~&Error compiling ~A~%" filename)
                (sgr 0)
                (fail))))))

(format t "~&~%--------------- BUILDING EXECUTABLE ---------------~%")

;;;(print (lisp-linked-sources))

(defun source-names->object-names (sources)
  (mapcar #'object-pathname sources))

(c:build-program #+win32 "g1.exe" #-win32 "g1"
                  :lisp-files (source-names->object-names (lisp-linked-sources))
                  :ld-flags (append (mapcar #'namestring (source-names->object-names (c-sources)))
				    (ld-flags)
                                    (mapcar (lambda (x) (format nil "-l~A" x)) (shared-libraries)))
                  :epilogue-code '(eval (read-from-string "(g1:main)")))

(sgr 32)
(format t "~&BUILD COMPLETE!~%~%")
(sgr 0)
(finish-output)


