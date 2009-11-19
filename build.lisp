(require 'asdf)

(declaim (optimize (debug 3) (speed 2) (safety 2)))

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
  (and (probe-file file1)
       (probe-file file2)
       (>= (file-write-date file1) (file-write-date file2))))

(defun read-unescaping (stream)
  (with-output-to-string (out)
    (loop as char = (read-char stream nil)
          when (eql char #\\) do (setf char (read-char stream nil))
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
  (let ((stream (ext:run-program "cpp" 
                                 (append (cflags)
                                         (list "-M" (namestring filename)))
                                 :input nil :output :stream :error t)))
    (handler-case 
        (progn
          (loop until (eql (read-char stream) #\:))
          (peek-char t stream nil)
          (split-string (read-unescaping stream)))
      (serious-condition (err)
        (format t "~&Error computing dependencies of ~A:~% ~A~%" filename err)
        (fail)))))

(defun c-file? (filename) (string-equal "c" (pathname-type (pathname filename))))

(defun file-dependencies (pathname)
  (cond
    ((c-file? pathname) (c-object-deps pathname))
    (t (list (namestring pathname)))))

(defun changed-dependencies (pathname)
  (remove-if (lambda (dep) (newer? (object-pathname pathname) (pathname dep)))
             (file-dependencies pathname)))

;;;; Build process

(format t "~&--------------- ESTABLISHING COMPILE-TIME ENVIRONMENT ---------------~%")

(defvar *compiler-sources* (make-hash-table :test 'equal))

(defun ensure-compiler-source (filename)
  (unless (gethash filename *compiler-sources*)
    (load (compile-file filename :print nil :verbose nil 
                        :output-file (format nil "compile-time-~A.fasl" 
                                             (pathname-name (pathname filename)))))
    (setf (gethash filename *compiler-sources*) t)))

(loop for filename in (lisp-compile-sources)
      do (ensure-compiler-source filename))

(format t "~&--------------- BUILDING SOURCE FILES ---------------~%")

;(trace changed-dependencies)
;(trace object-pathname)
;(trace c::compiler-cc)
;(trace newer?)

(loop for source-spec in (append (c-sources) (lisp-sources))
      as filename = (if (listp source-spec)
                        (first source-spec)
                        source-spec)
      as compile-time-deps = (and (listp source-spec) (rest source-spec))
      as deps = (changed-dependencies (pathname filename))
      when deps
      do (progn
           (format t "~&---- Compiling ~A ----~%" filename)
           (when (probe-file (object-pathname filename))
             (format t "~&(Recompiling due to changes in ~{~A~^, ~})~%" deps))
           (cond
             ((c-file? filename)
              (handler-bind
                  ((serious-condition (lambda (foo) (fail))))
                (c::compiler-cc filename (object-pathname filename)))
              #+NIL
              (unless (zerop (print (c::compiler-cc filename (object-pathname filename))))
                (ext:quit 1)))
             (t                         ; Lisp file
              (mapc #'ensure-compiler-source compile-time-deps)
              (unless (compile-file filename :verbose nil :system-p t :print nil
                                    :c-file (merge-pathnames
                                             (make-pathname :type "c")
                                             (merge-pathnames #p"tmp/" (pathname filename))))
                (format *trace-output* "~&Error compiling ~A~%" filename)
                (fail))))))


(format t "~&--------------- BUILDING EXECUTABLE ---------------~%")

(print (lisp-linked-sources))

(defun source-names->object-names (sources)
  (mapcar #'object-pathname sources))

(c:build-program "g1"
                  :lisp-files (source-names->object-names (lisp-linked-sources))
                  :ld-flags (append (mapcar #'namestring (source-names->object-names (c-sources)))
                                    (mapcar (lambda (x) (format nil "-l~A" x)) (shared-libraries)))
                  :epilogue-code '(g1:main) #+NIL '(eval (read-from-string "(g1:main)")))

(format t "~&--------------- BUILD COMPLETE ! ---------------~%")
(finish-output)


