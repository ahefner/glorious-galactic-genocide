(in-package :g1)

;;;; FFI Sugar

(eval-when (:compile-toplevel)
  (defun c-inline-get-inputs (list)
    (loop for name in (rest list) by #'cddr collect name))
  
  (defun c-inline-get-types (list)
    (loop for name in list by #'cddr collect name)))

#+ecl
(defmacro c (expr-or-return-type &rest args)
  "Syntactic sugar for ffi:c-inline one-liners."
  (typecase expr-or-return-type
    (string
     `(ffi:c-inline ,(c-inline-get-inputs args)
                    ,(c-inline-get-types args)
                    (values)
                    ,expr-or-return-type
                    :one-liner t))
    (t `(ffi:c-inline ,(c-inline-get-inputs (rest args))
                      ,(c-inline-get-types (rest args))
                      ,expr-or-return-type
                      ,(first args)
                      :one-liner t))))
#+ecl
(defmacro cx (expr-or-return-type &rest args)
  "Syntactic sugar for ffi:c-inline one-liners (no side-effects version)"
  (typecase expr-or-return-type
    (string
     `(ffi:c-inline ,(c-inline-get-inputs args)
                    ,(c-inline-get-types args)
                    (values)
                    ,expr-or-return-type
                    :one-liner t :side-effects nil))
    (t `(ffi:c-inline ,(c-inline-get-inputs (rest args))
                      ,(c-inline-get-types (rest args))
                      ,expr-or-return-type
                      ,(first args)
                      :one-liner t :side-effects nil))))
#+ecl
(defmacro call (expr-or-return-type &rest args)
  "Even more syntactic sugar for ffi:c-inline one-liners."
  (typecase expr-or-return-type
    (string
     `(c ,(format nil "~A(~{#~D~^, ~})" expr-or-return-type
                  (loop for i from 0 below (length (c-inline-get-inputs args)) collect i))
         ,@args))
    (t `(c ,expr-or-return-type
           ,(format nil "~A(~{#~D~^, ~})" (first args)
                    (loop for i from 0 below (length (c-inline-get-inputs (rest args))) collect i))
         ,@(rest args)))))

;;; Example:
;;; (call :int "printf" :cstring "testing %i" :int 42)


;;;; My personal lapse in taste:

(defmacro deftuple (name &rest slots)
  (flet ((slot-name (slotspec) (etypecase slotspec (cons (first slotspec)) (symbol slotspec))))
    (let ((prototype (gensym (string name))))
    `(progn
       (defstruct (,name (:constructor ,name ,(mapcar #'slot-name slots))) ,@slots)
       (defun ,(intern (format nil "~A-FROM" name))
           (,prototype &key ,@(loop for slot-name-spec in slots
                                    as slot-name = (slot-name slot-name-spec)
                                    as reader = (intern (format nil "~A-~A" name slot-name))
                                    collect `(,slot-name (,reader ,prototype))))
         (,name ,@(mapcar #'slot-name slots)))))))

;;;; Etc.

(define-modify-macro logiorf (&rest args) logior)

(define-modify-macro deletef (item &rest remove-keywords)
  (lambda (sequence item &rest args) (apply #'delete item sequence args)))

(defmacro orf (place value)
  ;; Can multiply evaluate place. Whatever, too much trouble to fix.
  `(or ,place (setf ,place ,value)))

(define-modify-macro minf (other) min)
(define-modify-macro maxf (other) max)

(defmacro random-choice (&body body-clauses)
  (let ((total-weights (reduce #'+ body-clauses :initial-value 0 :key #'first))
        (index 0)
        (sym (gensym)))
    (assert (integerp total-weights))
    (assert (>= total-weights 1))
    `(let ((,sym (random ,total-weights)))
      (cond
        ,@(mapcar
           (lambda (spec)
             (destructuring-bind (weight &rest body) spec
               (incf index weight)
               `((< ,sym ,index) ,@body)))
           body-clauses)))))


;;;; Further departures from taste and decorum

(defmacro defcat (name form)
  `(defmacro ,name (&body body)
    (labels ((transform (expression)
               `(progn (psetf $$$ $$
                              $$ $
                              $ ,expression)
                 $)))
      (let ((form ',form))
       `(let (($$$ nil)
              ($$ nil)
              ($ nil))
         (,form ,@(mapcar #'transform body)))))))

(defcat progn.. progn)
(defcat and.. and)

;;;; Simple cache utility.. should've added this earlier..

(defmacro cachef ((place value-form &key (test 'equal) (delete 'identity))
                  &body derived-forms)
  ;; The FLET mostly takes care of things, but it's not 100% correct
  ;; unless we enclose TEST and DELETE too..
  `(flet ((value-fn () ,value-form)
          (derived-fn () ,@derived-forms))
     (let ((value (value-fn)))
       (cacheobj-derived (cond 
                           ((not ,place)
                            (setf ,place (make-cacheobj :value value :derived (derived-fn))))
                           ((and ,place (,test (cacheobj-value ,place) value))
                            ,place)
                           (t (,delete (cacheobj-derived ,place))
                              (setf ,place (make-cacheobj :value value :derived (derived-fn)))))))))

;;;; Moved here from math.lisp:

(defun suffix (symbol suffix)
  (intern (format nil "~A~A" (string symbol) (string suffix))))

(defun symbol-xyz-list (symbol)
  (list (suffix symbol '#:.x)
	(suffix symbol '#:.y)
	(suffix symbol '#:.z)))

;;;; Needs to exist at compile-time.

(defsetf file (filename) (object)
  `(let ((filename ,filename)
         (object ,object))
     (when *devmode* (format *trace-output* "~&Writing ~W~%" filename))
     (with-open-file (out filename :direction :output :if-exists :supersede)
       (with-standard-io-syntax ()
         (let ((*print-circle* t)
               (*print-right-margin* 140)
               (*package* (find-package :g1)))
           (pprint object out))))))

;;;; ECL moans about wrapping DEFUNs with LET, and I'm told it
;;;; inhibits direct function calls.
#-ecl
(defmacro with-vars (bindings &body body)
  `(let ,bindings ,@body))

#+ecl
(defmacro with-vars (bindings &body body)
  (setf bindings (loop for b in bindings collect (if (consp b) b (list b nil))))
  (let ((syms (mapcar (lambda (x) (gensym (string (first x)))) bindings)))
    `(progn
       ,@(loop for b in bindings 
               for sym in syms
               collect `(defvar ,sym ,(second b)))
       (symbol-macrolet
           ,(loop for b in bindings for sym in syms
                  collect (list (first b) sym))
         ,@body))))
