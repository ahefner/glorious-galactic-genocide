(in-package :g1)

;;;; FFI Sugar

(defun c-inline-get-inputs (list)
  (loop for name in (rest list) by #'cddr collect name))

(defun c-inline-get-types (list)
  (loop for name in list by #'cddr collect name))

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
