(in-package :g1)

;;;; Stolen from Alexandria:

(defun random-elt (sequence &key (start 0) end)
  (declare (sequence sequence) (fixnum start) (type (or fixnum null) end))
  (elt sequence (+ start (random (- (or end (length sequence)) start)))))


(defun shuffle (sequence &key (start 0) end)
  "Returns a random permutation of SEQUENCE bounded by START and END.
Permuted sequence may share storage with the original one. Signals an
error if SEQUENCE is not a proper sequence."
  (declare (fixnum start) (type (or fixnum null) end))
  (typecase sequence
    (list
     (let* ((end (or end (length sequence)))
            (n (- end start)))
       (do ((tail (nthcdr start sequence) (cdr tail)))
           ((zerop n))
         (rotatef (car tail) (car (nthcdr (random n) tail)))
         (decf n))))
    (vector
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (aref sequence i) (aref sequence (random (+ i 1)))))))
    (sequence
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (elt sequence i) (elt sequence (random (+ i 1))))))))
  sequence)

;;; Note that while I declare this inline, it won't actually happen
;;; unless you put util.lisp in the compile-time deps of the relevant
;;; source module.

(declaim (inline clamp lerp nonnull))

(defun nonnull (foo)
  (assert foo)
  foo)

(defun clamp (x min max)
  (max min (min max x)))

(defun lerp (param from to)
  (+ (* from (- 1.0f0 param)) (* to param)))

(defun unit-ramp-in (ramp-start ramp-end value)
  (clamp (/ (- value ramp-start) (float (- ramp-end ramp-start) 0.0f0)) 0.0f0 1.0f0))

(defun bias-unit (x y)
  (clamp (/ (- x y) (- 1.0 (abs y))) 0.0 1.0))

(defun printl (&rest args) 
  (print args)
  (finish-output))

(defun whenzero (string number)
  (if (zerop number) string number))

(defun color-lighten (color)
  (macrolet ((f (x) `(ash (+ ,x 255) -1)))
    (vector (f (aref color 0)) (f (aref color 1)) (f (aref color 2)))))

(defun color-darken (color)
  (macrolet ((f (x) `(ash ,x -1)))
    (vector (f (aref color 0)) (f (aref color 1)) (f (aref color 2)))))

(defun color-with-alpha (color alpha)
  (vector (aref color 0) (aref color 1) (aref color 2) alpha))

;;; Like delete-if, but guaranteed to modify vector in place and
;;; adjust fill pointer.. that you can't rely on CL:DELETE* functions
;;; in this case is idiotic beyond belief.
(declaim (inline better-delete-if))
(defun sane-delete-if (predicate vector-with-fill-pointer)
  (loop with v = vector-with-fill-pointer
        with n = 0
        for item across vector-with-fill-pointer
        unless (funcall predicate item)
        do
        (setf (aref v n) item)
        (incf n)
        finally
        (setf (fill-pointer v) n)
        (return v)))

(defun pretty-sym (symbol)
  (string-capitalize (substitute #\Space #\- (symbol-name symbol))))

(defun iftype (type) (lambda (object) (typep object type)))

(defun file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil)
            (*package* (find-package :g1)))
        (read in)))))

(defun maybe-file (filename)
  (if (probe-file filename)
      (values (file filename) t)
      (values nil nil)))

(defun show (name value)
  (printl name value)
  value)

(defun degrees->radians (degrees) (* degrees 2 pi (/ 360)))

(defun empty? (seq)
  (etypecase seq
    (list (null seq))
    (vector (zerop (length seq)))))
