(in-package :g1)

;;; There were previously a couple functions used in macro expansions
;;; defined here, but I moved them to macrology.lisp while flailing
;;; around investigating build failures.

;;; Trivial math utilities, defined here as inline functions,
;;; in case there are nonportable ways to rephrase them which
;;; might optimize better.

(declaim (inline single fcos fsin ftan pi/x fsqrt x^-1/2)
         (ftype (function (single-float) single-float) fcos fsin ftan fsqrt x^-1/2)
         (ftype (function (number) single-float) single pi/x))

(defun single (x) (float x 0.0f0))
(defun fsin (x)   (single (sin x)))
(defun fcos (x)   (single (cos x)))
(defun ftan (x)   (single (tan x)))
(defun fsqrt (x)  (single (sqrt x)))
(defun pi/x (x)   (single (/ pi x)))
(defun x^-1/2 (x) (/ (fsqrt x)))

(defun +mod3 (x y) 
  (declare (type (integer 0 2) x y))
  (let ((tmp (+ x y)))
    (if (>= tmp 3)
        (the (integer 0 2) (- tmp 3))
        tmp)))

;;;; 2D Vector Functions

(declaim (inline coord coord.x coord.y coord- square normsq len))

;; Debug version:
#+NIL
(defun v2 (x y) 
  (assert (integerp x))
  (assert (integerp y))
  (cons x y))

(deftype v2 () '(cons fixnum fixnum))

(defun v2 (x y) (cons x y))
(defun v2round (x y) (cons (round x) (round y)))
(defun v2.x (coord) (car coord))
(defun v2.y (coord) (cdr coord))
(defun v2+ (a b)
  (v2 (+ (v2.x a) (v2.x b))
      (+ (v2.y a) (v2.y b))))
(defun v2- (a b)
  (v2 (- (v2.x a) (v2.x b))
      (- (v2.y a) (v2.y b))))
(defun v2scale (v scale)
  (v2 (round (* (v2.x v) scale))
      (round (* (v2.y v) scale))))
(defun square (x) (* x x))
(defun v2normsq (vector) (+ (square (v2.x vector)) (square (v2.y vector))))
(defun v2len (vector) (sqrt (v2normsq vector)))
(defun v2angle (angle scale)
  (v2 (round (* scale (cos angle)))
      (round (* scale (sin angle)))))

(defun v2<= (a b)
  (and (<= (v2.x a) (v2.x b))
       (<= (v2.y a) (v2.y b))))



;;;; 3D Vector Functions


(defmacro with-elts ((sequence &rest elt-names) &body body)
  (let ((seq (gensym "SEQ")))
    `(let* ((,seq ,sequence)
            ,@(loop for i upfrom 0
                    for elt-name in elt-names
                    collect `(,elt-name (elt ,seq ,i))))
       (declare (ignorable ,@elt-names))
       ,@body)))


(defmacro with-vector ((name &optional expression) &body body)
  `(let ((,name ,(or expression name)))
     (with-elts (,name ,@(symbol-xyz-list name))
     ,@body)))

(defmacro with-vectors (names &body body)
  (if (endp names)
      `(progn ,@body)
      `(with-vector (,(first names))
         (with-vectors ,(rest names) ,@body))))

(deftype v3 () '(simple-array single-float (3)))

;; TODO: This should really be an inline function with a compiler macro for the constant case..
(defmacro vec (x y z)
  `(let ((v (make-array 3 :element-type 'single-float :adjustable nil :fill-pointer nil)))
     (setf (elt v 0) (float ,x 0.0f0)
           (elt v 1) (float ,y 0.0f0)
           (elt v 2) (float ,z 0.0f0))
     v))

(defun v2->v3 (v2) (vec (single (v2.x v2)) (single (v2.y v2)) 0.0f0))
(defun ->v3 (vector) 
  (etypecase vector
    (cons (vec (single (v2.x vector)) (single (v2.y vector)) 0.0f0))
    (v3 vector)))

(declaim (inline v.x v.y v.z))

(defun v.x (vec) 
  (declare (type v3 vec))
  (elt vec 0))

(defun v.y (vec) 
  (declare (type v3 vec))
  (elt vec 1))

(defun v.z (vec) 
  (declare (type v3 vec))
  (elt vec 2))

;; Should I really be inlining these array-munging functions? Probably doesn't help much.
(declaim (inline dot cross len v+ v- vneg vscaleto)
         (ftype (function (v3) single-float) len normsq)
         (ftype (function (v3) v3) vneg)
         (ftype (function (v3 v3) boolean) v=)
         (ftype (function (v3 v3) single-float))
         (ftype (function (v3 v3) v3) cross v+ v- vmin vmax)
         (ftype (function (v3 v3 single-float) v3) a+b*c))

(defun vmin (u v)
  (with-vectors (u v)
    (vec (min u.x v.x) (min u.y v.y) (min u.z v.z))))

(defun vmax (u v)
  (with-vectors (u v)
    (vec (max u.x v.x) (max u.y v.y) (max u.z v.z))))

(defun v= (u v)
  (with-vectors (u v)
    (and (= u.x v.x) (= u.y v.y) (= u.z v.z) t)))

(defun v+ (u v)
  (with-vectors (u v)
    (vec (+ u.x v.x) (+ u.y v.y) (+ u.z v.z))))

(defun v- (u v)
  (with-vectors (u v)
    (vec (- u.x v.x) (- u.y v.y) (- u.z v.z))))

(defun vneg (v)
  (with-vector (v) (vec (- v.x) (- v.y) (- v.z))))

(defun dot (u v)
  (declare (type v3 u v))           
  (with-vectors (u v)
    (+ (* u.x v.x) (* u.y v.y) (* u.z v.z))))

(defun vnormsq (v)
  (declare (type v3 v))
  (dot v v))

(defun vzerop (vector)
  (declare (type v3 vector))
  (zerop (dot vector vector)))

(defun len (v)
  (declare (type v3 v))
  (fsqrt (dot v v)))

(defun vscale (vector scale) ; inline me?
  (declare (type v3 vector)
           (type single-float scale))
  (with-vector (v vector)
    (vec (* scale v.x) (* scale v.y) (* scale v.z))))

(defun normalize (vector)
  (declare (type v3 vector))
  (vscale vector (x^-1/2 (dot vector vector))))

(defun vscaleto (vector new-length)
  (declare (type v3 vector))
  (vscale vector (* new-length (x^-1/2 (dot vector vector)))))

(defun safe-vscaleto (vector new-length)
  (declare (type v3 vector))
  (if (vzerop vector)
      vector
      (vscale vector (* new-length (x^-1/2 (dot vector vector))))))

(defun safe-normalize (vector)
  (declare (type v3 vector))
  (safe-vscaleto vector 1.0))

(defun stp (a b c)
  (declare (type v3 a b c))
  (dot a (cross b c)))

(defun cross (u v)
  (declare (type v3 u v))
  (with-vectors (u v)
    (vec (- (* u.y v.z) (* u.z v.y))
         (- (* u.z v.x) (* u.x v.z))
         (- (* u.x v.y) (* u.y v.x)))))

(defun a+b*c (p pq l)
  (declare (type v3 p pq)
           (type single-float l))
  (with-vectors (p pq)
    (vec (+ p.x (* l pq.x))
         (+ p.y (* l pq.y))
         (+ p.z (* l pq.z)))))

(defun vdist (a b)
  (len (v- a b)))

(defun vdist<= (a b dist)
  (<= (vnormsq (v- a b)) (square dist)))

(defun v3angle-xy (z scale angle)
  (vec (* scale (cos angle))
       (* scale (sin angle))
       z))

(defun midpoint (a b) (vscale (v+ a b) 0.5))

(defun v3normxy (v)
  (with-vector (v)
    (vec (- v.y) v.x v.z)))
