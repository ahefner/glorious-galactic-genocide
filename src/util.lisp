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

(declaim (inline clamp))
(defun clamp (x min max)
  (max min (min max x)))
