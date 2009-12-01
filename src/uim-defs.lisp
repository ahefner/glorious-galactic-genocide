(in-package :g1)

(ffi:clines "#include <SDL/SDL.h>")

;;;; Resource Management

;;;; The way this is works is objects with a well-defined lifetime
;;;; (such as UI gadgets and panels) will inherit this mixin and the
;;;; object-is-alive slot will indicate whether the object is still in
;;;; use. Such an object, once discarded, can never return to
;;;; life. These objects often own temporary images (text labels,
;;;; mostly) and this way we have an easy way of instrumenting the
;;;; code to check for leaks.

(defclass dynamic-object ()
  ((object-is-alive :accessor object-is-alive :initform t)))

(defclass global-owner () ())
(defmethod object-is-alive ((object global-owner)) (declare (ignore object)) t)

;;; Objects can do resource cleanup here.
(defgeneric finalize-object (object)
  (:method ((object dynamic-object)) (declare (ignore object)))
  (:method :after ((object dynamic-object))
    (setf (object-is-alive object) nil)))

;;;; UI context

(defstruct uic
  ;; Bounding rectangle, in absolute screen coordinates. OpenGL will
  ;; have transformation applied so that abx/aby is the origin for
  ;; drawing operations.
  abx aby width height
  ;; Pointer position. amx/amy are in absolute pixel
  ;; coordinates. mx/my are transformed relative to abx/aby.
  amx amy mx my
  ;; Button and modifier state
  buttons
  buttons-pressed
  buttons-released
  modifiers
  modifiers-pressed
  modifiers-released
  active
  time delta-t)

(defclass gadget (dynamic-object)
  ((next-gadget :accessor next-gadget :initarg :next-gadget :initform nil)
   (parent-gadget :accessor parent-gadget :initarg :parent-gadget :initform nil)))

(defmacro keysym (name)
  `(cx :int ,(format nil "SDLK_~A" (if (= 1 (length (string name))) 
                                       (string-downcase (string name))
                                       name))))

(defconstant +left+ 1)
(defconstant +middle+ 2)
(defconstant +right+ 4)
(defconstant +scroll-up+ 8)
(defconstant +scroll-down+ 16)
(defconstant +scroll-left+ 32)
(defconstant +scroll-right+ 64)

;;;; Presentations

(defstruct presentation object type children)

(defmacro presenting ((uic object &key (type nil typep)) &body body-clauses)
  (let ((hit-sym (gensym "HIT"))
        display-clause hit-clause)
    (dolist (clause body-clauses)
      (unless (listp clause) (error "Malformed clause ~A in PRESENTING" clause))
      (case (first clause)
        (:display
         (when display-clause (error "Duplicate :display clause"))
         (setf display-clause (rest clause)))
        (:hit
         (when hit-clause (error "Duplicate :hit clause"))
         (setf hit-clause (rest clause)))
        (t (error "Unknown clause type ~A" (first clause)))))
    `(let (,hit-sym)
       (let ((object ,object)
             (type ,type)
             (presentation-children
              (let ((*presentation-stack* nil))
                ,@display-clause
                ,(when hit-clause
                  `(let ((region (progn ,@hit-clause))
                         (uic ,uic))
                     (setf ,hit-sym (funcall region (uic-mx uic) (uic-my uic)))))
                *presentation-stack*)))
         ;;(format t "~&presenting ~A / ~A  --  hit: ~A~%" ,object ,type ,hit-sym)
         ,(unless typep `(setf type object))
         (when ,hit-sym         
           (ecase (funcall *presentation-query* object type)
             (:discard  #| Discard this subtree |#)
             (:accept   #| Accept this presentation, push on to presentation stack |#            
              (push-new-presentation object type presentation-children))
             (:recurse  #| Don't push this presentation, but push accepted children |#
              (setf *presentation-stack* (nconc presentation-children *presentation-stack*)))))))))

(defmacro query-presentations (callback-lambda &body body)
  `(let ((*presentation-stack* nil)
         (*presentation-query* ,callback-lambda))
     ,@body))

;;;; Regions

;;; If circle is inlined, any half-decent CL ought to optimize the
;;; closure creation away. ECL doesn't, because it's a goddamned sorry
;;; piece of shit excuse for a Lisp compiler.

(declaim (inline circle))

(defun circle (cx cy radius)
  (let ((r^2 (square radius)))
    (lambda (x y)
      (<= (+ (square (- x cx)) (square (- y cy))) r^2))))

#+NIL
(presenting (uic star)
  (:display (draw-star star x y))
  (:hit (pointer-radius-test uic x y radius)))