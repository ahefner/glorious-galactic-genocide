(in-package :g1)

#+ecl
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
  (:method (object) (declare (ignore object)))
  (:method ((object dynamic-object)) (declare (ignore object)))
  (:method :around ((object dynamic-object))
    (if (object-is-alive object)
        (call-next-method)
        (warn "Finalize request for object ~A which is already dead!" object)))
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

;;; Note that for this to work, you have to include the SDL header in your lisp file.
(defmacro keysym (name)
  (declare (ignorable name))
  #+sbcl 666                            ; Dev hack
  #+ecl
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

(defconstant +control-mask+ #xC0)
(defconstant +alt-mask+ #xF00)          ; Merge Alt and Meta


;;;; Presentations

(defstruct presentation object type children)

(defmacro presenting ((uic object &key (type nil typep)) &key hit display)
  (let ((hit-sym (gensym "HIT")))            
    `(let (,hit-sym)
       (let ((object ,object)
             (type ,type)
             (presentation-children
              (let ((*presentation-stack* nil))
                ,display
                ,(when hit
                  `(let ((region ,hit)
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

;;; If circle is inlined, any serious CL compiler ought to optimize
;;; the closure creation away. ECL doesn't, which pisses me off.

(declaim (inline circle))

(defun circle (cx cy radius)
  (let ((r^2 (square radius)))
    (lambda (x y)
      (<= (+ (square (- x cx)) (square (- y cy))) r^2))))


;;;; Hi there.

(defstruct bar-style left right fill)
(defstruct button-style pressed released baseline)

;;;; Panels

(defclass panel-host-mixin ()
  ((closing-panel :accessor child-panel-close? :initform nil :initarg :init-closing-panel)
   (panel :accessor panel-of :initform nil :initarg :init-panel)
   (panel-y :accessor panel-y-of :initform 0 :initarg :init-panel-y)))

(defclass panel (dynamic-object) 
  ((panel-height :accessor panel-height :initarg :panel-height)
   (host :accessor host-of :initform nil :initarg :host )   
   (closing :accessor closing-p :initform nil)))

(defgeneric run-panel (panel uic bottom))

;;;; Cursor Layout Utility

(defstruct cursor left x y (newline-p t) (descent 0) (y-pad 0) (min-line-height 14) (color #(255 255 255)))

