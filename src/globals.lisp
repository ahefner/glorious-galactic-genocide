;;;; Global definitions.

;;;; We have to put them here and define them during compilation,
;;;; otherwise the compiler will warn when compiling files that use
;;;; them, because it hasn't necessarily seen the definition.

;;;; That said, I don't see why I couldn't move some of this shit to
;;;; uim-defs.lisp.

(in-package :g1)

;;; Graphics and whatnot. Elimate these.

(defvar *packset* nil)

(defvar *gadget-root*)
(defvar *gameui*)
(defvar *presentation-stack*)
(defvar *grab-id* nil)

;;; Object currently being inspected by a panel, to highlight in the starmap.
(defvar *selected-object* nil)

(defparameter *presentation-query* (constantly :discard))

(defvar *global-owner* nil)

(defvar *player* nil)
(defvar *universe* nil)

;; Needed in designer due to questionable design of tech info printer.
(defvar *design* nil)

(defvar *button-a* nil)                     ; Standard button-style

;; Really, you need one of these per text style. This will be for the "default" text style (:sans 11)
(defvar *word-map* (make-hash-table :test 'equal))

(defvar *debug-show-packset* nil)

(defvar *total-frames* 0)
(defvar *devmode* t)
(defparameter *starfield-depth* 200)

;;;; ...

(defstruct cacheobj value derived)      ; Nowhere else reasonable to define this..

;;;; Inline functions

(declaim (inline f->b))

(defun f->b (f)
  (clamp (round (* f 255)) 0 255))

