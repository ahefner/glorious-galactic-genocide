;;;; Global variables.

;;;; We have to put them here and define them during compilation,
;;;; otherwise the compiler will warn when compiling files that use
;;;; them, because it hasn't necessarily seen the definition.

;;;; That said, I don't see why I couldn't move some of this shit to
;;;; uim-defs.lisp.

(in-package :g1)

(defvar *stars00* nil)
(defvar *stars01* nil)
(defvar *stars02* nil)
(defvar *stars03* nil)

(defvar *gamebar-fill* nil)
(defvar *panel-fill* nil)

(defvar *packset* nil)

(defvar *gadget-root*)
(defvar *gameui*)
(defvar *presentation-stack*)
(defvar *grab-id* nil)

;;; Object currently being inspected by a panel, to highlight in the starmap.
(defvar *selected-object* nil)

(defparameter *presentation-query* (constantly :discard))

(defvar *global-owner* nil)

(defvar *player*)
(defvar *universe*)

(defvar *button-a* nil)                     ; Standard button-style
(defvar *slider160* nil)                    ; 160px slider texture with bevelled ends

;; Stupid hack for drawing a layer below the stars (but above the space background).
(defvar *starmap-display-under-hook* (constantly nil))

;; Really, you need one of these per text style. This will be for the "default" text style (:sans 11)
(defvar *word-map* (make-hash-table :test 'equal))

(defvar *debug-show-packset* nil)
