;;;; Global variables.

;;;; We have to put them here and define them during compilation,
;;;; otherwise the compiler will warn when compiling files that use
;;;; them, because it hasn't necessarily seen the definition.

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

;;; Object currently being inspected by a panel, to highlight in the starmap.
(defvar *selected-object* nil)

(defparameter *presentation-query* (constantly :discard))

(defvar *global-owner* nil)

(defvar *player*)
(defvar *universe*)

(defconstant light-years/units 65.0)
