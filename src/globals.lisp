;;;; Global variables.

;;;; We have to put them here and define them during compilation,
;;;; otherwise the compiler will bitch when compiling files that use
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
(defparameter *presentation-query* (constantly :discard))




(defvar *player* nil)

