(in-package :g1)

;;;; Underlying protocols for techs

(defun can-colonize? (tech planet-type)
  (and tech (find planet-type (slot-value tech 'colonizable))))


;;;; Tech definitions

(clrhash *name->tech*)

(deftech (special-tech reserve-tanks)
    "Extends ship range by 2 light-years.")

(defparameter *friendly-planet-types* 
  '(terran oceanic jungle arid desert tundra minimal))

(deftech (special-tech colony-base :colonizable *friendly-planet-types*)
    "A colony base can be used to create a new settlement on any uninhabited planet with a breathable atmosphere. The colony ship is dismantled and recycled during this process.")


