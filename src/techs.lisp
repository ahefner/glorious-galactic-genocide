(in-package :g1)

;;;; Underlying protocols for techs

(defun can-colonize? (tech planet-type)
  (and tech (find planet-type (slot-value tech 'colonizable))))


;;;; Tech definitions

(clrhash *name->tech*)

(deftech (special-tech reserve-tanks :range-bonus 3)
    "Extends ship range by 3 light-years.")

(defparameter *friendly-planet-types* 
  '(terran oceanic jungle arid desert tundra minimal))

(deftech (special-tech colony-base :colonizable *friendly-planet-types*)
    "A colony base can be used to create a new settlement on any uninhabited planet with a breathable atmosphere. The colony ship is dismantled and recycled by the new colony.")


;;; Engines

(deftech (engine ion-drive :engine-speed 1)
    "Allowing acceleration up to half the speed of light, the Ion Drive made interstellar travel practical for the first time.")

(deftech (engine pulse-drive :engine-speed 2)
    "Reaching nearly the speed of light, the Pulsed Nuclear Drive offers a substantial speed improvement over the basic Ion Drive, but is only practical on very large ships.")

(deftech (engine sublight-drive :engine-speed 2)
    "Sublight Drives are a miniaturized engine technology that can propel a ship to nearly the speed of light. Comparable with Pulse Drives in speed, the smaller Sublight Drive consumes less space and can be equipped on ships of any size.")

(deftech (engine stardrive :engine-speed 3)
    "Revolutionary advances in physics lead to the proposal of the Stardrive, the first FTL engine. Capable of accelerating a starship up to 150% of the speed of light, it enables travel three times faster than the standard Ion Drive.")

(deftech (engine hyperdrive :engine-speed 6)
    "The Hyperdrive creates a tunnel through shallow hyperspatial dimensions, allowing a ship to emerge at its destination having appeared to travel at three times the speed of light. The energy required to maintain the tunnel increases exponentially with size, making Hyperdrives impractical for larger vessels.")

(deftech (engine warp-engine :engine-speed 4)
    "A refinement of the Stardrive principle, Warp Engines allow a ship to travel at up to twice the speed of light.")

(deftech (engine tachyon-engine :engine-speed 5)
    "Originally proposed in 2089 by an obscure Human science fiction author named Jamal Huso, his \"tachyon engine\" was dismissed as quackery and forgotten until recently, when a theoretical physicist chanced upon one of his novels and realized the idea was sound. The tachyon engine can accelerate a ship up to 2.5x the speed of light.")

(deftech (engine trans-light-drive :name "Trans-light Drive" :engine-speed 6)
    "The ultimate refinement of the principle behind the Stardrive and Warp Drive, Trans-light Drives can propel a starship at up to three times the speed of light.")

