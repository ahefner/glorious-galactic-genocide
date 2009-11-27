(in-package :g1)

;;;; Game definitions

(defclass named ()
  ((name :accessor name-of :initarg :name :initform nil)))

(defclass owned ()
  ((owner :accessor owner-of :initarg :owner :initform nil)))

(defclass in-universe ()
  ((universe :accessor universe-of :initarg :universe)))

(defclass universe ()
  ((stars :accessor stars :initarg :stars)
   (min-bound :accessor min-bound-of)
   (max-bound :accessor max-bound-of)
   (fleets :accessor fleets-in-transit :initform nil)
   (namelist :initform (shuffle (constellation-name-list)))
   (colors-list :initform (shuffle (list (vector 255  90  90)
                                         (vector 255 134   0)
                                         (vector 255 255  90)
                                         (vector 120 255 120)
                                         (vector  40 160 255)
                                         (vector 190 116 255))))))

(defclass ent ()
  ((loc :accessor loc :initarg :loc)))

(defclass star (named ent in-universe)
  ((style :initform (random 4))
   (fleets :accessor fleets-orbiting :initform nil)
   (owner :accessor owner-of)
   (planet :reader planet-of :initform nil)
   (spectral-class :reader spectral-class :initarg :spectral-class)
   ;; Grungy bits:
   (label-img :initform nil)))

(defclass race (named)
  ((traits :accessor traits-of :initform '(tellurian) :initarg :traits)
   (homeworld-type    :reader homeworld-type-of
                      :initform 'terran
                      :initarg :homeworld-type)
   (homeworld-terrain :reader homeworld-terrain-of
                      :initform (vector 20 60 5 3)
                      :initarg :homeworld-terrain)
   (homeworld-population :reader homeworld-population-of
                         :initform 50
                         :initarg :homeworld-population)
   (production-modifier :reader production-modifier-of
                        :initform 1.0
                        :initarg :production-modifier)
   (base-adaptation     :reader base-adaptation-of
                        :initform (vector 5 0 1 0)
                        :initarg :habitability)
   (base-growth-costs   :reader base-growth-costs-of
                        :initform (vector 5 50 20 100))))

(defclass player (named)
  ((explored-stars :reader explored-stars-of :initform (make-hash-table :test 'eq))
   (race :reader race-of :initarg :race)
   (technologies :reader technologies-of :initform (make-hash-table :test 'eq))
   (color :accessor color-of :initarg :color :initform nil)
   (ship-designs-of :reader ship-designs-of :initform (make-array 9))

   ;;; All the slots below are cached values:

   ;; Number of population units current technology permits to inhabit per square of each terrain type:
   (adaptation-vector :accessor adaptation-vector-of :initarg :adaptation-vector)
   (growth-costs        :accessor growth-costs-of)

   (pollution-modifier :accessor pollution-modifier-of :initform 1.0 :initarg :pollution-modifier)
   (automation-level :accessor automation-level-of :initform 2 :initarg :automation-level)

   ;; Vector of stars colonized by this player. This is updated by
   ;; <mumble> in one sweep across the universe rather than modified
   ;; incrementally, so it's sort of a cache for convenience.
   (colonies :reader colonies :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass planet (named)
  ((star :reader star-of :initarg :star)
   (planet-type :reader planet-type-of :initarg :planet-type)
   (planet-attribute :reader planet-attribute-of :initarg :planet-attribute :initform :abundant)
   (terrains :reader terrains-of :initform nil :initarg :terrains)
   (habitability :accessor habitability-of :initform 1.0 :initarg :habitability)
   (production-modifier :accessor production-modifier-of :initform 1.0 :initarg :production-modifier)
   (growth-modifier     :accessor growth-modifier-of     :initform 1.0 :initarg :growth-modifier)

   ;; Pollution is a planetary property, so that it persists after a colony is destroyed
   (pollution  :accessor pollution-of  :initform 0 :initarg :pollution)
   (colony :accessor colony-of :initform nil :initarg :colony)))

(defstruct (spend (:type vector))
  (cleanup 0)                               ; Waste cleanup
  (housing 0)                               ; Population growth bonus
  (terraform 0)                             ; Terraforming
  (factories 0)                             ; Factory construction
  (bases 0)                                 ; Missile base construction
  (shield 0)                                ; Planetary shield construction
  (ships 0)                                 ; Ship construction 
  (research 0))                             ; Research spending

(defclass colony (owned)   
  ((planet :reader planet-of :initarg :planet)

   ;; Total production available for spending this turn:
   (production :accessor production-of :initform 0)
   
   ;; Remaining production after whatever portions are allocated automatically
   (unallocated-production :accessor unallocated-production-of :initform 0)

   ;; Accumulated spending by category:
   (spending :accessor spending-vector-of :initform (make-spend))

   ;; Terrain units are counted in a 4 element vector: Land, Sea, Ice, Magma.   
   (population :accessor population-of :initform 0 :initarg :population)
   (factories  :accessor factories-of  :initform 0 :initarg :factories)   
   (new-pollution :accessor new-pollution-of :initform 0)))

(declaim (inline land# ocean# ice# magma#))

(defun land#  (planet) (aref (terrains-of planet) 0))  
(defun ocean# (planet) (aref (terrains-of planet) 1))
(defun ice#   (planet) (aref (terrains-of planet) 2))
(defun magma# (planet) (aref (terrains-of planet) 3))

;;;; Ships and fleets

(defclass fleet (owned ent in-universe)
  ((star :accessor star-of :initform nil :initarg :star)
   (stacks :accessor stacks-of :initform nil :initarg :stacks)
   (speed :accessor speed-of :initform 1 :initarg :speed)))

(defstruct stack design count fleet)

(defclass design (named)
  ((speed :accessor speed-of :initform 1 :initarg :speed)))

