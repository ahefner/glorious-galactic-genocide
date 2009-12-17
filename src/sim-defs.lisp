(in-package :g1)

;;;; "Constants of the Universe"

(defconstant units/light-years 80.0)
(defconstant +missile-base-cost+ 1000)

;;;; Game definitions

(defclass named ()
  ((name :accessor name-of :initarg :name :initform nil)))

(defclass owned ()
  ((owner :accessor owner-of :initarg :owner :initform nil)))

(defclass in-universe ()
  ((universe :accessor universe-of :initarg :universe)))

(defclass universe ()
  ((stars :accessor stars :initarg :stars)
   (year :accessor year-of :initform 2200)
   (min-bound :accessor min-bound-of)
   (max-bound :accessor max-bound-of)
   (fleets :accessor fleets-in-transit :initform nil)
   (namelist :initform (shuffle (constellation-name-list)))
   (styles-list :initform (shuffle (get-player-styles)))))

(defclass perishable ()
  ((alive :accessor alive? :initform t)))

(defclass ent ()
  ((loc :accessor loc :initarg :loc)))

(defclass star (named ent in-universe)
  ((style :initform (random 4))
   (fleets :accessor fleets-orbiting :initform nil)
   (owner :accessor owner-of)
   (planet :reader planet-of :initform nil)
   (spectral-class :reader spectral-class :initarg :spectral-class)
   ;; Grungy bits:
   (screen-coord :initform nil :accessor screen-coord-of) ; v2
   (highlight-level :initform 0 :accessor highlight-level-of)
   (highlight-target :initform 0 :accessor highlight-target-of)
   (label-img :initform nil)))

;; Regarding stars and fleets: the relationship between (member
;; .. (fleets-orbiting star)) and (star-of fleet) isn't reflexive. When a
;; fleet is in the :departing state, its star-of still points to the
;; star it's departing from, but the fleet has been removed from the
;; list of fleets orbiting the star.

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

(defclass player (named perishable)
  ((explored-stars :reader explored-stars-of :initform (make-hash-table :test 'eq))
   (race :reader race-of :initarg :race)
   (technologies :reader technologies-of :initform (make-hash-table :test 'eq))
   (style :accessor style-of :initarg :style :initform nil)
   (ship-designs-of :reader ship-designs-of :initform (make-array 9))

   (owned-images :accessor owned-images-of :initform (make-hash-table))

   ;;; All the slots below are cached values, updated at the beginning of each turn:

   (travel-range :accessor range-of :initform 4)
   ;; Number of population units current technology permits to inhabit per square of each terrain type:
   (adaptation-vector :accessor adaptation-vector-of :initarg :adaptation-vector)
   (growth-costs        :accessor growth-costs-of)

   (pollution-modifier :accessor pollution-modifier-of :initform 1.0 :initarg :pollution-modifier)
   (automation-level :accessor automation-level-of :initform 2 :initarg :automation-level)

   ;; Vector of stars colonized by this player. This is updated by
   ;; update-player-planets in one sweep across the universe.
   ;; convenience.
   (colonies :reader colonies :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defstruct pstyle label-color fill-color primary-color)

(defclass planet (named)
  ((star :reader star-of :initarg :star)
   (planet-type :reader planet-type-of :initarg :planet-type)
   (planet-attribute :reader planet-attribute-of :initarg :planet-attribute :initform :abundant)
   (terrains :reader terrains-of :initform nil :initarg :terrains)
   (habitability :accessor habitability-of :initform 1.0 :initarg :habitability)
   (production-modifier :accessor production-modifier-of :initform 1.0 :initarg :production-modifier)
   (growth-modifier     :accessor growth-modifier-of     :initform 1.0 :initarg :growth-modifier)

   ;; Pollution is a planetary property, so it persists after a colony is destroyed
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

(defstruct (budget (:type vector) (:include spend))
  (unspent 0))

(defstruct (spend% (:type vector))
  (growth 90)
  (defense 25)
  (ships 65)
  (research 85))

(defclass colony (owned perishable)
  ((planet :reader planet-of :initarg :planet)

   ;; Player-chosen spending preferences, in integer percentages (0...100)
   (spendings-prefs :accessor spending-prefs-of :initform (make-spend%) :initarg :spending-prefs)

   ;; Total production available for spending this turn:
   (production :accessor production-of :initform 0)
   
   ;; Remaining production after whatever portions are allocated automatically
   (unallocated-production :accessor unallocated-production-of :initform 0)

   ;; Accumulated spending by category:
   (spending :accessor spending-vector-of :initform (make-spend))

   ;; Current ship design under contrstruction
   (building-design :accessor building-design-of :initform nil)
   
   ;; Number of missile bases
   (num-bases :accessor num-bases-of :initform 0 :initarg :num-bases)

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

(defclass fleet (owned ent in-universe perishable)
  ((successor :accessor successor-of :initform nil) ; If merged, points to merged fleet. Forms a chain.
   (star :accessor star-of :initform nil :initarg :star)
   (destination :accessor destination-of :initform nil :initarg :destination)
   (orbital :accessor orbital-of :initform nil :initarg :orbital)
   (stacks :accessor stacks-of :initform nil :initarg :stacks)
   (speed :accessor speed-of :initform 1 :initarg :speed)   
   (initiative :accessor initiative-of :initform 0)
   (vloc :accessor vloc :initarg :vloc)))

(defstruct stack design count fleet)

(defparameter *size-list* #("Shuttle" "Destroyer" "Cruiser" "Dreadnought"))

(defparameter *design-slot-names*
  #(#("Port Mount"
      "Starboard Mount"
      "Accessory Compartment")

    #("Central Mount"
      "Port Mount"
      "Starboard Mount"
      "Port Compartment"
      "Starboard Compartment")
  
    #("First Foreward Battery"
      "Second Foreward Battery"
      "Stern Battery"
      "Port Battery"
      "Starboard Battery"
      "Cargo Bay"
      "Port Accessory Bay"
      "Starboard Accessory Bay")

    #("First Foreward Battery"
      "Second Foreward Battery"
      "Stern Battery"
      "Port Battery"
      "Starboard Battery"
      "Sensor Bay"
      "Cargo Bay"
      "Port Accessory Bay"
      "Starboard Accessory Bay")))

(defclass design (named)
  ((size  :accessor size-of  :initform 0 :initarg :size)
   (techs :accessor design-tech-slots :initarg :techs)
   (engine :accessor engine-of :initform nil :initarg :engine)
   (range-bonus :accessor range-bonus-of :initform 0)
   ;; Derived attributes:
   (speed :accessor speed-of :initform 1 :initarg :speed)
   (cost  :accessor cost-of  :initarg :cost) ; Derived from the above, but fixed at design time.
   ;; Runtime bullshit:
   (thumbnail :initform nil :initarg :thumbnail)
   #+NIL (serial :reader design-serial :initform (get-new-design-serial))
   (slot-num :accessor design-slot-num :initform 0 :initarg :slot-num)
   (name-label :accessor name-label-of :initform nil)))

;;;; Technologies

(defclass tech (named)
  ((description :reader description-of :initform nil :initarg :description)

   ;; Engine speed of this tech (only engines should have nonzero speeds)
   (engine-speed :reader engine-speed :initform 0 :initarg :engine-speed)

   ;; Speed bonus of this tech (for specials that improve travel speed):
   (speed-bonus :reader speed-bonus :initform 0 :initarg :speed-bonus)
   ;; Range bonus (for reserve tanks)
   (range-bonus :reader range-bonus :initform 0 :initarg :range-bonus)

   ;; List of planet types this tech allows colonization of
   (colonizable :initform nil :initarg :colonizable)

   (name-label :accessor name-label-of :initform nil)))

(defclass ship-tech (tech) ())

(defclass special-tech (ship-tech) ())
(defclass engine (ship-tech) ())

(defclass nulltech (tech) ())

(defvar *name->tech* (make-hash-table))
(defun find-tech (name) (gethash name *name->tech*))

(defmacro deftech ((class name-sym &rest initargs) description)
    `(progn
       (setf (gethash ',name-sym *name->tech*)
             (make-instance ',class :description ,description ,@initargs :name (pretty-sym ',name-sym)))))
