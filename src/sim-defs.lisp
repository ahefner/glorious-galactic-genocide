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
   (armor-level :accessor armor-level-of :initform 0)
   ;; Runtime bullshit:
   (thumbnail :initform nil :initarg :thumbnail)
   #+NIL (serial :reader design-serial :initform (get-new-design-serial))
   (slot-num :accessor design-slot-num :initform 0 :initarg :slot-num)
   (name-label :accessor name-label-of :initform nil))) ;; DON'T FORGET TO FREE THESE!! ...

;;;; Technologies

(defclass tech (named)                  ; holy fuck look at all this bullshit!
  ((description :reader description-of :initform nil :initarg :description)

   ;; Tech level (used to determine price as 100*(level^1.6)
   (level :reader level-of :initform 0 :initarg :level)

   ;; Probability that tech will be available to a player in a given game (default 50%)
   (probability :reader probability-of :initform 0.50 :initarg :probability)

   ;; Engine speed of this tech (only engines should have nonzero speeds)
   (engine-speed :reader engine-speed :initform 0 :initarg :engine-speed)

   ;; Speed bonus of this tech (for specials that improve travel speed):
   (speed-bonus :reader speed-bonus :initform 0 :initarg :speed-bonus)

   ;; Range bonus (for reserve tanks and fuel techs)
   (range-bonus :reader range-bonus :initform 0 :initarg :range-bonus)

   ;; Contribution to overall starship armor (WTF does this do?)
   (armor-level :accessor armor-level-of :initform 0 :initarg :armor-level)

   ;; Shield level (cumulative with armor level, I guess)
   (shield-level :accessor shield-level-of :initform 0 :initarg :shield-level)

   ;; Hull factor scales the base hits of a ship size.
   (hull-modifier :accessor hull-modifier-of :initform 1 :initarg :hull-modifier)

   ;; Beam defense contribution
   (beam-defense :accessor beam-defense-of :initform 0 :initarg :beam-defense)

   ;; Scaling of enemy shields (when calculating weapon effect):
   (shield-scaling :reader shield-scaling-of :initform 1.0 :initarg :shield-scaling)

   ;; Weapon damage:
   (damage :accessor damage-of :initform nil :initarg :damage)

   ;; Size modifier (base size is computed from tech level unless otherwise specified):
   (size-modifier :accessor size-modifier-of :initform 1.0 :initarg :sizemod)

   ;; Cost modifier (base cost is computed from tech level unless otherwise specified):
   (cost-modifier :accessor cost-modifier-of :initform 1.0 :initarg :costmod)

   ;; Speed (of missiles and torpedos)
   (projectile-speed :accessor projectile-speed-of :initform 2.0 :initarg :projectile-speed)

   ;; Targetting bonus (for weapons)
   (weapon-targetting-bonus :accessor weapon-targetting-bonus-of :initform 0 :initarg :weapon-targetting-bonus)

   ;; Range (of targettable combat items, in unspecified units)
   (range :accessor range-of :initform 1.0 :initarg :range)

   ;; List of planet types this tech allows colonization of
   (colonizable :initform nil :initarg :colonizable)

   ;; If a tech has multiple variations which should be gained
   ;; simultaneously (such as a hull and armor tech), link one to the
   ;; other:
   (linked-to :reader linked-to :initform nil :initarg :linked-to)
   
   
   ;; --- Runtime bullshit ---
   (name-label :accessor name-label-of :initform nil)))

(defclass ship-tech (tech) ())

(defclass special-tech (ship-tech) ())
(defclass engine (ship-tech) ())
(defclass hull (ship-tech) ())
(defclass armor (hull) ())
(defclass shield (ship-tech) ())

(defclass weapon (ship-tech) ())
(defclass energy-weapon (weapon) ())
(defclass beam (energy-weapon) ())
(defclass particle-weapon (energy-weapon) ())
(defclass missile (weapon) ())
(defclass torpedo (weapon) ())
(defclass projectile (weapon) ())
(defclass bomb (weapon) ())

(defclass global-tech (tech) ())
(defclass fuel (global-tech) ())

(defstruct (range (:constructor range (min max))) min max)
(defgeneric roll (distribution)
  (:method ((r range)) (+ (range-min r) (random (1+ (- (range-max r) (range-min r)))))))

(defvar *name->tech* (make-hash-table))
(defun find-tech (name) (gethash name *name->tech*))

(defmacro deftech ((level class name-sym &rest initargs) description)
    `(progn
       (setf (gethash ',name-sym *name->tech*)
             (make-instance ',class :description ,description ,@initargs :level ,level :name (pretty-sym ',name-sym)))))
