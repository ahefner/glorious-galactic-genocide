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

(defclass at-star ()
  ((star :accessor star-of :initarg :star)))

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
                        :initform (vector 5 50 20 100))
   ;; Number of research choices a race receives initially.
   (initial-research-choices :reader initial-research-choices-of
                             :initform 6
                             :initarg initial-research-choices)))

(defclass player (named perishable)
  ((explored-stars :reader explored-stars-of :initform (make-hash-table :test 'eq))
   (race :reader race-of :initarg :race)
   (technologies :accessor technologies-of :initform nil)
   (presented-technologies :reader presented-technologies-of :initform (make-hash-table))
   ;; Potential techs: All the techs the player will ever be allowed to research, chosen at the beginning of the game.
   (potential-techs :accessor potential-techs-of :initform nil)
   ;; Available techs: Available for research immediately.
   (available-techs :accessor available-techs-of :initform nil)
   (style :accessor style-of :initarg :style :initform nil)
   (ship-designs-of :reader ship-designs-of :initform (make-array 9 :initial-element nil))
   (research-projects :accessor research-projects-of :initform (make-array 2 :initial-element nil))
   (event-list :accessor event-list-of :initform nil)
   ;; Current design in the editor. Under no circumstance may this point to a completed design.
   (working-design :accessor working-design-of :initform nil)
   ;; Property list of ship class names and the number of approved designs which used them.
   (model-history :accessor model-history-of :initform nil)

   ;;; Dubious runtime bullshit:
   (owned-images :accessor owned-images-of :initform (make-hash-table))

   ;;; All the slots below are cached values, updated whenever:

   (travel-range :accessor range-of :initform nil)
   ;; Number of population units current technology permits to inhabit per square of each terrain type:
   (adaptation-vector :accessor adaptation-vector-of :initarg :adaptation-vector)
   (growth-costs        :accessor growth-costs-of)

   (pollution-modifier :accessor pollution-modifier-of :initform 1.0 :initarg :pollution-modifier)
   (automation-level :accessor automation-level-of :initform 2 :initarg :automation-level)

   ;; Vector of stars colonized by this player. This is updated by
   ;; update-player-planets in one sweep across the universe.
   ;; convenience.
   (colonies :reader colonies :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defstruct pstyle label-color fill-color primary-color swizzle)

(defclass planet (named at-star)
  ((planet-type :reader planet-type-of :initarg :planet-type)
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

   ;; Current ship design under construction
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

(defclass fleet (owned ent in-universe perishable at-star)
  ((successor   :accessor successor-of   :initform nil) ; If merged, points to merged fleet. Forms a chain.   
   (destination :accessor destination-of :initform nil :initarg :destination)
   (orbital     :accessor orbital-of     :initform nil :initarg :orbital)
   (stacks      :accessor stacks-of      :initform nil :initarg :stacks)
   (speed       :accessor speed-of       :initform 1   :initarg :speed)   
   (initiative  :accessor initiative-of  :initform 0)
   ;; Displayed location of fleet in space. Decoupled from simulation!
   ;; True LOC changes discretely between turns. VLOC changes (mostly)
   ;; continuously, animated in real time.
   (vloc        :accessor vloc :initarg :vloc))
  (:default-initargs :star nil))

(defstruct stack design count fleet)

(defparameter *size-list* #("Shuttle" "Destroyer" "Cruiser" "Dreadnought"))

(defclass design (named owned)
  ((type :accessor design-type :initarg :type)
   (techs :accessor design-tech-slots :initarg :techs)
   (tech-counts :accessor design-tech-counts :initarg :tech-counts)
   (engine :accessor engine-of :initform nil :initarg :engine)
   ;; Index specifying which of the nine player designs this design occupies:
   (slot-num :accessor design-slot-num :initform nil :initarg :slot-num)

   ;; Attributes computed and fixed at creation time:
   (model-number :accessor model-number-of)
   (design-date :accessor design-date-of :initform (year-of *universe*) :initarg :design-date)

   ;; Derived attributes:
   (weight :accessor weight-of :initarg :weight :initform 1234)
   (range-bonus :accessor range-bonus-of :initform 0)
   (beam-defense :accessor beam-defense-of :initform 0)
   (target-acc :accessor target-acc-of :initform 1)
   (ecm-level-of :accessor ecm-level-of :initform 0)
   (thrust-of :accessor thrust-of :initform 1)
   (speed :accessor speed-of :initform 1 :initarg :speed)
   (cost  :accessor cost-of  :initarg :cost) ; Derived from the above, but fixed at design time.
   (armor-level :accessor armor-level-of :initform 0)

   ;; Runtime bullshit:
   ;; FIXME WTF, this doesn't belong here.
   (name-label :accessor name-label-of :initform nil))) ;; DON'T FORGET TO FREE THESE!! ...

;;;; Technologies

(defclass tech (named)                  ; holy fuck look at all this bullshit!
  ((description :reader description-of :initform nil :initarg :description)
   (designer-description :reader designer-description-of :initform nil :initarg :designer)
   (sym :reader sym-of :initarg :sym)

   ;; Tech level (used to determine price as 100*(level^1.6)
   (level :reader level-of :initform 0 :initarg :level)

   ;; Modifier for unit cost (scaled quadratically by tech level over magic fudge factor)
   (unit-cost-mod :reader unit-cost-mod-of :initarg :unit-cost-mod)

   ;; Probability that tech will be available to a player in a given game (default 50%)
   (probability :reader probability-of :initform 0.50 :initarg :probability)

   ;; Engine speed of this tech (only engines should have nonzero speeds)
   (engine-speed :reader engine-speed :initform 0 :initarg :engine-speed)

   ;; Speed bonus of this tech (for specials that improve travel speed):
   (speed-bonus :reader speed-bonus :initform 0 :initarg :speed-bonus)

   ;; Thrust/Maneuverability bonus
   (thrust-bonus :reader thrust-bonus :initform 0 :initarg :thrust-bonus)

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

   ;; Size modifier (total size of tech is the product of this and the base-size):
   (size-modifier :accessor size-modifier-of :initform 1.0 :initarg :sizemod)

   ;; Base size (in space units) of tech
   (base-size :accessor base-size-of :initarg :base-size)

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

   ;; Degree to which tech size is affected by the starship type's
   ;; space multiplier. Engines, 1.0 (100%, fully affected, so as to
   ;; scale up with ship size). For weapons, perhaps zero.
   (size-scaling :reader size-scaling-of :initform 0.0 :initarg :size-scaling)

   ;; Minimum size of a tech. TODO: If unsupplied, computed as 30% of the base size, rounded up? 
   (minimum-size :reader minimum-size-of :initarg :minimum-size   #|FIXME:|# :initform 5)

   ;; If a tech has multiple variations which should be gained
   ;; simultaneously (such as a hull and armor tech), link one to the
   ;; other:
   (linked-to :reader linked-to :initform nil :initarg :linked-to)
   
   ;; --- Runtime bullshit ---
   ;; FIXME: Why the fuck is this cached here? Let global-label do this shit.
   (small-name-label :accessor small-name-label-of :initform nil)
   (big-name-label :accessor big-name-label-of :initform nil)))

;;; TODO/FIXME: What is the 'base size' of a tech?
#+NIL
(defmethod minimum-size-of :around (tech)
  (or (call-next-method) (* 0.3 (base-size-of tech))))

(defclass ship-tech (tech) ())

;;; Abstract classes of ship techs. No defined tech should inherit
;;; directly from one of these.

;;; Note that non-bomb weapon :sizemods are all applied relative to
;;; the base-size of the WEAPON class. No subclass should change the
;;; base-size.

(defclass weapon (ship-tech) () (:default-initargs :base-size 20))
(defclass missile-weapon (weapon) ())
(defclass energy-weapon (weapon) ())

;;; Specific classes of ship techs. Every one of these needs to at least supply a base-size.

(defclass special-tech (ship-tech) () (:default-initargs :base-size 100))

(defclass engine (ship-tech) () (:default-initargs :base-size 15 :size-scaling 1.0))

(defclass hull (ship-tech) () (:default-initargs :base-size 15 :size-scaling 0.7))
(defclass armor (hull) () (:default-initargs :base-size 35 :size-scaling 0.7))

(defclass shield (ship-tech) () (:default-initargs :base-size 20 :size-scaling 1.0))


(defclass beam (energy-weapon) ())
(defclass particle-weapon (energy-weapon) ())
(defclass missile (missile-weapon) ())
(defclass torpedo (missile-weapon) ())
(defclass projectile (weapon) ())

(defclass bomb (weapon) () (:default-initargs :base-size 70))

(defclass global-tech (tech) ())
(defclass fuel (global-tech) ())

(defstruct (range (:constructor range (min max))) min max)
(defgeneric roll (distribution)
  (:method ((r range)) (+ (range-min r) (random (1+ (- (range-max r) (range-min r)))))))

(defgeneric distribution->string (dist)
  (:method ((this number)) (write-to-string this))
  (:method ((this range)) (format nil "~:D-~:D" (range-min this) (range-max this))))

(defvar *name->tech* (make-hash-table))
(defun find-tech (name) (gethash name *name->tech*))

(defmacro deftech ((level class name-sym &rest initargs) description &rest more-initargs)
  `(progn
     (let ((tech (orf (gethash ',name-sym *name->tech*) (make-instance ',class))))       
       (reinitialize-instance 
        tech
        :sym ',name-sym
        :description ,description
        ,@(append initargs more-initargs)
        :level ,level :name (pretty-sym ',name-sym)))))

(defstruct researching tech spent)

;;;; Events / Announcements. Modal events run first, grouped by
;;;; type. Modeless events are presented on the starmap screen.

(defclass player-event (owned)
  ((summary :reader summary-of :initform nil :initarg :summary)))

(defgeneric event-supercedes? (this by)
  (:method (this by)
    (declare (ignore this by))
    nil))

;;; Modal and modeless events are an exhaustive partition of player-event.
(defclass modal-event (player-event) ())
(defclass modeless-event (player-event) ())
(defclass non-ui-event (player-event) ())
(defclass sound-event (non-ui-event)
  ((name :initform nil :initarg :name)))

;;; Modal events are displayed at the beginning of the turn as
;;; bottom-panels.  Methods on this function are responsible for
;;; enqueueing the panels for display.
(defgeneric create-modal-event-panel (event host)
  (:method (event host)
    (declare (ignore host))
    (break "Modal event ~A doesn't implement create-modal-event-panel!" event)))

;;; Concrete event types:

(defclass new-tech-event (non-ui-event)
  ((tech :initarg :tech)))

(defclass explored-event (modeless-event at-star) ())

(defclass colony-ship-arrived-event (modeless-event at-star) ())

(defmethod event-supercedes? ((this explored-event) (that colony-ship-arrived-event))
  (eql (star-of this) (star-of that)))

;;;; Ship Design Templates

(defclass ship-type (named)
  ((space :reader space-of :initarg :space)
   (cost  :reader cost-of  :initarg :cost)
   (base-hits :reader base-hits-of :initarg :hits)
   (space-multiplier :reader space-multiplier-of :initarg :space-multiplier)
   (maneuverability :reader maneuverability-of :initarg :maneuverability)
   (slots :reader slots-of :initarg :slots)
   (schematic-position :reader schematic-position-of :initarg :schematic-position)
   (length-meters :reader length-meters-of :initarg :meters :initform 100)
   (weight :accessor weight-of :initarg :weight)

   ;; Cache slots, set at runtime:
   (thumbnail :initform nil)
   (texture-map :initform nil)
   (normal-map :initform nil)
   (light-map :initform nil)))

(defclass small-ship  (ship-type) () 

  (:default-initargs :space   100 :cost   20 :space-multiplier 1  :maneuverability 6 :weight 120    :hits 10))

(defclass medium-ship (ship-type) () 

  (:default-initargs :space   600 :cost  100 :space-multiplier 3  :maneuverability 3 :weight 6000   :hits 90))

(defclass large-ship  (ship-type) () 

  (:default-initargs :space  2200 :cost  450 :space-multiplier 10 :maneuverability 2 :weight 40000  :hits 350))

(defclass huge-ship   (ship-type) () 

  (:default-initargs :space  5000 :cost 1800 :space-multiplier 20 :maneuverability 1 :weight 120000 :hits 750))

(defclass hardpoint (named) 
  ((tech :accessor tech-of :initform nil :initarg :tech)
   (tech-class :initarg :tech-class)
   (allow-multi :initform nil :initarg :allow-multi)
   (empty-string :accessor empty-string-of :initarg :empty)
   ;; Index into tech/count arrays of design.
   (index :accessor index-of :initarg :index))     
  (:default-initargs :empty "(empty)"))

;;;; Hardpoint types: 
;;;;  weapon-mount: Holds a single weapon.
;;;;  battery-mount: Holds multiple weapons, limited only by available space.
;;;;  special-mount: Holds a special tech.
(defclass weapon-mount (hardpoint) ()
  (:default-initargs :tech-class 'weapon :empty "(none)"))

(defclass battery-mount (hardpoint) ()
  (:default-initargs :tech-class 'weapon :allow-multi t))

(defclass special-mount (hardpoint) ()
  (:default-initargs :tech-class 'special-tech :allow-multi nil))


