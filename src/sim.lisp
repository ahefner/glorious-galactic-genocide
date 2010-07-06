(in-package :g1)

(defmethod alive? (object) (declare (ignore object)) t)

;;;; Printers

(defmethod print-object ((planet planet) stream)
  (print-unreadable-object (planet stream :identity nil :type t)
    (ignore-errors 
      (apply 'format stream "~A: ~A ~D/~D/~D/~D" 
             (name-of planet)
             (planet-type-of planet)
             (coerce (terrains-of planet) 'list)))))

(defmethod print-object ((colony colony) stream)
  (print-unreadable-object (colony stream :identity nil :type t)
    (ignore-errors 
      (format stream "~A: pop ~D/~A ind ~D"
              (name-of colony)
              (population-of colony)
              (ignore-errors (compute-max-population colony))
              (factories-of colony)))))

(defmethod print-object ((star star) stream)
  (print-unreadable-object (star stream :identity nil :type t)
    (ignore-errors 
      (format stream "~W w/ ~A planet" 
              (name-of star)
              (or (and (planet-of star) (planet-type-of (planet-of star))) "no"))
      (and.. (owner-of star)
             (format stream " owned by ~W" (name-of $))))))

(defmethod print-object ((race race) stream)
  (print-unreadable-object (race stream :identity nil :type t)
    (format stream "~A" (name-of race))))

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :identity nil :type t)
    (format stream "~A, a ~A" (name-of player) (name-of (race-of player)))))

(defmethod print-object ((object named) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~W" (name-of object))))

;;;; Duct tape methods

(defmethod owner-of ((star star))
  (and.. (planet-of star) (owner-of $)))

(defmethod owner-of ((planet planet))
  (and.. (colony-of planet) (owner-of $)))

(defmethod name-of ((this colony)) (name-of (planet-of this)))

(defmethod star-of ((this star)) this)
(defmethod star-of ((this colony)) (star-of (planet-of this)))

(defmethod fleets-orbiting ((this planet)) (fleets-orbiting (star-of this)))
(defmethod fleets-orbiting ((this colony)) (fleets-orbiting (star-of this)))

(defmethod loc ((this colony)) (loc (star-of (planet-of this))))
(defmethod loc ((this planet)) (loc (star-of this)))

(defmethod colony-of ((star star))
  (and.. (planet-of star) (colony-of $)))

;;;; Hello again.

(defparameter *race-human*
  (make-instance 'race :name "Human"))

(defun player-currency (player)
  (declare (ignore player))
  "GC")

(defun update-player-planets (universe)
  (loop for player in (all-players universe) do (setf (fill-pointer (colonies player)) 0))
  (loop for star across (stars universe)
        as planet = (planet-of star)
        as owner = (owner-of star)
        when owner do (vector-push-extend (colony-of (planet-of star)) (colonies owner)))
  (loop for player in (all-players universe)
        do (sort (colonies player) #'string<= :key #'name-of)))

;;; Compute player adaptations and growth rates based on racial traits
;;; and researched technology.
(defun update-player-stats (player)
  (with-slots (adaptation-vector growth-costs race) player
    (setf adaptation-vector (base-adaptation-of race)
          growth-costs (base-growth-costs-of race))
    #| TODO: Apply technology here! :) |#
    player))

(defun all-players (universe)
  (remove-duplicates (remove nil (map 'list #'owner-of (stars universe)))))

(defun explore-star (star player)
  (setf (gethash star (explored-stars-of player)) t))

(defun explored? (player star)
  (gethash star (explored-stars-of player)))

(defun players-in-contact (player-1 player-2)
  (declare (ignore player-1 player-2))
  ;; TODO!!
  t)

(defun all-planets (universe)
  (remove nil (map 'list #'planet-of (stars universe))))

(defun distance-from-player (player staroid)
  (reduce #'min (colonies player) :key (lambda (col) (len (v- (loc col) (loc staroid))))))

(defun distance-from-player-ly (player staroid)
  (ceiling (distance-from-player player staroid) units/light-years))

;;;;

(defun get-player-styles ()
  (list (make-pstyle :label-color #(255  90  90) :fill-color #(180   0   0) :primary-color #(255   0   0) :swizzle "gbba")
        (make-pstyle :label-color #(255 134   0) :fill-color #(167 106   0) :primary-color #(255 160   0) :swizzle "grba")
        (make-pstyle :label-color #(255 255  90) :fill-color #(131 127  55) :primary-color #(255 255   0) :swizzle "ggba")
        (make-pstyle :label-color #(170 224 108) :fill-color #( 86 140  22) :primary-color #(  0 255   0) :swizzle "rgba")
        (make-pstyle :label-color #( 40 160 255) :fill-color #(  0  64 128) :primary-color #(  0 200 255) :swizzle "brga")
        (make-pstyle :label-color #(190 116 255) :fill-color #( 64  64 128) :primary-color #(255   0 255) :swizzle "gbga")))

;;;; Planetary economy

;;; Industrial output is constrained by requiring sufficient population to operate the factories.
(defun max-factories (colony)
  (* (automation-level-of (owner-of colony)) (population-of colony)))

(defun active-factories (colony)
  (min (max-factories colony) (factories-of colony)))

(defun compute-production (colony)
  (values
   ;; Production
   (+ (* 4 (expt (production-modifier-of (planet-of colony)) 2))
    (* (production-modifier-of (race-of (owner-of colony)))
       (production-modifier-of (planet-of colony))
       (+ (population-of colony) (active-factories colony))))
   ;; Pollution - each active factory produces a base of one third
   ;; unit of pollution per turn, modified by player technology,
   ;; rounded down.
   (floor (* (pollution-modifier-of (owner-of colony))
             (active-factories colony))
          3)))

(defun simulate-production (colony)
  (assert (zerop (new-pollution-of colony))) ; New pollution should be tenured already
  (multiple-value-bind (new-prod new-pol) (compute-production colony)
    (incf (production-of colony) new-prod)    ; Zeroed first by colony-turn-prep
    (incf (new-pollution-of colony) new-pol))
  (setf (unallocated-production-of colony) (production-of colony))) ; XXX KILL THIS

(defun dump-waste (colony)
  ;; Ocean and magma terrain have limited ability to absorb pollution as it is created.
  ;;(printl :before-dump (new-pollution-of colony))
  (setf (new-pollution-of colony)        
        (max 0 (floor 
                (- (new-pollution-of colony)
                   (* 3.00 (magma# (planet-of colony)))
                   (* 0.15 (ocean# (planet-of colony))))))))

(defun maxpops-by-terrain (planet owner)
  (map 'vector
       (lambda (x y) (round (* x y (habitability-of planet))))
       (adaptation-vector-of owner)
       (terrains-of planet)))

(defun planet-max-population (planet player)
  ;; Max population less pollution penalty (-1 pop unit per unit pollution)
  (round
   (max 0 (- (reduce #'+ (maxpops-by-terrain planet player))
             (pollution-of planet)))))

(defun compute-max-population (colony)
  (planet-max-population (planet-of colony) (owner-of colony)))

(defun compute-population-distribution (colony population)
  (let* ((max-population (compute-max-population colony))
         (maxpops (maxpops-by-terrain (planet-of colony) (owner-of colony)))
         (pop (min population max-population)) ; Initially, sustainable population.
         (displaced (- population pop))
         (pops (make-array 4 :initial-element 0)))
    ;; TODO: Will need revising for non-tellurian races
    (loop for tidx from 0 below 4
          as pophere = (min pop (aref maxpops tidx)) do
          (decf pop pophere)
          (incf (aref pops tidx) pophere))
    (values pops (+ displaced pop))))

(defparameter *base-pop-growth-cost* 50.0)

(defun terrain-development-costs (colony)
  (map 'vector
       (lambda (density) 
         (and (> density 0.00001) 
              (ceiling (* *base-pop-growth-cost*
                          (/ 5.0 density)
                          (growth-modifier-of (planet-of colony))))))
       (adaptation-vector-of (owner-of colony))))

;;; Calculate population growth given a production investment
(defun calculate-pop-growth (colony production)
  (let* ((maxpop (compute-max-population colony))
         (maxpops (compute-population-distribution colony maxpop))
         (curpop (population-of colony))
         (curpops (compute-population-distribution colony curpop))
         (vacant (map 'vector (lambda (max cur) (max 0 (- max cur))) maxpops curpops))
         (unit-costs (terrain-development-costs colony))
         (scosts (sort (map 'list 'list '(0 1 2 3) unit-costs '(land ocean tundra magma))
                       #'< :key (lambda (x) (or (second x) 99999999)))))
    ;(printl :scosts scosts)
    ;(printl :vacant vacant)
    ;(printl :total-cost (reduce #'+ (map 'vector '* vacant (map 'vector (lambda (x) (or x 0)) unit-costs))))
    
    (loop with total-growth = 0
          for (tidx cost terrain-type) in scosts
          as vacant-here = (aref vacant tidx)
          when cost do
          (loop while (and (>= production cost) (> vacant-here 0)) do
                (incf total-growth)
                (decf production cost)
                (decf vacant-here)
                #+NIL (printl :grew terrain-type cost production))
          finally 
          (return (values total-growth production)))
    ))

(defparameter *overpop-decay-rate* 0.10)

(defun simulate-population-growth (colony)
  (multiple-value-bind (growth remaining)
      (calculate-pop-growth colony (spend-housing (spending-vector-of colony)))
    (incf (population-of colony) growth)
    (setf (spend-housing (spending-vector-of colony)) remaining))
  (let ((maxpop (compute-max-population colony)))
    (when (= maxpop (population-of colony)) 
      (setf (spend-housing (spending-vector-of colony)) 0))
    (when (> (population-of colony) maxpop)
      (setf (population-of colony) 
            (max 0 (- (population-of colony) 
                      (ceiling (* *overpop-decay-rate* (- (population-of colony) maxpop)))))))))

;; Additional production per population unit that goes into housing
;; automatically, to simulate natural population growth.
(defparameter *inherent-population-growth* 1.0)

(defparameter *factory-base-cost* 30)

(defun compute-factory-construction (colony spending)
  ;; Assumption: Base automation level will always be at least 2.
  ;; TODO: Support automation levels >2
  (let* ((level2-needed (max 0 (- (* 2 (population-of colony)) (factories-of colony))))
         (level2-cost *factory-base-cost*)
         (level2-buildable (truncate spending level2-cost))
         (level2-built (min level2-needed level2-buildable))
         (actual-cost (* level2-built level2-cost)))
    (assert (<= (* level2-built level2-cost) spending))
    (values level2-built (- spending actual-cost))))

(defun simulate-factory-construction (colony)
  (multiple-value-bind (built remaining)
      (compute-factory-construction colony (spend-factories (spending-vector-of colony)))
    (setf (spend-factories (spending-vector-of colony)) remaining)
    (incf (factories-of colony) built)))

(defparameter *waste-cleanup-cost* 2)

#+NIL
(defun allocate-funds (colony amount &optional (reason "??"))
  (let ((real-amount (min (ceiling amount) (unallocated-production-of colony))))
    (format t "~&~A: Allocated ~A BC for ~A~%" (name-of colony) real-amount reason)
    (decf (unallocated-production-of colony) real-amount)
    real-amount))

(defun budget-allocate (budget amount &optional (reason "??"))
  (declare (ignorable reason))
  (let ((real-amount (min (round amount) (budget-unspent budget))))
    (decf (budget-unspent budget) real-amount)
    real-amount))

(defun budget-allot-cleanup (colony budget)
  (incf (budget-cleanup budget)
        (budget-allocate budget 
                         (* *waste-cleanup-cost* (+ (pollution-of (planet-of colony)) 
                                                    (new-pollution-of colony)))
                         "ecology")))

(defun budget-allot-development (colony budget funds)
  (assert (<= funds (budget-unspent budget))) ; seems a little harsh..
  (let ((needed-factories (max 0 (- (max-factories colony) (factories-of colony))))
        (needed-population (max 0 (- (compute-max-population colony) (population-of colony)))))
    ;; Build factories first.
    (unless (zerop needed-factories)
      (let ((spent (budget-allocate budget (min funds (* needed-factories *factory-base-cost*)) "factories")))
        (incf (spend-factories budget) spent)
        (decf funds spent)))
    (assert (>= funds 0))
    ;; Then grow population.
    (unless (zerop needed-population)
      (multiple-value-bind (growth remaining) (calculate-pop-growth colony funds)
        (incf (budget-housing budget)
              (budget-allocate budget (min funds
                                           (if (= growth needed-population)
                                               (- funds remaining) 
                                               funds))
                               "housing"))))))

(defun compute-base-construction (colony amount)
  (declare (ignore colony))
  (let* ((cost +missile-base-cost+)
         (num-bases (truncate amount cost)))
    (values num-bases (- amount (* num-bases cost)))))

(defun simulate-base-construction (colony)
  (multiple-value-bind (new-bases remaining-funds)
      (compute-base-construction colony (spend-bases (spending-vector-of colony)))
    (incf (num-bases-of colony) new-bases)
    (setf (spend-bases (spending-vector-of colony)) remaining-funds)))

(defun compute-ship-construction (colony amount)
  (let* ((cost (cost-of (building-design-of colony)))
         (num-ships (truncate amount cost)))
    (values num-ships (- amount (* num-ships cost)))))

(defun simulate-ship-construction (colony)
  (when (building-design-of colony)
    (multiple-value-bind (num-built remaining-funds)
        (compute-ship-construction colony (spend-ships (spending-vector-of colony)))
      (unless (zerop num-built)
        (build-ships colony (building-design-of colony) num-built)
        (setf (spend-ships (spending-vector-of colony)) remaining-funds)))))

(defun post-cleanup-budget (colony)
  (let ((budget (make-budget)))
    ;; Production and pollution should already be computed..
    (setf (budget-unspent budget) (production-of colony))
    ;; Ecology is mandatory (for now).
    (budget-allot-cleanup colony budget)
    budget))

(defun colony-compute-budget (colony)
  (let ((budget (post-cleanup-budget colony))
        (prefs (spending-prefs-of colony)))
    ;; For each item in the user's spending preferences, allocate funds.
    (flet ((foo (reader) (truncate (* 1/100 (funcall reader prefs) (budget-unspent budget)))))
      (budget-allot-development colony budget (foo #'spend%-growth))
      (incf (budget-bases budget) (budget-allocate budget (foo #'spend%-defense) "Bases"))
      (when (building-design-of colony)
        (incf (budget-ships budget) (budget-allocate budget (foo #'spend%-ships) "Ships")))
      (incf (budget-research budget) (budget-allocate budget (foo #'spend%-research) "Research")))

    ;;(printl colony :fixme-wasted (budget-unspent budget))

    budget))

(defun colony-turn-prep (colony)
  ;; Now, prepare for the coming turn. Don't call this more than once per turn.
  (setf (production-of colony) 0)
  (simulate-production colony)          ; Generate production points and new pollution.  
  (dump-waste colony)                   ; The planet may absorb some new (but not accumulated) pollution.
  (incf (spend-housing (spending-vector-of colony)) ; Inherent population growth
        (* *inherent-population-growth* (population-of colony))))

(defun simulate-colony (colony)  
  ;;(format t "~&Wasted ~D of ~D production units.~%" (unallocated-production-of colony) (production-of colony))
  (setf (spend-research (spending-vector-of colony)) 0)
  ;; Apply budget to spending
  (map-into (spending-vector-of colony) #'+ (spending-vector-of colony) (colony-compute-budget colony))

;  (printl :prefs (spending-prefs-of colony))
;  (printl :budget (colony-compute-budget colony))
;  (printl :spending (spending-vector-of colony))

  ;; Tenure pollution
  (incf (pollution-of (planet-of colony)) (new-pollution-of colony))
  (setf (new-pollution-of colony) 0)
  
  ;; Cleanup pollution
  (setf (pollution-of (planet-of colony))
        (max 0 (- (pollution-of (planet-of colony)) (truncate (spend-cleanup (spending-vector-of colony)) *waste-cleanup-cost*))))
  (setf (spend-cleanup (spending-vector-of colony)) 0)

  ;; Construction proceeds based on spending determined by the player during the previous turn
;  (printl :final-spending (spending-vector-of colony))
  (simulate-factory-construction colony)
  (simulate-population-growth colony)   ; Grow population according to housing production
  (simulate-base-construction colony)
  (simulate-ship-construction colony)

  ;; Compute production and pollution for next turn.
  (colony-turn-prep colony)

  colony)

(defun tally-tech-spending (player)
  (loop for colony across (colonies player)
        summing (spend-research (spending-vector-of colony))))

;;; Debug shit

(defun print-col-status (colony)
  (format t "Colony ~A: ~D pop, ~D factories, ~D pollution. Max pop=~D  Spending=~D~%"
          (name-of colony) (population-of colony) (factories-of colony) (pollution-of (planet-of colony)) (compute-max-population colony) (spending-vector-of colony)))

(defun foostep (colony)
  (simulate-colony colony)  
  (print-col-status colony))




;;;; Fleets, stacks, ships, etc.

(defun fleet-num-ships (fleet) (reduce #'+ (stacks-of fleet) :key #'stack-count))

(defun minz (x y) (if (and x y) (min x y) (or x y)))

(defun fleet-range-ly (fleet &optional counts)
  (+ (range-of (owner-of fleet))
     (or 
      (reduce #'minz (stacks-of fleet)
              :key (lambda (stack)
                     (unless (and counts (eql 0 (gethash stack counts)))
                       (range-bonus-of (stack-design stack))))
              :initial-value nil)
      0)))

(defun fleet-range-units (fleet &optional counts)
  (* (fleet-range-ly fleet counts) units/light-years))

(defun fleet-state (fleet)
  (if (destination-of fleet)
      (if (star-of fleet) :departing :enroute)
      :orbiting))

(defun find-free-orbitals (star)
  (sort (copy-list (set-difference '(0 1 2 3 4 5) (mapcar #'orbital-of (fleets-orbiting star)))) #'<))

(defun find-free-orbital (star) (first (find-free-orbitals star)))

(defun orbital-vector (orbital)
  (let ((angle (+ (atan -11 35) (* orbital -0.5674163524304525d0)))
        (length 36.7f0))
    (vec (single (* length (cos angle))) (single (* length (sin angle))) 0.0f0)))

(defun orbital-loc (star orbital)
  (v+ (loc star) (orbital-vector orbital)))

(defun split-fleet (fleet count-map &key loc star destination)
  (let ((new-fleet (make-instance 'fleet 
                                  :owner (owner-of fleet)
                                  :destination destination
                                  :star star
                                  :loc loc :vloc loc
                                  :universe (universe-of fleet))))
    (loop for stack in (stacks-of fleet)
          as n = (gethash stack count-map (stack-count stack)) do
          (decf (stack-count stack) n)
          (incf (stack-count (ensure-stack (stack-design stack) new-fleet)) n))
    (update-fleet fleet)
    (update-fleet new-fleet)
    new-fleet))

(defun ensure-fleet (player starable)
  (let ((star (star-of starable)))
    (or (find player (fleets-orbiting star) :key #'owner-of)
        (let ((fleet (make-instance 'fleet
                                    :owner player
                                    :star star
                                    :loc (loc star)
                                    :vloc (loc star) ; XXX ??? Hmm.
                                    :universe (nonnull (universe-of star))
                                    :orbital (find-free-orbital star))))
          ;;(setf (loc fleet) (orbital-loc star (orbital-of fleet)))
          (push fleet (fleets-orbiting star))
          fleet))))

(defun ensure-stack (design fleet)
  (or (find design (stacks-of fleet) :key #'stack-design)
      (let ((stack (make-stack :design design :count 0 :fleet fleet)))
        (push stack (stacks-of fleet))
        stack)))

(defun update-fleet (fleet)
  (setf (stacks-of fleet) (delete-if #'zerop (stacks-of fleet) :key #'stack-count))
  (when (null (stacks-of fleet)) (setf (alive? fleet) nil))
  (cond
    ((and (not (alive? fleet)) (star-of fleet))           ; Orbiting/Departing fleet is empty, remove from star.
     (deletef (fleets-in-transit (universe-of fleet)) fleet)  ; For departing fleets
     (deletef (fleets-orbiting (star-of fleet)) fleet))
    ((not (alive? fleet))           ; Fleet in transit empty (should only occur if ship type scrapped)
     (break "FIXME, fleet evaporated in transit"))
    (t ; Fleet still exists, update properties
     (setf (speed-of fleet) (reduce #'min (stacks-of fleet)
                                    :key (lambda (stack) (speed-of (stack-design stack)))))))
  (setf (stacks-of fleet)
        (sort (stacks-of fleet) #'<= :key (lambda (x) (design-slot-num (stack-design x))))))

(defun star-in-range-of-fleet (star fleet &optional counts)
  (<= (distance-from-player (owner-of fleet) star) (fleet-range-units fleet counts)))

(defun add-ships (player star design num)
  (let ((fleet (ensure-fleet player star)))
    (incf (stack-count (ensure-stack design fleet)) num)
    (update-fleet fleet)))

(defun build-ships (colony design num)
  (new-player-event (owner-of colony) 'sound-event :name :chime-low)
  (add-ships (owner-of colony) colony design num))

(defun fleet-successor (fleet)
  (or (and.. (successor-of fleet) (fleet-successor $)) fleet))

;;; ** IMPORTANT ** 

(defun merge-fleets (fleet-1 #|into|# fleet-2)  
  (prog1 fleet-2
    (dolist (stack (stacks-of fleet-1))
      (incf (stack-count (ensure-stack (stack-design stack) fleet-2)) (stack-count stack))
      (setf (stack-count stack) 0))
    (update-fleet fleet-2)))

;;; Identity of fleet objects isn't always preserved: fleets are
;;; merged at stars.  join-fleet-to-star returns the new fleet object
;;; which must be used in place of the one passed as an argument. When
;;; this occurs, the fleet's successor slot is set, and you should
;;; find the end of this chain using fleet-successor.

(defun join-fleet-to-star (fleet star)
  (assert (not (successor-of fleet)))
  (unless (find fleet (fleets-orbiting star))
    (dolist (stack (stacks-of fleet))
      (add-ships (owner-of fleet) star (stack-design stack) (stack-count stack)))
    (setf (loc fleet) (loc star)
          (vloc fleet) (loc star)       ; ??
          (successor-of fleet) (ensure-fleet (owner-of fleet) star)
          (destination-of fleet) nil)
    (deletef (fleets-in-transit (universe-of fleet)) fleet))
  ;; A given owner can only have one fleet..
  (let ((newfleet (ensure-fleet (owner-of fleet) star)))
    (prog1 newfleet 
      (update-fleet newfleet))))

;;; If fleets are merged e.g. a departing fleet is recalled, note that
;;; this function returns the new fleet object. Assume the original
;;; object is always destroyed. Second value indicates if the order
;;; was successful (nil if out of range).
(defun send-fleet (fleet star)
  (assert (not (successor-of fleet)))
  (cond
    ((eql star (star-of fleet)) (values (join-fleet-to-star fleet star) t))
    ((not (star-in-range-of-fleet star fleet)) (values fleet nil))
    (t (unless (destination-of fleet)
         (push fleet (fleets-in-transit *universe*)))
       (deletef (fleets-orbiting (star-of fleet)) fleet)
       (setf (destination-of fleet) star)
       ;; TODO: Play a little computer noise here.
       (values fleet t))))

(defun simulate-fleet (fleet)
  (when (destination-of fleet)
    (let* ((speed-units (* units/light-years (speed-of fleet)))
           (dest (destination-of fleet))
           (distance (vdist (loc fleet) (loc dest))))
      (printl :speed (speed-of fleet) :in-units speed-units :distance-remaining distance)
      (cond
        ((<= distance speed-units)
         (setf (initiative-of fleet) (- speed-units distance))
         (new-player-event (owner-of fleet) 'sound-event :name :chime-high)
         (join-fleet-to-star fleet dest))
        (t (setf (loc fleet) (v+ (loc fleet) (vscaleto (v- (loc dest) (loc fleet)) speed-units))
                 (star-of fleet) nil))))))

(defun fleet-compute-eta-to (fleet destination)
  (if (not destination)
      0                                 ; Feh.
      (ceiling (vdist (loc fleet) (loc destination))
               (* units/light-years (speed-of fleet)))))

(defun fleet-compute-eta (fleet) (fleet-compute-eta-to fleet (destination-of fleet)))

(defun fleet-find-colony-ship-for (fleet planet)
  (find-if (lambda (stack) (some (lambda (x) (can-colonize? x (planet-type-of planet))) (design-techs (stack-design stack))))
           (stacks-of fleet)))

(defun establish-colony (player stack)
  (decf (stack-count stack))
  (update-fleet (stack-fleet stack))
  (let* ((star   (star-of (stack-fleet stack)))
         (planet (planet-of star)))
    (assert (null (colony-of planet)))
    (setf (colony-of planet) (make-instance 'colony
                                            :owner player
                                            :planet planet
                                            :population 1
                                            :factories 1))
    (prog1 (colony-of planet)
      (colony-turn-prep (colony-of star))
      (update-player-planets (universe-of star)))))

;;;; Technology

(defun update-player-attributes (player)
  (let ((globals (remove-if-not (lambda (tech) (typep tech 'global-tech)) (technologies-of player))))
    (setf (range-of player) (reduce #'max globals :key #'range-bonus :initial-value 4))))

(defun grant-tech (player tech)
  (pushnew tech (technologies-of player))
  (deletef (potential-techs-of player) tech)
  (deletef (available-techs-of player) tech)
  ;; Remove from ongoing research projects
  (loop for index upfrom 0 
        for project across (research-projects-of player)
        when (and project (eql tech (researching-tech project)))
        do (setf (aref (research-projects-of player) index) nil))
  ;; Add linked techs
  (loop for linked being the hash-values of *name->tech*
        when (eql tech (linked-to linked)) 
        do (grant-tech player linked))
  ;; Recompute cached attributes
  (update-player-attributes player))

(defun tech-level-cost (level) (* 100 (round (expt level 1.6))))

(defun tech-research-cost (tech) (tech-level-cost (level-of tech)))

(defun tech-base-unit-cost (tech)
  (ceiling
   (* (cost-modifier-of tech)
        (expt 2 (/ (level-of tech) 20 #| Magic fudge factor |# )))))

(defun tech-cost-for-design (design tech)
  ;; The magic fudge factor 'foo' is the number of tech levels necessary to double the per-unit cost.
  (ceiling
   (* 5
      (if (cost-scales-with-size? tech)
          ;; Magic scaling here makes medium ships the reference for scaled equipment cost.
          (* (/ 600.0) (space-of (design-type design)))
          1.0)
      (tech-base-unit-cost tech))))

(defun progress-research-project (player project investment)
  (printl :research-progress (researching-tech project)
          :spent (researching-spent project)
          :investing investment)
  (incf (researching-spent project) investment)
  (if (>= (researching-spent project) (tech-research-cost (researching-tech project)))
      (prog1 t
        (grant-tech player (researching-tech project))
        (new-player-event player 'new-tech-event :tech (researching-tech project)))
      nil))

(defun begin-research-project (player tech)
  (let ((pos (position nil (research-projects-of player))))
    (assert (not (null pos)))
    (setf (aref (research-projects-of player) pos)
          (make-researching :tech tech :spent 0))
    (deletef (available-techs-of player) tech)))

(defun player-needs-new-research (player)
  (some #'null (research-projects-of player)))

(defun range-to-string (range)
  (cond
    ((< range 1) "Short")
    ((< range 1.5) "Medium")
    ((< range 2.0) "Extended")
    ((<= range 3.5) "Long")
    (t "Very Long")))

(defmethod designer-description-of ((a weapon))
  (with-slots (damage range shield-scaling projectile-speed weapon-targetting-bonus) a
    (with-output-to-string (out)
      (let ((sep nil))
        (flet ((sep () 
                 (when sep (write-string ", " out))
                 (setf sep t)))
          (typecase damage
            (range (sep) (format out "Damage: ~:D-~:D" (range-min damage) (range-max damage))))
          (unless (= range 1.0)
            (sep)
            (format out "~A Range" (range-to-string range)))
          (unless (= shield-scaling 1.0)
            (sep)
            (format out "~D% shield bypass" (round (* 100 (- 1.0 shield-scaling)))))
          (unless (= projectile-speed 2.0)
            (sep)
            (format out "Speed ~Fx" projectile-speed))
          (unless (= weapon-targetting-bonus 0)
            (sep)
            (format out "+~D targetting" weapon-targetting-bonus)))))))

;;;; Designs

(defun design-techs (design) (remove nil (design-tech-slots design)))

(defun design-techs-and-counts (design)
  (loop for tech across (design-tech-slots design)
        for count across (deisgn-tech-counts design)
        when tech collect (cons tech count)))

(defmacro latest-model-number (player type)
  `(getf (model-history-of ,player)
         (intern (name-of ,type) :keyword) 0))

(defun ensure-numbered-slots (type)
  (loop for index upfrom 0 
        for slot in (slots-of type)
        do (setf (index-of slot) index)))

(defun make-design (name type &rest args)
  (let ((design 
         (apply #'make-instance 'design 
                :name name :type type
                :techs (map 'vector (constantly nil) (slots-of type))
                :tech-counts (map 'vector (constantly nil) (slots-of type))
                args)))
    (prog1 design      
      (analyze-design design)
      (assert (owner-of design))
      (setf (model-number-of design) (incf (latest-model-number (owner-of design) type))))))

(defun index-for-new-design (player)
  (position nil (ship-designs-of player)))

(defun define-new-design (player design)
  (let ((index (index-for-new-design player)))
    (unless index (error "No slots left for design (~A)" player))
    (setf (aref (ship-designs-of player) index) design
          (design-slot-num design) index)))

(defun get-nth-design (player n) (aref (ship-designs-of player) n))

(defun space-remaining (design)
  (- (space-of (design-type design))
     (space-used-of design)))

;;; FIXME/TODO: This gives us base size, but size should miniaturize with advancing tech level too.
(defgeneric compute-tech-size (design tech)
  (:method (design tech)
    (max
     (minimum-size-of tech)
     (round
      (* (1+ (* (size-scaling-of tech)
                (+ -1 (space-multiplier-of (design-type design)))))
         (size-modifier-of tech)
         (base-size-of tech))))))

(defgeneric compute-tech-weight (design tech)
  (:method (design tech)    
    (* 0.3 (compute-tech-size design tech))))

;;; Compute derived attributes (cost, speed, etc.) from a design, and set those slots.
;;; May be called multiple times when the designer UI is running.
;;; TODO: Compute cost (don't specify in make-design, that's dumb)

;;

(defun analyze-design (design)
  (let ((type (design-type design)))
    (format t "~&---------- Analyzing design ~W ----------~%" design)
    (setf (weight-of design) (round
                              (+ (weight-of type)
                                 (loop for tech across (design-tech-slots design)
                                       for count across (design-tech-counts design)                                    
                                       summing (if (not tech)
                                                   0
                                                   (* count 
                                                      (compute-tech-weight design tech))))))
          (space-used-of design) (round
                                  (loop for tech across (design-tech-slots design)
                                        for count across (design-tech-counts design)
                                        when tech
                                        do (format t "~&   ~:D x ~A = ~:D~%" count (name-of tech) (* count (compute-tech-size design tech)))
                                        summing (if (not tech)
                                                    0
                                                    (* count
                                                       (compute-tech-size design tech)))))
          (cost-of design) (+ (cost-of type)
                              (loop for tech across (design-tech-slots design)
                                    for count across (design-tech-counts design)
                                    summing (if (not tech) 0 (* count (tech-cost-for-design design tech)))))
          (thrust-of design) (+ (maneuverability-of type)
                                (reduce #'+ (design-techs design) :key #'thrust-bonus))
          (speed-of design) (engine-speed (engine-of design))
          (range-bonus-of design) (reduce #'+ (design-techs design) :key #'range-bonus))
    (format t "~&~%Analysis result:~%")
    (describe design)))

(defun hardpoint-applicable-techs (hardpoint player)
  (let ((tech-class (slot-value hardpoint 'tech-class)))
    (remove-if-not (lambda (tech) (typep tech tech-class)) (technologies-of player))))

;;;; Turn cycle

(defun next-turn (universe)  
  (loop for star across (stars universe) do (and.. (planet-of star) (colony-of $) (simulate-colony $)))
  (loop for fleet in (fleets-in-transit universe) do (simulate-fleet fleet))
  (loop for star across (stars universe) do (dolist (fleet (fleets-orbiting star)) (explore-star star (owner-of fleet))))

  ;; Note that we can't do research until we've computed final planet budgets.
  (dolist (player (all-players universe))
    (loop for project across (research-projects-of player)
          as investment = (tally-tech-spending player)
          when project do (progress-research-project player project investment)))

  ;; One last thing:
  (incf (year-of universe) 2))

;;;; Events

(defun enqueue-player-event (player new-event)
  (setf (event-list-of player)
        (delete-if (lambda (old-event) (event-supercedes? old-event new-event))
                   (event-list-of player)))
  (push new-event (event-list-of player)))

(defun new-player-event (player event-type &rest args)
  (enqueue-player-event player (apply #'make-instance event-type :owner player args)))


