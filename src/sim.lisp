(in-package :g1)

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
  (round (reduce #'min (colonies player) :key (lambda (col) (len (v- (loc col) (loc staroid)))))
         light-years/units))

;;;;

(defun get-player-styles ()
  (list (make-pstyle :label-color (vector 255  90  90) :fill-color (vector 180 0 0))
        (make-pstyle :label-color (vector 255 134   0) :fill-color (vector 167 106 0))
        (make-pstyle :label-color (vector 255 255  90) :fill-color (vector 131 127 55))
        (make-pstyle :label-color (vector 170 224 108) :fill-color (vector 86 140 22))
        (make-pstyle :label-color (vector  40 160 255) :fill-color (vector 0 64 128))
        (make-pstyle :label-color (vector 190 116 255) :fill-color (vector 64 64 128))))

;;;; Planetary economy

;;; Industrial output is constrained by requiring sufficient population to operate the factories.
(defun max-factories (colony)
  (* (automation-level-of (owner-of colony)) (population-of colony)))

(defun active-factories (colony)
  (min (max-factories colony) (factories-of colony)))

(defun compute-production (colony)
  (values
   ;; Production
   (* (production-modifier-of (race-of (owner-of colony)))
      (production-modifier-of (planet-of colony))
      (+ (population-of colony) (active-factories colony)))
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
    (setf (new-pollution-of colony) new-pol)) ; Setf, not incf, so colony-turn-prep is idempotent
  (setf (unallocated-production-of colony) (production-of colony)))

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

(defun allocate-funds (colony amount &optional (reason "??"))
  (let ((real-amount (min (ceiling amount) (unallocated-production-of colony))))
    ;;(format t "~&Allocated ~A BC for ~A~%" real-amount reason)
    (decf (unallocated-production-of colony) real-amount)
    real-amount))

(defun auto-allot-cleanup (colony)
  ;;(printl :need-to-cleanup (+ (pollution-of (planet-of colony)) (new-pollution-of colony)))
  (incf (spend-cleanup (spending-vector-of colony))
        (allocate-funds colony (* *waste-cleanup-cost* (+ (pollution-of (planet-of colony)) (new-pollution-of colony))) "ecology")))

(defun auto-allot-development (colony)
  ;; Really we want to take some percentage of the unallocated funds, chosen by the player.
  (let ((funds (unallocated-production-of colony))
        (needed-factories (max 0 (- (max-factories colony) (factories-of colony))))
        (needed-population (max 0 (- (compute-max-population colony) (population-of colony)))))
    (unless (zerop needed-factories)
      (let ((spent (allocate-funds colony (* needed-factories *factory-base-cost*) "factories")))
        (incf (spend-factories (spending-vector-of colony)) spent)
        (decf funds spent)))
    (unless (zerop needed-population)
      (multiple-value-bind (growth remaining) (calculate-pop-growth colony funds)
        (incf (spend-housing (spending-vector-of colony))
              (allocate-funds colony (if (= growth needed-population)
                                         (- funds remaining) 
                                         funds)
                              "housing"))))))

(defun colony-turn-prep (colony)
  ;; Now, prepare for the coming turn.
  (setf (production-of colony) 0)
  (simulate-production colony)          ; Generate production points and new pollution.  
  (dump-waste colony)                   ; The planet may absorb some new (but not accumulated) pollution.
  (incf (spend-housing (spending-vector-of colony)) ; Inherent population growth
        (* *inherent-population-growth* (population-of colony))))  

(defun simulate-colony (colony)  
  ;;(format t "~&Wasted ~D of ~D production units.~%" (unallocated-production-of colony) (production-of colony))

  ;; Tenure pollution
  (incf (pollution-of (planet-of colony)) (new-pollution-of colony))
  (setf (new-pollution-of colony) 0)
  
  ;; Cleanup pollution
  (setf (pollution-of (planet-of colony))
        (max 0 (- (pollution-of (planet-of colony)) (truncate (spend-cleanup (spending-vector-of colony)) *waste-cleanup-cost*))))
  (setf (spend-cleanup (spending-vector-of colony)) 0)

  ;; Construction proceeds based on spending determined by the player during the previous turn
  (simulate-factory-construction colony)
  (simulate-population-growth colony)   ; Grow population according to housing production

  ;; Compute production and pollution for next turn.
  (colony-turn-prep colony)

  ;; Shit that shouldn't be here.

  (auto-allot-cleanup colony)
  (auto-allot-development colony)
)

;;; Debug shit

(defun print-col-status (colony)
  (format t "Colony ~A: ~D pop, ~D factories, ~D pollution. Max pop=~D  Spending=~D~%"
          (name-of colony) (population-of colony) (factories-of colony) (pollution-of (planet-of colony)) (compute-max-population colony) (spending-vector-of colony)))

(defun foostep (colony)
  (simulate-colony colony)  
  (print-col-status colony))




;;;; Fleets, stacks, ships, etc.

(defun find-free-orbitals (star)
  (sort (copy-list (set-difference '(0 1 2 3 4 5) (mapcar #'orbital-of (fleets-orbiting star)))) #'<))

(defun find-free-orbital (star) (first (find-free-orbitals star)))

(defun orbital-vector (orbital)
  (let ((angle (+ (atan -11 35) (* orbital -0.5674163524304525d0)))
        (length 36.7f0))
    (vec (single (* length (cos angle))) (single (* length (sin angle))) 0.0f0)))

(defun orbital-loc (star orbital)
  (v+ (loc star) (orbital-vector orbital)))

(defun ensure-fleet (player starable)
  (let ((star (star-of starable)))
    (or (find player (fleets-orbiting star) :key #'owner-of)
        (let ((fleet (make-instance 'fleet
                                    :owner player
                                    :universe (universe-of star)
                                    :orbital (find-free-orbital star))))
          (setf (loc fleet) (orbital-loc star (orbital-of fleet)))
          (push fleet (fleets-orbiting star))
          fleet))))

(defun ensure-stack (design fleet)
  (or (find design (stacks-of fleet) :key #'stack-design)
      (let ((stack (make-stack :design design :count 0 :fleet fleet)))
        (push stack (stacks-of fleet))
        stack)))

(defun update-fleet (fleet)
  (setf (stacks-of fleet) (delete-if #'zerop (stacks-of fleet) :key #'stack-count))
  (cond
    ((and (null (stacks-of fleet)) (star-of fleet))           ; Orbiting fleet is empty, remove from star.
     (deletef (fleets-orbiting (star-of fleet)) fleet))
    ((null (stacks-of fleet))           ; Fleet in transit empty (should only occur if ship type scrapped)
     (break "FIXME, fleet evaporated in transit")
     #+NIL (deletef fleet foo))
    (t ; Fleet still exists, update properties
     (setf (speed-of fleet) (reduce #'min (stacks-of fleet)
                                    :key (lambda (stack) (speed-of (stack-design stack))))))))

(defun build-ship (colony design)
  (incf (stack-count (ensure-stack design (ensure-fleet (owner-of colony) colony)))))  

