(in-package :g1)

;;;; Printers

(defmethod print-object ((planet planet) stream)
  (print-unreadable-object (planet stream :identity nil :type t)
    (apply 'format stream "~A: ~A ~D/~D/~D/~D" 
           (name-of planet)
           (planet-type-of planet)
           (coerce (terrains-of planet) 'list))))

(defmethod print-object ((colony colony) stream)
  (print-unreadable-object (colony stream :identity nil :type t)
    (format stream "~A: pop ~D/~A ind ~D"
           (name-of colony)
           (population-of colony)
           (ignore-errors (compute-max-population colony))
           (factories-of colony))))

(defmethod print-object ((star star) stream)
  (print-unreadable-object (star stream :identity nil :type t)
    (format stream "~W w/ ~A planet" 
            (name-of star)
            (or (and (planet-of star) (planet-type-of (planet-of star))) "no"))
    (and.. (owner-of star)
           (format stream " owned by ~W" (name-of $)))))

(defmethod print-object ((race race) stream)
  (print-unreadable-object (race stream :identity nil :type t)
    (format stream "~A" (name-of race))))

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :identity nil :type t)
    (format stream "~A, a ~A" (name-of player) (name-of (race-of player)))))

;;;; Duct tape

(defmethod owner-of ((star star))
  (and.. (planet-of star) (owner-of $)))

(defmethod owner-of ((planet planet))
  (and.. (colony-of planet) (owner-of $)))

(defmethod name-of ((this colony)) (name-of (planet-of this)))

;;;; Hello again.

(defparameter *race-human*
  (make-instance 'race :name "Human"))

(defun update-player-planets (universe)
  (loop for player in (all-players universe) do (setf (fill-pointer (colonies player)) 0))
  (loop for star across (stars universe)
        as planet = (planet-of star)
        as owner = (owner-of star)
        when owner do (vector-push-extend star (colonies owner)))
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

;;;; Planetary economy

;;; Industrial output is constrained by requiring sufficient population to operate the factories.
(defun active-factories (colony)
  (min (* (automation-level-of (owner-of colony)) (population-of colony))
       (factories-of colony)))

(defun compute-production (colony)
  (values
   ;; Production
   (* (production-modifier-of (race-of (owner-of colony)))
      (production-modifier-of (planet-of colony))
      (+ (population-of colony) (active-factories colony)))
   ;; Pollution - each active factory produces a base of one quarter
   ;; unit of pollution per turn, modified by player technology,
   ;; rounded down.
   (floor (* (pollution-modifier-of (owner-of colony))
             (active-factories colony))
          4)))

(defun simulate-production (colony)
  (multiple-value-bind (new-prod new-pol) (compute-production colony)
    (incf (production-of colony) new-prod)
    (incf (new-pollution-of colony) new-pol)))

(defun dump-waste (colony)
  ;; Ocean and magma terrain have limited ability to absorb pollution as it is created.
  (setf (new-pollution-of colony)
        (max 0 (- (new-pollution-of colony)
                  (* 3.00 (magma# (planet-of colony)))
                  (* 0.25 (ocean# (planet-of colony)))))))

(defun compute-max-population (colony)
  ;; Max population less pollution penalty (-1 pop unit per unit pollution)
  (round
   (max 0 (- (reduce #'+ (maxpops-by-terrain colony))
             (pollution-of (planet-of colony))))))

(defun maxpops-by-terrain (colony)
  (map 'vector
       (lambda (x y) (round (* x y (habitability-of (planet-of colony)))))
       (adaptation-vector-of (owner-of colony))
       (terrains-of (planet-of colony))))

(defun compute-population-distribution (colony population)
  (let* ((max-population (compute-max-population colony))
         (maxpops (maxpops-by-terrain colony))
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
          with starting-production = production
          for (tidx cost terrain-type) in scosts
          as vacant-here = (aref vacant tidx)
          when cost do
          (loop while (and (>= production cost) (> vacant-here 0)) do
                (incf total-growth)
                (decf production cost)
                (decf vacant-here)
                #+NIL (printl :grew terrain-type cost production))
          finally 
          (return (values total-growth (- starting-production production))))
    ))

(defparameter *overpop-decay-rate* 0.10)

(defun simulate-population-growth (colony)
  (multiple-value-bind (growth spent)
      (calculate-pop-growth colony (spend-housing (spending-vector-of colony)))
    (incf (population-of colony) growth)
    (decf (spend-housing (spending-vector-of colony)) spent))
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

(defun simulate-colony (colony)

  (incf (pollution-of (planet-of colony)) (new-pollution-of colony))
  (setf (new-pollution-of colony) 0)

  (simulate-production colony)          ; Generate production points and new pollution.
  (dump-waste colony)                   ; The planet may absorb some new (but not accumulated) pollution.

  (incf (spend-housing (spending-vector-of colony)) ; Inherent population growth
        (* *inherent-population-growth* (population-of colony)))

  ;; *** Player should run here to set spending, so rotate these steps..

  (simulate-population-growth colony)
)

;;; Debug shit

(defun print-col-status (colony)
  (format t "Colony ~A: ~D pop, ~D factories, ~D pollution. Max pop=~D~%"
          (name-of colony) (population-of colony) (factories-of colony) (pollution-of (planet-of colony)) (compute-max-population colony)))

(defun foostep (colony)
  (simulate-colony colony)
  (print-col-status colony))

