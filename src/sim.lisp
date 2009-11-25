(in-package :g1)

;;;; Printers

(defmethod print-object ((planet planet) stream)
  (print-unreadable-object (planet stream :identity nil :type t)
    (apply 'format stream "~A: ~A ~D/~D/~D/~D" 
           (name-of planet)
           (planet-type-of planet)
           (coerce (terrains-of planet) 'list))))

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

;;; How should spending allocation be presented to the user? How
;;; should the settings be represented?

;;; MOO-style sliders allocate percentages of production, but there's
;;; some trickery behind the scenes where the game might adjust the
;;; sliders automatically. Ultimately, the inputs should be amounts in
;;; credits (or other monetary unit).

;;; Concrete spending allocation:
(defstruct spend
  cleanup                               ; Waste cleanup
  housing                               ; Population growth bonus
  terraform                             ; Terraforming
  factories                             ; Factory construction
  bases                                 ; Missile base construction
  shield                                ; Planetary shield construction
  ships                                 ; Ship construction 
  research                              ; Research spending
)

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
  (max 0 (- (reduce #'+ (map 'vector #'*
                             (adaptation-vector-of (owner-of colony))
                             (terrains-of (planet-of colony))))
            (pollution-of (planet-of colony)))))

(defun compute-population-distribution (colony)
  (let* ((terrains (terrains-of (planet-of colony)))
         (max-population (compute-max-population colony))
         (maxpops (map 'vector #'* (adaptation-vector-of (owner-of colony)) terrains))
         (realpop (population-of colony))
         (pop (min realpop max-population))
         (displaced (- realpop pop))
         (pops (make-array 4 :initial-element 0)))
    (print (list :maxpops maxpops))
    ;; TODO: Will need revising for non-tellurian races
    (loop for tidx from 0 below 4
          as pophere = (min pop (aref maxpops tidx)) do
          (decf pop pophere)
          (incf (aref pops tidx) pophere))
    (values pops (+ displaced pop))))

(defun simulate-colony (colony)
  (simulate-production colony)          ; Generate production points and new pollution.
  (dump-waste colony)                   ; The planet may absorb some new (but not accumulated) pollution.

  ;; *** Player should run here to set spending - split this process in two!


  ;; Finalize
  (incf (pollution-of (planet-of colony)) (new-pollution-of colony))
  (setf (new-pollution-of colony) 0)
)

;(defun apply-spending (planet spending)
;  (