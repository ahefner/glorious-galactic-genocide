(in-package :g1)

;;;; Planet types are just symbols.

(defvar *planet-types* '(terran oceanic jungle arid desert tundra minimal barren volcanic dead inferno toxic radiated))

(defparameter *planet-type-habitabilities*
  '((terran   . 1.0)
    (oceanic  . 1.0)
    (jungle   . 0.8)
    (arid     . 0.4)
    (tundra   . 0.5)
    (desert   . 0.27)
    (minimal  . 0.2)
    (barren   . 0.17)
    (volcanic . 0.12)
    (dead     . 0.12)
    (inferno  . 0.1)
    (toxic    . 0.1)
    (radiated . 0.1)))

(defparameter *planet-type-rich-probability*
  '((terran   . 0.01)
    (oceanic  . 0.02)
    (jungle   . 0.02)
    (arid     . 0.10)
    (tundra   . 0.15)
    (desert   . 0.15)
    (minimal  . 0.25)
    (barren   . 0.35)
    (volcanic . 0.55)
    (dead     . 0.35)
    (inferno  . 0.60)
    (toxic    . 0.70)
    (radiated . 0.85)))

(defparameter *planet-type-urich-probability*
  '((terran   . 0.00)
    (oceanic  . 0.00)
    (jungle   . 0.00)
    (arid     . 0.02)
    (tundra   . 0.05)
    (desert   . 0.05)
    (minimal  . 0.10)
    (barren   . 0.10)
    (volcanic . 0.10)
    (dead     . 0.10)
    (inferno  . 0.20)
    (toxic    . 0.20)
    (radiated . 0.25)))

(defparameter *planet-type-poor-probability*
  '((terran   . 0.30)
    (oceanic  . 0.20)
    (jungle   . 0.30)
    (arid     . 0.20)
    (tundra   . 0.15)
    (desert   . 0.15)
    (minimal  . 0.10)
    (barren   . 0.05)
    (volcanic . 0.05)
    (dead     . 0.04)
    (inferno  . 0.00)
    (toxic    . 0.00)
    (radiated . 0.00)))

(defparameter *planet-type-upoor-probability*
  '((terran   . 0.10)
    (oceanic  . 0.05)
    (jungle   . 0.10)
    (arid     . 0.05)
    (tundra   . 0.05)
    (desert   . 0.05)
    (minimal  . 0.04)
    (barren   . 0.04)
    (volcanic . 0.02)
    (dead     . 0.01)
    (inferno  . 0.00)
    (toxic    . 0.00)
    (radiated . 0.00)))

(defun choose-planet-attribute (type score)
  (cond
    ((< score (cdr (assoc type *planet-type-urich-probability*))) :ultra-rich)
    ((< score (cdr (assoc type *planet-type-rich-probability*))) :rich)
    ((< score (- 1.0 (cdr (assoc type *planet-type-poor-probability*)))) :abundant)
    ((< score (- 1.0 (cdr (assoc type *planet-type-upoor-probability*)))) :poor)
    (t :ultra-poor)))

(defgeneric choose-planet-terrains (planet-type))

(defmethod choose-planet-terrains ((planet-type (eql 'terran)))
  (declare (ignorable planet-type))
  (vector (+ 15 (random 10))
          (+ 38 (random 30))
          (+  2 (random 15))
          (+  1 (random  6))))

(defmethod choose-planet-terrains ((planet-type (eql 'oceanic)))
  (declare (ignorable planet-type))
  (vector (+  0 (random  13))
          (+ 45 (random 45))
          (+  0 (random 20))
          (+  0 (max 0 (+ -4 (random 12))))))

(defmethod choose-planet-terrains ((planet-type (eql 'jungle)))
  (declare (ignorable planet-type))
  (vector (+ 15 (random 12))
          (+ 35 (random 15))
          (+  2 (random 12))
          (+  2 (random 12))))

(defmethod choose-planet-terrains ((planet-type (eql 'arid)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 30))
          (+  1 (random 12))
          (+  0 (max 0 (+ -5 (random 10))))
          (+  2 (random 12))))

(defmethod choose-planet-terrains ((planet-type (eql 'desert)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 38))
          (+  0 (max 0 (+ -5 (random 8))))
          (+  0 (max 0 (+ -6 (random 8))))
          (+  2 (random 16))))

(defmethod choose-planet-terrains ((planet-type (eql 'tundra)))
  (declare (ignorable planet-type))
  (vector (+  1 (random 10))
          (+  2 (random 20))
          (+  15 (random 50))
          (+  0 (max 0 (+ -10 (random 12))))))

(defmethod choose-planet-terrains ((planet-type (eql 'minimal)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 30))
          (+  0 (max 0 (+ -5 (random 8))))
          (+  0 (max 0 (+ -6 (random 12))))
          (+  1 (max 0 (+ -4 (random 12))))))

(defmethod choose-planet-terrains ((planet-type (eql 'barren)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 36))
          0
          (+  1 (max 0 (+ -6 (random 9))))
          (+  2 (max 0 (+ -4 (random 13))))))

(defmethod choose-planet-terrains ((planet-type (eql 'volcanic)))
  (declare (ignorable planet-type))
  (vector (+  4 (random 15))
          (max 0 (+ -3 (random 6)))
          (+  0 (max 0 (+ -7 (random 9))))
          (+ 10 (random 35))))

(defmethod choose-planet-terrains ((planet-type (eql 'dead)))
  (declare (ignorable planet-type))
  (vector (+ 10 (random 78))
          0
          0
          0 #+NIL (+  0 (max 0 (+ -13 (random 16))))))

(defmethod choose-planet-terrains ((planet-type (eql 'inferno)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 36))
          0
          0
          (+  0 (max 0 (+ -4 (random 22))))))

(defmethod choose-planet-terrains ((planet-type (eql 'toxic)))
  (declare (ignorable planet-type))
  (vector (+ 19 (random 45))
          (max 0 (+ -7 (random 13)))
          0
          (+  0 (max 0 (+ -7 (random 15))))))

(defmethod choose-planet-terrains ((planet-type (eql 'radiated)))
  (declare (ignorable planet-type))
  (vector (+ 13 (random 46))
          0
          0
          (+  0 (max 0 (+ -7 (random 13))))))

;;;; Spectral classes

(defgeneric choose-random-planet-type (spectral-class))

(defmethod choose-random-planet-type ((spectral-class (eql 'O)))
  (declare (ignorable spectral-class))
  (random-choice 
   (02 'desert)
   (04 'minimal)
   (05 'barren)
   (04 'volcanic)
   (07 'dead)
   (05 'inferno)
   (05 'toxic)
   (08 'radiated)
   (08 nil)))

(defmethod choose-random-planet-type ((spectral-class (eql 'B)))
  (declare (ignorable spectral-class))
  (random-choice
   (01 'terran)
   (01 'oceanic)
   (01 'jungle)
   (01 'arid)
   (03 'desert)
   (01 'tundra)
   (04 'minimal)
   (05 'barren)
   (07 'volcanic)
   (06 'dead)
   (07 'inferno)
   (06 'toxic)
   (05 'radiated)
   (06 nil)))

(defmethod choose-random-planet-type ((spectral-class (eql 'A)))
  (declare (ignorable spectral-class))
  (random-choice
   (01 'terran)
   (01 'oceanic)
   (01 'jungle)
   (02 'arid)
   (03 'desert)
   (01 'tundra)
   (04 'minimal)
   (04 'barren)
   (06 'volcanic)
   (06 'dead)
   (07 'inferno)
   (06 'toxic)
   (05 'radiated)
   (02 nil)))

(defmethod choose-random-planet-type ((spectral-class (eql 'F)))
  (declare (ignorable spectral-class))
  (random-choice 
   (02 'terran)
   (02 'oceanic)
   (02 'jungle)
   (02 'arid)
   (03 'desert)
   (03 'tundra)
   (04 'minimal)
   (04 'barren)
   (04 'volcanic)
   (04 'dead)
   (03 'inferno)
   (03 'toxic)
   (03 'radiated)))

(defmethod choose-random-planet-type ((spectral-class (eql 'G)))
  (declare (ignorable spectral-class))
  (random-choice
   (06 'terran)
   (05 'oceanic)
   (05 'jungle)
   (03 'arid)
   (03 'desert)
   (03 'tundra)
   (03 'minimal)
   (03 'barren)
   (03 'volcanic)
   (01 'dead)
   (02 'inferno)
   (02 'toxic)
   (01 'radiated)))

(defmethod choose-random-planet-type ((spectral-class (eql 'K)))
  (declare (ignorable spectral-class))
  (random-choice
   (03 'terran)
   (03 'oceanic)
   (03 'jungle)
   (04 'arid)
   (04 'desert)
   (02 'tundra)
   (04 'minimal)
   (03 'barren)
   (03 'volcanic)
   (03 'dead)
   (03 'inferno)
   (02 'toxic)
   (03 'radiated)
   (03 nil)))

(defmethod choose-random-planet-type ((spectral-class (eql 'M)))
  (declare (ignorable spectral-class))
  (random-choice
   (01 'terran)
   (01 'oceanic)
   (03 'arid)
   (04 'desert)
   (02 'tundra)
   (04 'minimal)
   (04 'barren)
   (05 'volcanic)
   (03 'dead)
   (02 'inferno)
   (01 'toxic)
   (03 'radiated)
   (08 nil)))
  

;;;; Map generator

(defun choose-random-spectral-class ()
  (random-choice 
   (06 'O)                                   ; Blue
   (06 'B)                                   ; Blue/white
   (07 'A)                                   ; White
   (07 'F)                                   ; White (yellowish) (green, to make things interesting)
   (12 'G)                                   ; Yellow
   (12 'K)                                   ; Orange
   (25 'M)))                                 ; Red

(defparameter *star-min-distance* 130
  "Minimum distance in the X/Y plane separating stars on the map")

(defun inset-random (range border)
  (+ border (random (- range border border))))

(defun random-star (&key x y z)
  (let ((border 80)
        (size 2000))
    (make-instance 'star
                   :universe (nonnull *universe*)
                   :loc (vec (or x (inset-random size border))
                             (or y (inset-random size border))
                             (or z (random *starfield-depth*)))
                   :spectral-class (choose-random-spectral-class))))

(defun normsq-in-xy-plane (a)
  (with-vector (a)
    (+ (square a.x) (square a.y))))

(defun star-within-safe-radius (star stars radius)
  (loop named bill 
        with sc = (loc star)
        with r^2 = (square radius)
        for other across stars
        as oc = (loc other)
        ;; Because the presentation is basically 2D overhead, a 3D
        ;; distance check doesn't suffice. You can get two stars on
        ;; top of each other, impossible to select at the center of
        ;; the screen.
        when (< (normsq-in-xy-plane (v- oc sc)) r^2) do (return-from bill nil)
        finally (return-from bill t)))

(defun choose-name-keyed (universe keyfn generator)
  (with-slots (namelist) universe
    (let ((nameset (find-if keyfn namelist)))
      (cond
        (nameset 
         (setf namelist (delete nameset namelist))
         (funcall keyfn nameset))
        (t (funcall generator))))))

(defun choose-new-star-name (universe)
  (choose-name-keyed universe #'first
                     (lambda ()
                       (format nil "~A ~D"
                               (random-elt '("Wolf" "XJ" "HD" "XM" "XC"))
                               (random 100000)))))

(defun choose-new-constellation-name (universe)
  (choose-name-keyed universe #'second
    (lambda () (error "Ran out of constellations during map generation. This shouldn't happen."))))

(defparameter *constellation-prefixes*
  ;; Yes, I skip a few letters that I don't like.
  '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" 
    "Zeta" "Eta" "Theta" "Iota" "Kappa"
    "Lambda" "Mu" "Nu" "Omicron" 
    "Rho" "Sigma" "Tau" "Omega" ))

(defun create-star-safely (stars)
  (loop repeat 10000
        as star = (random-star)
        when (star-within-safe-radius star stars 120)
        do (return star)
        ;; FIXME, robustify:
        finally (error "Unable to position star during map generation.")))

(defun generate-constellation (universe num-stars)
  (let* ((name (choose-new-constellation-name universe))
         (prefixes *constellation-prefixes*)
         (root (create-star-safely (stars universe)))
         (constellation (list root)))
    ;;(format t "~&Creating constellation ~A~%" name)
    (setf (name-of root) (format nil "Alpha ~A" name))
    (pop prefixes)
    (vector-push-extend root (stars universe))
    (decf num-stars)
    (loop with tries = 0
          as base = (random-elt constellation)
          as dist = (+ *star-min-distance* (random 200) (random 200))
          as angle = (random (* 2 pi))
          as x = (+ (v.x (loc base)) (* dist (sin angle)))
          as y = (+ (v.y (loc base)) (* dist (cos angle)))
          as new-star = (random-star :x x :y y)
          until (zerop num-stars) do
          (incf tries)
          (cond
            ((>= tries 100)
             (format t "~&Giving up on constellation ~A~%" name)
             (return))
            ((star-within-safe-radius new-star (stars universe) *star-min-distance*)
             (vector-push-extend new-star (stars universe))
             (push new-star constellation)
             (setf (name-of new-star) (format nil "~A ~A" (pop prefixes) name))
             (decf num-stars))
            (t #+NIL (format t "~&(~A stars: Rejecting ~F,~F from ~A)~%" (length constellation) x y (name-of base)) )))
    #+NIL
    (dolist (star constellation)      
      (format t "  ~A at ~A~%" (name-of star) (loc star)))))

(defun generate-random-starmap (uni num-stars num-constellations)
  ;; Quadratic, but the universe isn't that big.
  (let ((stars (make-array num-stars :adjustable t :fill-pointer 0)))
    (setf (slot-value uni 'stars) stars)

    ;; TODO: Generate homeworlds here too.. 

    ;; Generate constellations:
    (loop repeat num-constellations
          as remaining = (- num-stars (length stars))
          with min-size = 3
          when (>= remaining min-size)
          do (generate-constellation uni (+ min-size (random (1+ (min (- (length *constellation-prefixes*) min-size)
                                                                      (- remaining min-size)))))))
    ;; Generate stray stars:
    (loop as star = (random-star)
          until (= num-stars (length stars))
          when (star-within-safe-radius star stars *star-min-distance*) do
          (vector-push-extend star stars)
          (setf (name-of star) (choose-new-star-name uni))
          #+NIL
          (format t "~&Created star ~A. Type ~A at ~A~%" 
                  (name-of star) (spectral-class star) (loc star)))

    (sort stars #'> :key (lambda (star) (v.z (loc star))))))

(defun generate-planets (universe)
  (with-slots (stars) universe
    (loop for star across stars do
          (unless (planet-of star)
            (let* ((planet-type (choose-random-planet-type (spectral-class star)))
                   (terrains (and planet-type (choose-planet-terrains planet-type)))
                   (attribute (and planet-type (choose-planet-attribute planet-type (random 1.0)))))
              (when planet-type
                (setf (slot-value star 'planet)
                      (make-instance 'planet
                                     :name (format nil "~A ~A" (name-of star) (random-elt '("I" "II" "II" "III" "III" "IV" "V")))
                                     :star star
                                     :planet-type planet-type
                                     :planet-attribute attribute
                                     :production-modifier (case attribute
                                                            (:abundant 1)
                                                            (:rich 2)
                                                            (:ultra-rich 3)
                                                            (:poor 1/2)
                                                            (:ultra-poor 1/3)
                                                            (t 1))
                                     :habitability (cdr (assoc planet-type *planet-type-habitabilities*))
                                     :terrains terrains))))))))

;;; TODO: Check that this star is in a reasonable position (other stars in range..)
(defun find-suitable-homeworld (stars)
  (loop as star = (random-elt stars) do
        (when (not (planet-of star)) (return star))))

(defun place-homeworld (uni player homeworld-name)
  (let* ((stars (stars uni))
         (race (race-of player))
         (star (find-suitable-homeworld stars)))         
    (explore-star star player)
    (setf (name-of star) homeworld-name)
    (setf (slot-value star 'planet)
          (make-instance 'planet
                         :name homeworld-name
                         :star star
                         :planet-type (homeworld-type-of race)
                         :terrains (homeworld-terrain-of race)))
    (setf (colony-of (slot-value star 'planet))
          (make-instance 'colony
                         :owner player
                         :planet (planet-of star)
                         :population (homeworld-population-of race)
                         :factories (homeworld-population-of race)))
    (colony-turn-prep (colony-of star))))

(defun assign-computer-colors (universe)
  (loop for player in (all-players universe)
        as next-color = (first (slot-value universe 'styles-list))        
        unless (style-of player) do 
        (setf (style-of player) (pop (slot-value universe 'styles-list)))
        (unless next-color
          (error "Ran out of player colors during map generation. How?"))))

(defun make-test-player (&optional (name "Goldfinch"))
  (let ((player (make-instance 'player :race *race-human* :name name)))
    (update-player-stats player)))

(defun choose-techs-for-race (race)
  (declare (ignore race))
  (stable-sort 
   (shuffle (loop for tech being the hash-values of *name->tech* 
                  when (and (not (linked-to tech)) 
                            (< (random 1.0) (probability-of tech)))
                  collect tech))
   #'< :key #'level-of))

(defun init-player-techs (player)
  (let* ((all-techs (choose-techs-for-race (race-of player)))
         (initial-techs (remove-if-not (lambda (tech) (zerop (level-of tech))) all-techs))
         (remaining-techs (remove 0 all-techs :key #'level-of)))
    (loop for tech in initial-techs do 
          (grant-tech player tech)
          ;; TEMPORARY HACK show initial techs after first turn!
          #+HACKHACK (setf (gethash tech (presented-technologies-of player)) t))
    (setf (potential-techs-of player) remaining-techs
          (available-techs-of player) (subseq remaining-techs
                                              0
                                              (min (initial-research-choices-of (race-of player))
                                                   (length remaining-techs))))
    ;; HACK TEMPORARY!!!
    (setf (available-techs-of player) remaining-techs)))

(defun add-initial-ships-and-designs (player)
  (let ((designs (ship-designs-of player))
        (homeworld (aref (colonies player) 0))
        (scout (make-design "Scout" (find-ship-type "Scout") 
                            :owner player
                            :cost 100   ; HACK FIXME
                            :engine (find-tech 'ion-drive)))
        (colony-ship (make-design "Colony Ship" (find-ship-type "Modular Cruiser") 
                                  :owner player
                                  :cost 2500  ; HACK FIXME
                                  :engine (find-tech 'ion-drive))))
    (define-new-design player scout)
    (define-new-design player colony-ship)

    (setf (aref (design-tech-slots scout) 3) (find-tech 'reserve-tanks))

    (setf (aref (design-tech-slots colony-ship) 4) (find-tech 'colony-base)
          (aref (design-tech-slots colony-ship) 0) (find-tech 'heavy-laser-beam)
          (aref (design-tech-slots colony-ship) 1) (find-tech 'heavy-laser-beam))

    (setf (aref designs 0) scout
          (aref designs 1) colony-ship
          (building-design-of homeworld) scout)
    (build-ships homeworld scout 2)
    (build-ships homeworld colony-ship 1)
    ;; Building the ships causes a sound-event to be inserted into the
    ;; event list, but it won't play until next turn. We don't want it
    ;; to play at all, so just clear the whole list.
    (setf (event-list-of player) nil)))

(defun make-test-universe ()
   (let* ((uni (make-instance 'universe))         
          (*universe* uni)
          (player (make-test-player (format nil "Goldfinch ~D" (random 100)))))
     (with-slots (stars min-bound max-bound) uni
       (generate-random-starmap uni 80 4)
       (place-homeworld uni player "Earth")
       (place-homeworld uni (make-test-player "Corwin") "Pattern")
       (place-homeworld uni (make-test-player "Nunu") "Phleyphen")
       (place-homeworld uni (make-test-player "Scuzbart") "Scumyard")
       (assign-computer-colors uni)
       (generate-planets uni)
       (update-player-planets uni)
       (dolist (player (all-players uni))
         (init-player-techs player)
         (add-initial-ships-and-designs player))
       (setf min-bound (reduce #'vmin stars :key #'loc)
             max-bound (reduce #'vmax stars :key #'loc))
       #+NIL (format t "~&Universe bounds: ~A - ~A~%" min-bound max-bound))
     (values uni player)))
