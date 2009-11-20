;;;; Starmap

(in-package :g1)

(ffi:clines "#include \"sys.h\"")

;;;; Game definitions

(defclass named ()
  ((name :accessor name-of :initarg :name :initform nil)))

(defclass owned ()
  ((owner :accessor owner-of :initarg :owner :initform nil)))

(defclass universe ()
  ((stars :accessor stars :initarg :stars)
   (min-bound :accessor min-bound-of)
   (max-bound :accessor max-bound-of)
   (namelist :initform (shuffle (constellation-name-list)))))

(defclass ent ()
  ((loc :accessor loc :initarg :loc)))

(defclass star (named owned ent)
  ((style :initform (random 4))
   (owner :accessor owner-of)
   (planet :reader planet-of :initform nil)
   (spectral-class :reader spectral-class :initarg :spectral-class)
   ;; Grungy bits:
   (label-img :initform nil)))

(defclass player (named)
  ((explored-planets :reader explored-planets :initform (make-hash-table :test 'eq))

   (technologies :reader technologies-of :initform (make-hash-table :test 'eq))

   ;; All the stuff below should be considered as cached values:

   ;; Number of population units current technology permits to inhabit per square of each terrain type:
   (habitability-vector :accessor habitability-vector
                        :initform (vector 10 0 0 0)
                        :initarg :habitability-vector)

   ;; Vector of planets colonized by this player. This is updated by
   ;; <mumble> in one sweep across the universe rather than modified
   ;; incrementally, so it's sort of a cache for convenience.
   (colonies :reader colonies :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass planet (named owned)
  ((star :reader star-of :initarg :star)
   (planet-type :reader planet-type-of :initarg :planet-type)
   (planet-attribute :reader planet-attribute-of :initarg :planet-attribute)

   ;; Terrain units are counted in a 4 element vector: Land, Sea, Ice, Magma.
   (terrains :reader terrains-of :initform nil :initarg :terrains)
   (population :accessor population-of :initform 0 :initarg :population)
   (factories  :accessor factories-of  :initform 0 :initarg :factories)
   (pollution  :accessor pollution-of  :initform 0 :initarg :pollution)))

(defmethod print-object ((planet planet) stream)
  (print-unreadable-object (planet stream :identity nil :type t)
    (format stream "~A: ~A ~A" (name-of planet) (planet-type-of planet) (terrains-of planet))))


;;;; Planet types are just symbols.

(defvar *planet-types* '(terran oceanic jungle arid desert tundra minimal barren volcanic dead inferno toxic radiated))

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
          (+  0 (max 0 (+ -6 (random 9))))
          (+  2 (max 0 (+ -4 (random 13))))))

(defmethod choose-planet-terrains ((planet-type (eql 'volcanic)))
  (declare (ignorable planet-type))
  (vector (+  4 (random 20))
          (max 0 (+ -3 (random 6)))
          (+  0 (max 0 (+ -7 (random 9))))
          (+ 15 (random 55))))

(defmethod choose-planet-terrains ((planet-type (eql 'dead)))
  (declare (ignorable planet-type))
  (vector (+ 12 (random 28))
          0
          0
          (+  0 (max 0 (+ -8 (random 16))))))

(defmethod choose-planet-terrains ((planet-type (eql 'inferno)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 36))
          0
          0
          (+  0 (max 0 (+ -4 (random 22))))))

(defmethod choose-planet-terrains ((planet-type (eql 'toxic)))
  (declare (ignorable planet-type))
  (vector (+ 18 (random 36))
          0
          0
          (+  0 (max 0 (+ -7 (random 15))))))

(defmethod choose-planet-terrains ((planet-type (eql 'radiated)))
  (declare (ignorable planet-type))
  (vector (+ 13 (random 36))
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
   (07 'volcanic)
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

(defparameter *starfield-depth* 200)

(defun inset-random (range border)
  (+ border (random (- range border border))))

(defun random-star (&key x y z)
  (let ((border 80)
        (size 2000))
    (make-instance 'star
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
    (format t "~&Creating constellation ~A~%" name)
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
          (format t "~&Created star ~A. Type ~A at ~A~%" 
                  (name-of star) (spectral-class star) (loc star)))

    (sort stars #'> :key (lambda (star) (v.z (loc star))))))

(defun generate-planets (universe)
  (with-slots (stars) universe
    (loop for star across stars do
          (unless (planet-of star)
            (let* ((planet-type (choose-random-planet-type (spectral-class star)))
                   (terrains (and planet-type (choose-planet-terrains planet-type))))
              (when planet-type
                (setf (slot-value star 'planet)
                      (print
                      (make-instance 'planet
                                     :name (format nil "~A ~A" (name-of star) (random-elt '("I" "II" "III" "IV" "V")))
                                     :planet-type planet-type
                                     :terrains terrains)))))))))

(defun make-test-universe ()
  (time
   (let ((uni (make-instance 'universe)))
     (with-slots (stars min-bound max-bound) uni
       (generate-random-starmap uni 80 4)
       (generate-planets uni)
       (setf min-bound (reduce #'vmin stars :key #'loc)
             max-bound (reduce #'vmax stars :key #'loc))
       (format t "~&Universe bounds: ~A - ~A~%" min-bound max-bound))
     uni)))




;;;; Starmap UI

(defclass starmap (gadget)
  ((universe :reader universe-of :initarg :universe)
   (zoom-target :accessor zoom-target-of :initform 0.0)
   (zoom :accessor zoom-of :initform 0.0)
   (scroll-coord  :initform (v2 0 0))
   (scroll-target :initform (v2 0 0))))

(defclass debug-starmap (starmap) ())

(defmethod gadget-key-pressed ((starmap debug-starmap) uic keysym char)
  (declare (ignorable starmap uic))
  (print (list :press keysym char))
  (cond
    ((eql keysym (keysym :G))
     (setf (slot-value starmap 'universe) (make-test-universe)))))

(defmethod gadget-key-released ((starmap debug-starmap) uic keysym)
  (declare (ignore starmap uic))
  (print (list :release keysym)))

(defmethod gadget-paint ((gadget starmap) uic)
  (with-slots (universe scroll-coord scroll-target zoom zoom-target) gadget
    ;;(setf scroll-coord (v2 (uic-mx uic) (uic-my uic)))
    (let* ((stars (stars universe))
           (zoom-step 180)
           (min-zoom (* zoom-step (round -2600 zoom-step)))
           (max-zoom (* zoom-step (round 560 zoom-step)))
           (u-min (min-bound-of universe))
           (u-max (max-bound-of universe))
           (u-cam-border 400)
           ;; TODO: Filter the delta-t values for these purposes, as
           ;; the jitter seems to compound and make the scrolling feel
           ;; very gritty.
           (interp (expt 0.1 (uic-delta-t uic)))
           (camera (vec (v2.x scroll-coord) (v2.y scroll-coord) zoom)))
      (multiple-value-bind (pointer-x pointer-y)
          (inverse-perspective-transform camera (uic-mx uic) (uic-my uic) (ash *starfield-depth* -1))

        (when (clicked? uic +left+)
          (print (list :at pointer-x pointer-y))
          (setf scroll-target (v2 (round (clamp pointer-x (- (v.x u-min) u-cam-border) (+ u-cam-border (v.x u-max))))
                                  (round (clamp pointer-y (- (v.y u-min) u-cam-border) (+ u-cam-border (v.y u-max)))))))


        (unless (zerop (logand (ash 1 4) (uic-buttons-pressed uic)))
          (decf zoom-target zoom-step))

        (unless (zerop (logand (ash 1 3) (uic-buttons-pressed uic)))
          (incf zoom-target zoom-step))

        (setf zoom-target (clamp zoom-target min-zoom max-zoom)
              zoom (lerp interp zoom-target zoom)
              scroll-coord (v2 (round (lerp interp (v2.x scroll-target) (v2.x scroll-coord)))
                               (round (lerp interp (v2.y scroll-target) (v2.y scroll-coord)))))

        (render-starfield (v2.x scroll-coord) (v2.y scroll-coord))

        (loop for star across stars
              with pointer-radius-sq = (square 23)
              as v = (perspective-transform (v- (loc star) camera))
              as pointer-distance-sq = (+ (square (- (v.x v) (uic-mx uic)))
                                          (square (- (v.y v) (uic-my uic))))
              as x = (round (v.x v)) as y = (round (v.y v))
              do
              (when (<= pointer-distance-sq pointer-radius-sq)
                (when (clicked? uic +right+)
                  (cond
                    ((not (planet-of star)) (format t "~&Star ~A has no inhabitable planet.~%" (name-of star)))
                    (t (let* ((planet (planet-of star))
                              (terrain (terrains-of planet)))
                         (apply 'format t "~&Planet ~A (~A)~%  Total size: ~D (~D land, ~D sea, ~D ice, ~D magma)~%"
                                 (name-of planet) (planet-type-of planet)
                                 (reduce #'+ terrain)
                                 (coerce terrain 'list))))))                          
                (draw-img (img :halo-0) x y))
              (draw-star star x y t)
 )))))

(defconstant +perspective-foo+ 0.0014f0)  ; ~ 1/714

(defun perspective-transform (v)
  (declare (type v3 v)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (with-vector (ortho v)
    (let ((z (+ 1.0 (* ortho.z +perspective-foo+)))
          (w/2 (cx :float "window_width/2.0"))
          (h/2 (cx :float "window_height/2.0")))
      (vec (+ w/2 (/ ortho.x z))
           (+ h/2 (/ ortho.y z))
           ortho.z))))

(defun inverse-perspective-transform (camera sx sy z)
  (let ((w/2 (cx :float "window_width/2.0"))
        (h/2 (cx :float "window_height/2.0"))
        (p (+ 1.0 (* (- z (v.z camera)) +perspective-foo+))))
    (values (+ (* (- sx w/2) p) (v.x camera))
            (+ (* (- sy h/2) p) (v.y camera)))))
        

(defun star->image (star)
  (with-slots (style spectral-class) star
    (case spectral-class
      (O (img :star-o-00))
      (B (img :star-b-00))
      (A (img :star-white-0))
      (F (img :star-f-00))
      (G (case style 
           (0 (img :star-g-00))
           (1 (img :star-g-01))
           (2 (img :star-g-02))
           (3 (img :star-g-03))))
      (K (img :star-k-00))
      (M (case style 
           (0 (img :star-m-00))
           (1 (img :star-m-01))
           (2 (img :star-m-02))
           (3 (img :star-m-03))))
      (t (img :star-unknown)))))

(defun planet->image (planet)
  (case (planet-type-of planet)
    ((terran jungle oceanic) (img :spl-oceanic-0)) ; TODO: Terran images
    (arid  (img :spl-desert-0))         ; TODO: arid
    (desert  (img :spl-desert-0))
    (tundra (img :spl-tundra-0))
    (minimal (img :spl-minimal-0))
    (barren  (img :spl-barren-0))
    (dead    (img :spl-dead-0))
    ;; Still need: volcanic, inferno, toxic, radiated.
    (otherwise (img :spl-dead-0))))

(defun draw-planet (planet x y)
  (draw-img (planet->image planet) x y))

(defun draw-star (star x y draw-planet-p)
  (let* ((planet-offset 14)
         (planet (planet-of star))
         (img (star->image star))
         (label-height 12)         
         (ly (+ y label-height (ash (img-height img) -1))))
    (with-slots (label-img) star
      (unless label-img
        (setf label-img (render-label (name-of star) label-height :align-x :center)))
      (draw-img img x y)
      (when (and draw-planet-p planet)
        (draw-planet planet (+ x planet-offset) (+ y planet-offset)))
      (draw-img label-img x ly))))

