;;;; Starmap

(in-package :g1)

(ffi:clines "#include \"sys.h\"")

;;;; Starmap UI

#+NIL
(defclass panel-host-mixin ()
  ((panel :initform nil)
   (panel-y :initform nil)))

(defclass starmap (gadget)
  ((universe :reader universe-of :initarg :universe)
   (zoom-target :accessor zoom-target-of :initform 0.0)
   (zoom :accessor zoom-of :initform 0.0)
   (scroll-coord  :initform (v2 0 0) :initarg :scroll-coord)
   (scroll-target :accessor scroll-target-of :initform (v2 0 0) :initarg :scroll-target)))

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
  (query-presentations 
      (lambda (object type) :accept)
    (present-starmap gadget uic)
    (when (uic-active uic)
      (let* ((coordinate (find :empty-space *presentation-stack* :key #'presentation-type))
             (objects (mapcar #'presentation-object (remove coordinate *presentation-stack*))))
        (when (clicked? uic +left+)
          (cond
            ((and objects (not (second objects)))
             (starmap-select gadget (first objects)))
            (objects (printl :fixme "multiple objects under cursor"))
            (coordinate
             (scroll-to gadget (presentation-object coordinate)))))))))

(defun scroll-to (starmap v2)
  (with-slots (universe scroll-target) starmap
    (let ((u-cam-border 400)
          (u-min (min-bound-of universe))
          (u-max (max-bound-of universe)))
      (setf scroll-target (v2 (round (clamp (v2.x v2) (- (v.x u-min) u-cam-border) (+ u-cam-border (v.x u-max))))
                              (round (clamp (v2.y v2) (- (v.y u-min) u-cam-border) (+ u-cam-border (v.y u-max)))))))))

(defun present-starmap (gadget uic)
  (with-slots (universe scroll-coord scroll-target zoom zoom-target) gadget
    ;;(setf scroll-coord (v2 (uic-mx uic) (uic-my uic)))
    (let* ((stars (stars universe))
           (zoom-step 180)
           (min-zoom (* zoom-step (round -2600 zoom-step)))
           (max-zoom (* zoom-step (round 560 zoom-step)))           
           ;; I'm concerned that this actually amplifies the effect of framerate jitter..
           (interp (expt 0.1 (uic-delta-t uic)))
           (camera (vec (v2.x scroll-coord) (v2.y scroll-coord) zoom)))
      (multiple-value-bind (pointer-x pointer-y)
          (inverse-perspective-transform camera (uic-mx uic) (uic-my uic) (ash *starfield-depth* -1))

        (presenting (uic (v2 pointer-x pointer-y) :type :empty-space)
          (:display )
          (:hit (constantly t)))

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
              ;; This is absolutely NOT how things should be done.
              #+NIL
              (when (<= pointer-distance-sq pointer-radius-sq)
                (when (clicked? uic +right+)
                  (cond
                    ((not (planet-of star)) (format t "~&Star ~A has no inhabitable planet.~%" (name-of star)))
                    (t (let* ((planet (planet-of star))
                              (colony (colony-of planet))
                              (terrain (terrains-of planet)))
                         (apply 'format t "~&Planet ~A (~A)~%  Pop ~D  Ind ~D  Waste ~D~%  Total size: ~D (~D land, ~D sea, ~D ice, ~D magma)~%"
                                 (name-of planet) (planet-type-of planet)                                 
                                 (population-of colony)
                                 (factories-of colony)
                                 (pollution-of planet)
                                 (reduce #'+ terrain)
                                 (coerce terrain 'list))))))
                (draw-img (img :halo-0) x y))
              (present-star uic star x y)
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

(let (orbital-vectors)
  (defun relative-orbital-vectors ()
    (or orbital-vectors
        (setf orbital-vectors 
              (coerce (loop for i from 0 below 6 as v = (orbital-vector i) collect (v2 (round (v.x v)) (round (v.y v)))) 'vector)))))

(let (fleet-count-images)
  (defun fleet-count-image (n)
    (unless fleet-count-images
      (setf fleet-count-images
            (vector (img :fl1) (img :fl2) (img :fl3) (img :fl4) (img :fl5)
                    (img :fl6) (img :fl7) (img :fl8) (img :fl9) (img :flmany))))
    (cond
      ((<= 1 n 9) (aref fleet-count-images (1- n)))
      (t (aref fleet-count-images 9)))))
                    

(defun present-star (uic star x y)
  (let* ((planet-offset 14)
         (planet (planet-of star))
         (owner (and planet (owner-of planet)))
         (st (and owner (style-of owner)))
         (ocolor (and st (pstyle-label-color st)))
         (explored (explored? *player* star))
         (fleets (fleets-orbiting star))
         (in-sensor-range t)            ; XXX
         (img (star->image star))
         (label-height 12)
         (orbital-vectors (relative-orbital-vectors))
         (ly (+ y label-height (ash (img-height img) -1))))

    (with-slots (label-img) star
      (unless label-img
        (setf label-img (render-label (name-of star) label-height :align-x :center)))
      (presenting (uic star)
        (:hit (circle x y 23))
        (:display (draw-img img x y)))

      (when (and in-sensor-range fleets)
        ;; This is dumb, only draw the swoosh if there's a fleet in orbital 0.
        ;; (Should there ALWAYS be a fleet in orbital zero, provided there are any?)
        ;; Alternately, draw a rotated swoosh for every fleet.
        (draw-img (img :swoosh) x y)
        (loop for fleet in fleets as rel = (aref orbital-vectors (orbital-of fleet))
              ;for orbital from 0 below 6 as rel = (aref orbital-vectors orbital) 
              as ox = (+ x (v2.x rel)) as oy = (+ y (v2.y rel))
              as color = (pstyle-fill-color (style-of (owner-of fleet)))
              as num-ships = (reduce #'+ (stacks-of fleet) :key #'stack-count) 
              do
              (presenting (uic fleet :type :orbiting-fleet)
                (:hit (circle ox oy 8))
                (:display
                 (draw-img (img :circle-16) ox oy)
                 (draw-img-deluxe (img :inner-16) ox oy (aref color 0) (aref color 1) (aref color 2))
                 (draw-img (fleet-count-image num-ships) ox oy)))))

      (when (and planet explored)
        (draw-planet planet (+ x planet-offset) (+ y planet-offset)))
      (cond
        ((and owner (players-in-contact owner *player*))
         (draw-img-deluxe label-img x ly (aref ocolor 0) (aref ocolor 1) (aref ocolor 2)))
        (explored  (draw-img label-img x ly))))))

(defun activate-panel (new-panel)
  (with-slots (panel) *gameui*
    (setf panel new-panel)))

(defun starmap-select (starmap object)
  (typecase object
    (star 
     (activate-panel (make-instance 'star-panel :star object :starmap starmap)))))



;;;; -- Star/planet panel --

(defgeneric run-panel (panel uic bottom))

(defclass star-panel ()
  ((star :accessor star-of :initarg :star)
   (starmap :initarg :starmap)))

(defmethod run-panel ((panel star-panel) uic bottom)
  (declare (ignore bottom))
  (setf bottom 200)
  (let* ((left (img :panel-left))
         (right (img :panel-right))
         (edge-top (- bottom (img-height left)))
         (coordinate nil))

    (with-slots (starmap star) panel
      (present-starmap starmap (child-uic uic 0 0 :active nil))
      
      (draw-bar left right *panel-fill* 0 edge-top (uic-width uic))      
      (fill-rect 0 0 (uic-width uic) edge-top 7 7 7 244))))

