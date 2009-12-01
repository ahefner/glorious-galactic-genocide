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

(defun planet->images (planet)
  (case (planet-type-of planet)
    ((terran jungle oceanic) (values (img :spl-oceanic-0) (img :ppl-ocean))) ; TODO: Terran images
    ((arid desert)  (values (img :spl-desert-0) (img :ppl-desert)))
    (tundra  (values (img :spl-tundra-0)  (img :ppl-tundra)))
    (minimal (values (img :spl-minimal-0) (img :ppl-minimal)))
    (barren  (values (img :spl-barren-0)  (img :ppl-barren)))
    (dead    (values (img :spl-dead-0)    (img :ppl-dead)))
    ;; Still need: volcanic, inferno, toxic, radiated.
    (otherwise (values (img :spl-dead-0) (img :ppl-dead)))))

(defun planet->starmap-image (planet)
  (nth-value 0 (planet->images planet)))

(defun draw-planet (planet x y)
  (draw-img (planet->starmap-image planet) x y))

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
        (setf label-img (render-label *universe* :sans label-height (name-of star) :align-x :center)))
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
                 (draw-img-deluxe (img :inner-16) ox oy color)
                 (draw-img (fleet-count-image num-ships) ox oy)))))

      (when (and planet explored)
        (draw-planet planet (+ x planet-offset) (+ y planet-offset)))
      (cond
        ((and owner (players-in-contact owner *player*))
         (draw-img-deluxe label-img x ly ocolor))
        (explored  (draw-img label-img x ly))))))

(defun activate-panel (new-panel)
  (with-slots (panel) *gameui*
    (when panel (finalize-object panel))
    (setf panel new-panel)))

(defun starmap-select (starmap object)
  (typecase object
    (star 
     (let ((colony (and.. (planet-of object) (colony-of $))))
       (when colony (activate-panel (make-instance 'colony-panel :colony colony :starmap starmap)))))))

;;;; Starmap panels

(defgeneric run-panel (panel uic bottom))

;;; A UI panel is essentially a gadget. It probably should be a
;;; gadget, and the only serious distinction is some stupidity with
;;; the way panels overlap and allocate space bottom-up that foils my
;;; notion of how child gadget should behave.

(defclass panel (dynamic-object) ())

;;;; Colony Control Panel

(defclass colony-panel (panel)
  ((colony :accessor colony-of :initarg :colony)
   (starmap :initarg :starmap)
   
   (name-label :initform nil)
   (class-label :initform nil)
   (stats-labels :initform nil)))

(defstruct cursor left x y (newline-p t) (descent 0) (y-pad 0) (min-line-height 14))

(defun cursor-draw-img (cursor img &optional (color (vector 255 255 255 255)))
  (when (cursor-newline-p cursor)
    (setf (cursor-x cursor) (cursor-left cursor)))
  (draw-img-deluxe img (cursor-x cursor) (cursor-y cursor) color)
  (maxf (cursor-descent cursor) (- (img-height img) (img-y-offset img)))
  (incf (cursor-x cursor) (img-width img)))

(defun cursor-newline (cursor)
  (incf (cursor-y cursor) (max (cursor-min-line-height cursor)
                               (+ (cursor-y-pad cursor) (cursor-descent cursor))))
  (setf (cursor-newline-p cursor) t
        (cursor-descent cursor) 0
        (cursor-x cursor) (cursor-left cursor)))

(defun cursor-draw-line (cursor images)
  (if (or (listp images) (vectorp images))
      (map nil (lambda (img) (cursor-draw-img cursor img)) images)
      (cursor-draw-img cursor images))
  (cursor-newline cursor))

(defun cursor-draw-lines (cursor line-seq)
  (map nil (lambda (line) (cursor-draw-line cursor line)) line-seq))

(defmethod run-panel ((panel colony-panel) uic bottom)
  (setf bottom 200)
  (with-slots (starmap colony name-label class-label stats-labels) panel
    (let* ((left (img :panel-left))
           (right (img :panel-right))
           (edge-top (- bottom (img-height left)))
           (player (owner-of colony))
           (planet (planet-of colony))
           (color1 (pstyle-label-color (style-of player)))
           (color2 (color-lighten color1) #+NIL (vector 180 180 180))
           (col1 (make-cursor :left 165 :y (- bottom 133)))
           (coordinate nil))

      (gadget-paint starmap uic #+NIL (child-uic uic 0 0 :active nil))
      
      (draw-bar left right *panel-fill* 0 edge-top (uic-width uic))
      (fill-rect 0 0 (uic-width uic) edge-top 7 7 7 244)

      (draw-img (nth-value 1 (planet->images planet)) 81 (- bottom 88))
      (cursor-draw-img col1 (orf name-label (render-label panel :gothic 20 (format nil "Colony ~A" (name-of colony)))) color1)
      (cursor-newline col1)
      (cursor-draw-img col1 (orf class-label (render-label panel :sans 11 (planet-type-description (planet-type-of planet)))) color2)
      (cursor-newline col1)
      (incf (cursor-y col1) 9)
      (cursor-draw-lines col1 
       (orf stats-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (list (format nil "Population: ~D Million (max. ~D)"
                                  (population-of colony) (compute-max-population colony))
                          (format nil "Industry: ~D Factories (max. ~D)"
                                  (factories-of colony) (max-factories colony))
                          (format nil "Pollution: ~A" (whenzero "None" (pollution-of planet)))
                          (format nil "~A Missile Bases" (whenzero "No" 0))
                          " "
                          (format nil "Available Production: ~D of ~D BC" 
                                  (unallocated-production-of colony)
                                  (production-of colony))))))

)))


(defmethod finalize-object ((panel colony-panel))
  (format t "~&Scuttled a colony panel. It should free its labels now...~%")
  (with-slots (name-label class-label stats-labels) panel
    (free-img name-label)
    (free-img class-label)
    (map nil #'free-img stats-labels)))

