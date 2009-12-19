;;;; -*- Coding: latin-1 -*-

;;;; Starmap

(in-package :g1)

(ffi:clines "#include \"sys.h\"")

;;;; Starmap UI

(defclass starmap (gadget)
  ((universe :reader universe-of :initarg :universe)
   (windup-factor :accessor windup-factor-of :initform 0.0)
   (camera :accessor camera-vector-of :initform (vec 0 0 0) :initarg :camera)
   (camera-target :accessor camera-target-of :initform (vec 0 0 0) :initarg :camera-target)))

(defclass debug-starmap (starmap) ())

(defmethod gadget-key-pressed ((starmap debug-starmap) uic keysym char)
  (declare (ignorable starmap uic))
  (print (list :press keysym char))
  (cond
    ((eql keysym (keysym :G))
     (setf (values *universe* *player*) (make-test-universe)
           (slot-value starmap 'universe) *universe*))
    ((eql char #\P) (setf *debug-show-packset* (not *debug-show-packset*)))
    ((eql keysym (keysym :E))
     (print "Exploring the universe!")
     (loop for star across (stars *universe*) do (explore-star star *player*)))))

(defmethod gadget-key-released ((starmap debug-starmap) uic keysym)
  (declare (ignore starmap uic))
  (print (list :release keysym)))

(defun query-starmap (starmap uic &key 
                      (query-fn (constantly :accept))
                      allow-middle-click ; Middle click counted as object selection?
                      (background-click (lambda (coordinate) (scroll-to starmap coordinate)))                      
                      (object-clicked (constantly nil)))
  (query-presentations query-fn
    (present-starmap starmap uic)
    (when (uic-active uic)
      (let* ((coordinate (find :empty-space *presentation-stack* :key #'presentation-type))
             (objects (mapcar #'presentation-object (remove coordinate *presentation-stack*))))
        (when (or (clicked? uic +left+) (and allow-middle-click (clicked? uic +middle+)))
          (cond
            ;; If there's precisely one object under the cursor, and
            ;; it isn't already selected, select it.
            ((and objects (null (rest objects)))
             (if (and (eql (first objects) *selected-object*) (typep (first objects) 'ent))
                 (scroll-to starmap (loc (first objects)) nil)
                 (funcall object-clicked (first objects))))
            ;; Otherwise report the map location where the user clicked.
            (coordinate (funcall background-click (presentation-object coordinate)))))))))

(defmethod gadget-paint ((gadget starmap) uic)
  (query-starmap gadget uic
   :object-clicked
   (lambda (object)
     (unless (eql object *selected-object*) ; Ignore if already selected.
       (starmap-select gadget object)))))

(defun scroll-to (starmap vector &optional use-z)
  (with-slots (universe camera-target) starmap
    (let ((u-cam-border 400.0)
          (u-min (min-bound-of universe))
          (u-max (max-bound-of universe))
          (vx (if (typep vector 'v2) (v2.x vector) (v.x vector)))
          (vy (if (typep vector 'v2) (v2.y vector) (v.y vector))))
      (setf camera-target 
            (vec (clamp (single vx) (- (v.x u-min) u-cam-border) (+ u-cam-border (v.x u-max)))
                 (clamp (single vy) (- (v.y u-min) u-cam-border) (+ u-cam-border (v.y u-max)))
                 (if (and use-z (typep vector 'v3)) (v.z vector) (v.z camera-target)))))))

(defun draw-travel-arrows (camera uic universe)
  (loop for fleet in (fleets-in-transit universe)
        as dest = (destination-of fleet) do
        (assert dest)
        (draw-line (fleet-screen-vector camera fleet) (screen-coord-of dest)
                   :color (pstyle-primary-color (style-of (owner-of fleet)))
                   :pattern-offset (logand (ash (uic-time uic) -15) 31))))

(defun move-travelling-fleet (delta-t fleet)
  (let* ((moverate 64.0)                  ; units per second
         (delta (v- (loc fleet) (vloc fleet)))
         (maxmove (len delta)))
    (when (> maxmove 0.1)              ; 1/10th of a pixel is certainly close enough..
      (setf (vloc fleet) (v+ (vloc fleet) (vscale delta (min 1.0 (/ (* delta-t moverate) maxmove))))))))

(defun present-starmap (gadget uic)
  (with-slots (universe camera-target camera windup-factor) gadget
    (let* ((stars (stars universe))
           (zoom-step 180)
           windup
           (min-zoom (single (* zoom-step (round -2600 zoom-step))))
           (max-zoom (single (* zoom-step (round 560 zoom-step))))
           ;; I'm concerned that this actually amplifies the effect of framerate jitter..
           (interp (expt 0.1 (uic-delta-t uic))))

      (setf windup-factor (* windup-factor (expt 0.2 (uic-delta-t uic)))
            windup (* 1.4 (max 1.0 (- windup-factor 0.5)))) ; So first button click doesn't contribute.
      (when (< windup-factor 0.001) (setf windup-factor 0.0))

      ;; Update camera position:
      ;;   (Vectors are supposed to be immutable, but I can get away with this..)
      (unless (zerop (logand (ash 1 4) (uic-buttons-pressed uic)))
        (decf (aref camera-target 2) (single zoom-step)))
      
      (unless (zerop (logand (ash 1 3) (uic-buttons-pressed uic)))
        (incf (aref camera-target 2) (single zoom-step)))
      
      (setf (aref camera-target 2) (clamp (aref camera-target 2) min-zoom max-zoom))
      
      (with-vectors (camera camera-target)
        (let ((z (if (< (abs (- camera-target.z camera.z)) 25.0)
                     camera.z
                     (lerp interp camera-target.z camera.z))))
          
          (when (vdist<= camera-target camera 30.0) (setf interp 1.0))
          
          (setf (camera-vector-of gadget)
                (vec (lerp interp camera-target.x camera.x)
                     (lerp interp camera-target.y camera.y)
                     z))))

      (with-vectors (camera)
        ;; Transform the star positions before drawing. The hook may need them!
        (loop for star across stars do (setf (screen-coord-of star) (perspective-transform (v- (loc star) camera))))

        ;; Draw the stars and do pointer vs. objects checks..
        (presenting (uic (inverse-perspective-transform camera (uic-mx uic) (uic-my uic) (ash *starfield-depth* -1))
                         :type :empty-space)
                    :hit (constantly t))

          (render-starfield (round camera.x) (round camera.y))
          
          (draw-travel-arrows camera uic universe)

          (funcall *starmap-display-under-hook*)

          (loop for star across stars
                as v = (screen-coord-of star)
                as x = (v2.x v) as y = (v2.y v)
                do
                (when (eql star *selected-object*)
                  (draw-img (img :halo-0) x y))
                (present-star uic star x y))

          (dolist (fleet (fleets-in-transit universe))
            (move-travelling-fleet (* windup (uic-delta-t uic)) fleet)
            (present-travelling-fleet uic fleet camera))))))

(defconstant +perspective-foo+ 0.0014f0)  ; ~ 1/714

(defun perspective-transform (v)
  (declare (type v3 v)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (with-vector (ortho v)                ; Why don't I do this in C?
    (let ((z (+ 1.0 (* ortho.z +perspective-foo+)))
          (w/2 (cx :int "window_width/2"))
          (h/2 (cx :int "window_height/2")))
      (v2round (+ w/2 (/ ortho.x z))
               (+ h/2 (/ ortho.y z)) ))))

(defun inverse-perspective-transform (camera sx sy z)
  (let ((w/2 (cx :float "window_width/2.0"))
        (h/2 (cx :float "window_height/2.0"))
        (p (+ 1.0 (* (- z (v.z camera)) +perspective-foo+))))
    (vec (+ (* (- sx w/2) p) (v.x camera))
         (+ (* (- sy h/2) p) (v.y camera))
         z)))

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
    (terran   (values (img :spl-terran-0)   (img :ppl-terran)))
    (jungle   (values (img :spl-jungle-0)   (img :ppl-jungle)))
    (oceanic  (values (img :spl-oceanic-0)  (img :ppl-ocean)))
    (arid     (values (img :spl-arid-0)     (img :ppl-arid)))
    (desert   (values (img :spl-desert-0)   (img :ppl-desert)))
    (tundra   (values (img :spl-tundra-0)   (img :ppl-tundra)))
    (minimal  (values (img :spl-minimal-0)  (img :ppl-minimal)))
    (barren   (values (img :spl-barren-0)   (img :ppl-barren)))
    (volcanic (values (img :spl-volcanic-0) (img :ppl-volcanic)))
    (dead     (values (img :spl-dead-0)     (img :ppl-dead)))
    (inferno  (values (img :spl-inferno-0)  (img :ppl-inferno)))
    (toxic    (values (img :spl-toxic-0)    (img :ppl-toxic)))
    (radiated (values (img :spl-radiated-0) (img :ppl-radiated)))
    ;; This shouldn't happen:
    (otherwise (values (img :spl-dead-0)  (img :ppl-dead)))))

(defun planet->starmap-image (planet)
  (nth-value 0 (planet->images planet)))

(defun draw-planet (planet x y)
  (draw-img (planet->starmap-image planet) x y))

(let (orbital-vectors)
  (defun relative-orbital-vectors ()
    (or orbital-vectors
        (setf orbital-vectors 
              (coerce (loop for i from 0 below 6 as v = (orbital-vector i) collect (v2round (v.x v) (v.y v))) 'vector)))))

(let (fleet-count-images)
  (defun fleet-count-image (n)
    (unless fleet-count-images
      (setf fleet-count-images
            (vector (img :fl1) (img :fl2) (img :fl3) (img :fl4) (img :fl5)
                    (img :fl6) (img :fl7) (img :fl8) (img :fl9) (img :flmany))))
    (cond
      ((<= 1 n 9) (aref fleet-count-images (1- n)))
      (t (aref fleet-count-images 9)))))

(defun draw-fleet-icon (fleet sx sy)
  (when (eq fleet *selected-object*) 
    (draw-img (img :fleet-selected-glow) sx sy))
  (draw-img (img :circle-16) sx sy)
  (draw-img-deluxe (img :inner-16) sx sy (pstyle-fill-color (style-of (owner-of fleet))))
  (draw-img (fleet-count-image (fleet-num-ships fleet)) sx sy))

(defun present-travelling-fleet (uic fleet camera)
  ;; XXX magic fudge factors
  ;; XXX in sensor range?!?!?
  (let ((coord (fleet-screen-vector camera fleet)))
    (presenting (uic fleet)
      :hit (circle (v2.x coord) (v2.y coord) 9)
      :display (draw-fleet-icon fleet (v2.x coord) (v2.y coord)))))

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

      (if (< (highlight-level-of star) 5)
          (setf (highlight-level-of star) 0)
          (draw-img-deluxe (img :halo-in-range) x y (vector 255 255 255 (truncate (highlight-level-of star)))))

      (setf (highlight-level-of star)
            (lerp (expt 0.1 (uic-delta-t uic))
                  (highlight-target-of star)
                  (highlight-level-of star)))

      (setf (highlight-target-of star) 0) ; Should be restored every frame before we reach here..

      (presenting (uic star)
        :hit (circle x y 23)
        :display (draw-img img x y))

      (when (and in-sensor-range fleets)
        ;; This is dumb, only draw the swoosh if there's a fleet in orbital 0.
        ;; (Should there ALWAYS be a fleet in orbital zero, provided there are any?)
        ;; Alternately, draw a rotated swoosh for every fleet.
        (draw-img (img :swoosh) x y)
        (loop for fleet in fleets as rel = (aref orbital-vectors (orbital-of fleet))
              ;for orbital from 0 below 6 as rel = (aref orbital-vectors orbital) 
              as ox = (+ x (v2.x rel)) as oy = (+ y (v2.y rel))
              do
              (assert (eql star (star-of fleet)))
              (presenting (uic fleet :type :orbiting-fleet)
                :hit (circle ox oy 8)
                :display (draw-fleet-icon fleet ox oy))))

      (when (and planet explored)
        (draw-planet planet (+ x planet-offset) (+ y planet-offset)))
      (cond
        ((and owner (players-in-contact owner *player*))
         (draw-img-deluxe label-img x ly ocolor))
        (explored  (draw-img label-img x ly))))))

(defun starmap-select (starmap object)
  (typecase object
    (fleet (activate-panel (make-instance 'fleet-panel :starmap starmap :fleet object)))
    (star
     (let* ((star (and (typep object 'star) object))
            (explored (explored? *player* star)) ; XXX
            (planet (and.. explored star (planet-of $)))
            (colony (and.. planet (colony-of $))))
       (cond
         (colony (activate-panel (make-instance 'colony-panel :starmap starmap :colony colony)))
         (planet (activate-panel (make-instance 'planet-panel :starmap starmap :planet planet)))
         (star   (activate-panel (make-instance 'star-panel   :starmap starmap :star star))))))))

;;;; Cursor Layout Utility

(defstruct cursor left x y (newline-p t) (descent 0) (y-pad 0) (min-line-height 14))

(defun cursor-draw-img (cursor img &optional (color (vector 255 255 255 255)))
  (when (cursor-newline-p cursor)
    (setf (cursor-x cursor) (cursor-left cursor)
          (cursor-newline-p cursor) nil))
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

;;;; Starmap panels

;;; A UI panel is essentially a gadget. It probably should be a
;;; gadget, and the only serious distinction is some stupidity with
;;; the way panels overlap and allocate space bottom-up that foils my
;;; notion of how child gadget should behave.

(defgeneric finish-for-turn (object)
  (:method (foo) (declare (ignore foo))))



;;; Common panel labels

(defvar *plabel-show-planet* nil)
(defun show-planet-label ()
  (orf *plabel-show-planet* (render-label *global-owner* :sans 12 "Show Planet Overview" :align-x :center)))

(defvar *plabel-show-colony* nil)
(defun show-colony-control-label ()
  (orf *plabel-show-colony* (render-label *global-owner* :sans 12 "Show Colony Controls" :align-x :center)))

;;;; Planet Panel

(defparameter *planet-panel-px* 81)
(defparameter *planet-panel-py* 88)
(defparameter *planet-panel-col1-left* 165)
(defparameter *planet-panel-col1-baseline* 135)
(defparameter *planet-panel-col2-left* 372)
(defparameter *planet-panel-col2-baseline* 121)

(defclass planet-panel (panel)
  ((planet :accessor planet-of :initarg :planet)
   (name-label :initform nil)
   (class-label :initform nil)
   (owner-label :initform nil)
   (col1-labels :initform nil)
   (col2-labels :initform nil)
   (area-label :initform nil)
   (description-typesetting :initform nil))
  (:default-initargs :panel-height 168))

(defmethod run-panel ((panel planet-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap planet name-label class-label owner-label col1-labels col2-labels area-label description-typesetting) panel
    (let* ((*selected-object* (star-of planet))
           (owner (and.. (colony-of planet) (owner-of $)))
           (maxpop-owner (and owner (planet-max-population planet owner)))
           (maxpop-us (planet-max-population planet *player*))
           (style (style-of (or owner *player*)))
           (color1 (pstyle-label-color style))
           (color2 (color-lighten color1))
           (terrains (terrains-of planet))
           (col1-baseline (+ *planet-panel-col1-baseline* (if (and owner (not (eql owner *player*))) 7 0)))
           (col1 (make-cursor :left *planet-panel-col1-left* :y (- bottom col1-baseline)))
           (col2 (make-cursor :left *planet-panel-col2-left* :y (- bottom *planet-panel-col2-baseline*)))
           (units-to-mm 6)
           (area (reduce #'+ terrains)))

      (gadget-paint starmap (child-uic uic 0 0 :active (>= (uic-my uic) bottom)))
      (draw-panel-background uic bottom)
      (draw-img (nth-value 1 (planet->images planet)) *planet-panel-px* (- bottom *planet-panel-py*))
      
      (cursor-draw-img col1 (orf name-label (render-label panel :gothic 20 (format nil "~A ~A"
                                                                                   (if owner "Colony" "Planet")
                                                                                   (name-of planet)))) color1)
      (cursor-newline col1)
      (cursor-draw-img col1 (orf class-label (render-label panel :sans 11 (planet-type-description (planet-type-of planet)))) color2)
      (cursor-newline col1)
      
      (when (and owner (not (eql *player* owner)))
        (cursor-draw-img col1 (orf owner-label (render-label panel :sans 11 (format nil "Owned by ~A" (name-of owner)))))
        (cursor-newline col1))

      (incf (cursor-y col1) 9)
      (setf (cursor-y col2) (cursor-y col1)) ; Align columns at this point
            
      
      (cursor-draw-lines col1
        (orf col1-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (flet ((show (name units &optional (pl name))
                             (cond
                               ((zerop units) (format nil "No ~A" name))
                               ((= units 1) (format nil "Minimal ~A" name))
                               (t (format nil "~,1F% ~A (~:D Mm²)" (* 100 units (/ area)) pl (* units units-to-mm))))))
                      (list (show "Solid Land"        (aref terrains 0))
                            (show "Surface Water"     (aref terrains 1))
                            (show "Surface Ice"       (aref terrains 2))
                            (show "Volcanic Activity" (aref terrains 3) "Volcanically Active"))))))
      (incf (cursor-y col1) 9)
      (cursor-draw-img col1
        (orf area-label (render-label panel :sans 11 (format nil "Total Surface Area: ~:D Mm²" (* area units-to-mm)))))

      (with-slots (habitability production-modifier) planet
        (cursor-draw-lines col2
          (orf col2-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (remove nil
                     (list
                      (and (not (eql owner *player*)) (format nil "Distance: ~:D LY" (distance-from-player-ly *player* planet)))
                      (and (or (not maxpop-owner) (= maxpop-owner maxpop-us))
                           (format nil "Max Population: ~:D million" maxpop-us))
                      (and maxpop-owner (/= maxpop-owner maxpop-us) (format nil "Max Population (owner): ~:D million" maxpop-owner))
                      (and maxpop-owner (/= maxpop-owner maxpop-us) (format nil "Max Population (us): ~:D million" maxpop-us))
                      (cond ((<= habitability 0.11) "Extremely Hostile")
                            ((<= habitability 0.25) "Hostile Environment")
                            ((<= habitability 0.55) "Capable of Supporting Life")
                            (t "Favorable to Organic Life"))
                      (cond ((<= production-modifier 0.4) "Extremely Mineral Poor")
                            ((<= production-modifier 0.8) "Mineral Poor")
                            ((<= production-modifier 1.5) "Abundant Minerals")
                            ((<= production-modifier 2.5) "Rich Mineral Deposits")
                            (t "Very Rich Mineral Deposits"))
                      (unless (zerop (pollution-of planet))
                        (format nil "Pollution: ~:D" (pollution-of planet)))
                      ))))))

      (and.. (find *player* (fleets-orbiting (star-of planet)) :key #'owner-of)
             (fleet-find-colony-ship-for $ planet)
             (run-labelled-button uic (global-label :bold 14 "Colonize") 436 (- bottom 40)
                                  :color (pstyle-label-color (style-of (owner-of $$))))
             (establish-colony (owner-of $$$) $$))

      (let ((x0 534))
        (orf description-typesetting 
             (nreverse (typeset-text *word-map* (min 400 (- (uic-width uic) 15 x0))
                                     (planet-type-blurb (planet-type-of planet)) :justify t)))
        (draw-typeset-text description-typesetting 
                           x0
                           (- bottom 84 (ash (typeset-word-y (first description-typesetting)) -1))
                           color1))
        
      ;; Allow the user to switch the colony control panel.
      (when (and (uic-active uic) 
                 (colony-of planet)
                 (eq (owner-of (colony-of planet)) *player*)
                 (pointer-in-radius* uic 71 *planet-panel-px* *planet-panel-py*))
        (draw-img (show-colony-control-label) *planet-panel-px* (- bottom *planet-panel-py* -50))
        (when (clicked? uic +left+)
          (activate-panel (make-instance 'colony-panel :colony (colony-of planet) :starmap starmap))))

      )))

(defmethod finalize-object ((panel planet-panel))
  (with-slots (name-label class-label col1-labels col2-labels area-label owner-label) panel
    (free-img name-label)
    (free-img class-label)
    (free-img area-label)
    (free-img owner-label)
    (map nil #'free-img col1-labels)
    (map nil #'free-img col2-labels)))

;;;; Colony Control Panel

(defclass colony-panel (panel panel-host-mixin)
  ((colony :accessor colony-of :initarg :colony)
   (name-label :initform nil)
   (class-label :initform nil)
   (owner-label :initform nil)
   (stats-labels :initform nil)
   (production-label :initform nil)
   (eta-label-cache :initform nil)
   (shipyard-color :initform nil)
   (amnt-labels :initform (make-array 4)))  
  (:default-initargs :panel-height 168))

(defun colpanel-cache-amnt (panel idx amount)
  (with-slots (amnt-labels) panel
    (let ((cell (aref amnt-labels idx)))
      (cond
        ((eql amount (car cell)) (cdr cell))
        (t 
         (when (cdr cell) (free-img (cdr cell)))
         (cdr (setf (aref amnt-labels idx)
                    (cons amount (render-label panel :sans 11 
                                               (format nil "~:D BC" (round amount)))))))))))

;;; Compute production for a good given its cost, accumulated funds,
;;; and rate of additional funds. Returns two values - the number
;;; which will be produced per turn, and (if that number is zero) the
;;; number of turns until completion of one unit. If only one is
;;; produced, report it as one turn remaining rather than 1 built,
;;; because that's better for the UI (I think).
(defun estimate-construction (cost accum rate)
  (let* ((cash (+ accum rate))
         (num-built (if (zerop cash) 0 (floor cash cost))))
    (cond
      ((= num-built 1) (values 0 1))
      ((not (zerop num-built)) (values num-built 0))
      ((and (zerop num-built) (zerop rate)) (values 0 0))
      (t (values 0 (ceiling (/ (- cost accum) rate)))))))

(defun estimate-ship-construction (colony rate)
  (estimate-construction (cost-of (building-design-of colony)) 
                         (spend-ships (spending-vector-of colony))
                         rate))

(let (build-ships-label
      set-defenses-label)
 (defmethod run-panel ((panel colony-panel) uic bottom)
   (when (or (not (alive? (colony-of panel))) ; Colony is dead
             (and (uic-active uic) (not (panel-of panel)) (released? uic +right+)))
    (close-panels))
   (when (and (panel-of panel) (released? uic +right+))
     (close-panels panel))
  (with-slots (starmap colony name-label class-label owner-label stats-labels eta-label-cache production-label shipyard-color) panel
    (let* ((*selected-object* (star-of colony))
           (player (owner-of colony))
           (planet (planet-of colony))
           (design (building-design-of colony))
           (color1 (pstyle-label-color (style-of player)))
           (post-cleanup-budget (post-cleanup-budget colony))
           (color2 (color-lighten color1))
           (sp (spending-prefs-of colony))
           (*planet-panel-col1-baseline* (+ *planet-panel-col1-baseline* (if (not (eql player *player*)) 7 0)))
           (col1 (make-cursor :left *planet-panel-col1-left* :y (- bottom *planet-panel-col1-baseline*)))
           (col2 (make-cursor :left *planet-panel-col2-left*)))

      (gadget-paint starmap (child-uic uic 0 0 :active (>= (uic-my uic) (max bottom (panel-y-of panel))))) ; FUX! FIXMES!
      (when (panel-of panel)
        (run-hosted-panel (child-uic uic 0 0 :active (>= (uic-my uic) bottom)) panel bottom))
      (draw-panel-background uic bottom)
      (draw-img (nth-value 1 (planet->images planet)) *planet-panel-px* (- bottom *planet-panel-py*))
      
      ;; Draw the title and first column
      (cursor-draw-img col1 (orf name-label (render-label panel :gothic 20 (format nil "Colony ~A" (name-of colony)))) color1)
      (cursor-newline col1)
      (cursor-draw-img col1 (orf class-label (render-label panel :sans 11 (planet-type-description (planet-type-of planet)))) color2)
      (cursor-newline col1)
      (unless (eql *player* player)
        (cursor-draw-img col1 (orf owner-label (render-label panel :sans 11 (format nil "Owned by ~A" (name-of player)))))
        (cursor-newline col1))
      (incf (cursor-y col1) 9)
      (setf (cursor-y col2) (cursor-y col1)) ; Align columns at this point
      (cursor-draw-lines col1 
       (orf stats-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (list (format nil "Population: ~:D million (max. ~:D)"
                                  (population-of colony) (compute-max-population colony))
                          (format nil "Industry: ~:D Factor~:@p (max. ~:D)"
                                  (factories-of colony) (max-factories colony))
                          (format nil "Pollution: ~:D (spending ~:D BC/turn)" 
                                  (whenzero "None" (pollution-of planet))
                                  (budget-cleanup post-cleanup-budget))
                          (format nil "~A Missile Base~:P" (whenzero "No" (num-bases-of colony)))))))
      (incf (cursor-y col1) 9)
      (cursor-draw-img col1
                       (orf production-label
                        (render-label panel :sans 11
                         (format nil "Available Production: ~:D of ~:D BC"
                                 (round (budget-unspent post-cleanup-budget))
                                 (round (production-of colony))))))

      ;; Draw the (no longer) hypothetical second column
      (let ((b (colony-compute-budget colony))
            (left (+ 48 *planet-panel-col2-left*))
            (idx 0)
            (adjust -12))
        (flet ((item (name idx id amnt)
                 (cursor-draw-img col2 (global-label :sans 11 name) color2)
                 (let ((old (aref sp idx)))
                   (setf (aref sp idx) (run-slider id uic left (+ adjust (cursor-y col2)) old 100 (zerop amnt))
                         (cursor-x col2) (+ left 160 4))
                   ;; You think that now, since I've switched to using
                   ;; a cacheobj, I could skip this, but no.
                   (unless (= old (aref sp idx))
                     (free-img (and eta-label-cache (cacheobj-derived eta-label-cache)))
                     (setf eta-label-cache nil)))
                 (cursor-draw-img col2 (colpanel-cache-amnt panel idx amnt))
                 (incf idx)
                 (cursor-newline col2) (incf (cursor-y col2) 7)))
          (item "Growth"   0 :colony-gr-slider (+ (budget-housing b) (budget-terraform b) (budget-factories b)))
          (item "Defense"  1 :colony-df-slider (+ (budget-bases b) (budget-shield b)))
          (item "Ships"    2 :colony-sh-slider (budget-ships b))
          (item "Tech"     3 :colony-re-slider (budget-research b))

          (when design
            (cachef (eta-label-cache design)
              (setf b (colony-compute-budget colony)) ; Recompute with new slider positions..
              (multiple-value-bind (built eta) (estimate-ship-construction colony (budget-ships b))
                (render-label panel :sans 11 
                              (cond
                                ((and (zerop built) (zerop eta))
                                 (setf shipyard-color #(150 150 150))
                                 (format nil "~A (idle)" (name-of design)))
                                ((zerop built) 
                                 (setf shipyard-color color1)                                    
                                 (format nil "~A (~:D turn~:P)" (name-of design) eta))
                                (t (setf shipyard-color color2)
                                   (format nil "~A (~:Dx)" (name-of design) built)))))))))

      ;; Draw the shipyard controls
      (let* ((cx 710)             
             (thumb (and design (design-thumbnail design)))
             (label (and design (cacheobj-derived eta-label-cache))))
        
        (when (run-labelled-button uic (global-label :bold 14 "Shipyard") cx (- bottom 70) :color color1)
          (activate-panel (make-instance 'shipyard-panel :colony colony) panel))
        (run-labelled-button uic (global-label  :bold 14 "Defenses") cx (- bottom 40) :color color1)
        (when design
          (draw-img-deluxe thumb cx (- bottom 125) shipyard-color)
          (draw-img label (- cx (ash (img-width label) -1)) #+NIL (- bottom 125 -10 (- (ash (img-height thumb) -1))) (- bottom 76))))

      ;; Allow toggling between planet and star panel
      (when (and (uic-active uic) (pointer-in-radius* uic 71 *planet-panel-px* *planet-panel-py*))
        (draw-img (show-planet-label) *planet-panel-px* (- bottom *planet-panel-py* -50))
        (when (clicked? uic +left+)
          (activate-panel (make-instance 'planet-panel :planet (planet-of colony) :starmap starmap))))

))))

(defmethod finalize-object ((panel colony-panel))
  (with-slots (name-label class-label stats-labels production-label owner-label eta-label-cache amnt-labels ) panel
    (when (slot-value panel 'panel) (finalize-object (slot-value panel 'panel)))
    (free-img name-label)
    (free-img class-label)
    (free-img production-label)
    (free-img owner-label)
    (when eta-label-cache (free-img (cacheobj-derived eta-label-cache)))
    (map nil #'free-img stats-labels)
    (loop for foo across amnt-labels when foo do (free-img (cdr foo))))
  (values))

;;;; Star Panel

(defclass star-panel (panel)
  ((star :initarg :star :accessor star-of)
   (name-label :initform nil)
   (class-label :initform nil)
   (distance-label :initform nil)
   (blurb-label :initform nil))
  (:default-initargs :panel-height 120))

(defmethod run-panel ((panel star-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap star closing name-label distance-label class-label blurb-label) panel
    (let* ((*selected-object* star)
           (color1 (pstyle-label-color (style-of *player*)))
           (color2 (color-lighten color1))
           (column (make-cursor :left 120 :y (- bottom 85))))
      
      (gadget-paint starmap (child-uic uic 0 0 :active (>= (uic-my uic) bottom)))
      (draw-panel-background uic bottom)

      (draw-img (star->image star) 60 (- bottom 60))
      (cursor-draw-img column
       (orf name-label (render-label panel :gothic 20 (name-of star) #+NIL (format nil "Star ~A" (name-of star))))
       color1)
      (cursor-newline column)
      (cursor-draw-img column
       (orf class-label (render-label panel :sans 11 (format nil "Spectral Class ~A" (spectral-class star))))
       color2)
      (cursor-newline column)
      (incf (cursor-y column) 9)
      (cursor-draw-img column
       (orf distance-label (render-label panel :sans 11
                            (format nil "Distance: ~:D LY" (distance-from-player-ly *player* star)))))      
      (cursor-newline column)
      (cursor-draw-img column
       (orf blurb-label
            (render-label panel :sans 11
             (cond
               ((not (explored? *player* star)) "This star has not yet been explored.")
               ((not (planet-of star)) "This star has no colonizable planets.")
              (t "")))))
)))

(defmethod finalize-object ((panel star-panel))
  (with-slots (name-label class-label distance-label blurb-label) panel
    (free-img name-label)
    (free-img class-label)
    (free-img distance-label)
    (free-img blurb-label)))

;;;; Stuffs

(defun design-thumbnail (design)
  (or (slot-value design 'thumbnail) (img :showfleet-sq-default)))

(defmethod name-label-of :around ((this design))
  (or (call-next-method)
      (setf (name-label-of this) (render-label this :sans 11 (name-of this)))))

(defmethod finalize-object ((this design))
  (with-slots (name-label) this
    (free-img name-label)))

;;;; Fleet panel

(defclass fleet-panel (panel)
  ((fleet :accessor fleet-of :initarg :fleet)
   (counts :initform (make-hash-table))
   (target :accessor target-of :initform nil)
   (highlighted-stars :initform nil)
   (too-far :initform nil)
   (finished :initform nil)
   (label-table :initform (make-hash-table)))
  (:default-initargs :panel-height 160))

(defun fleet-screen-vector (camera fleet)
  ;; Assumption: We're far enough into rendering that the star
  ;; positions are established for the current frame.
  (case (fleet-state fleet)
    (:orbiting
     (let ((sc (screen-coord-of (star-of fleet)))
           (rel (aref (relative-orbital-vectors) (orbital-of fleet))))
       (v2 (+ (v2.x rel) (v2.x sc)) (+ (v2.y rel) (v2.y sc)))))
    (:enroute
     (perspective-transform (v- (vloc fleet) camera)))
    (:departing                         ; FIXME !!
     (perspective-transform (v- (vloc fleet) camera)))))

(defun draw-adjust-buttons (uic x y num max)
  (if (= num max)
      (draw-img (img :tiny-plus-pressed) (+ x 23) y)
      (when (run-img-button uic (img :tiny-plus-released) (img :tiny-plus-pressed) (+ x 23) y)
        (incf num)))  
  (if (zerop num)
      (draw-img (img :tiny-minus-pressed) (- x 23) y)
      (when (run-img-button uic (img :tiny-minus-released) (img :tiny-minus-pressed) (- x 23) y)
        (decf num)))
  (cond
    ((= max 1) (draw-img (img :tiny-half-pressed) x y))
    (t (if (run-img-button uic (img :tiny-half-released) (img :tiny-half-pressed) x y)
           (setf num (floor max 2))
           num)))
  num)

(defun fleetpanel-counts-empty? (fleet counts)
  (and (eql (hash-table-count counts) (length (stacks-of fleet)))
       (loop for count being the hash-values of counts unless (zerop count) do (return nil) finally (return t))))

;;; This function ought to be taken out and shot.
(defmethod run-panel ((panel fleet-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap fleet label-table closing highlighted-stars
                       too-far counts target finished) panel

    (macrolet ((label (key) `(gethash ,key label-table))
               (invalidate-label (key) `(progn (free-img (label ,key))
                                               (setf (label ,key) nil)))
               (deflabel (key (&key (face :sans) (size 11) (align-x :left)) fmt &rest args)
                 `(orf (label ,key) (render-label panel ,face ,size (format nil ,fmt ,@args) :align-x ,align-x))))

      (setf fleet (and fleet (fleet-successor fleet)))
      (when (and fleet (not (alive? fleet))) (setf fleet nil))

      ;; This branch can run briefly as the (now empty) panel closes when all the ships have been dispatched.
      (unless fleet
        (gadget-paint starmap uic)
        (draw-panel-background uic bottom)
        (close-panels)
        (return-from run-panel))

      (let* ((*selected-object* (unless closing fleet))
             (top (- bottom (panel-height panel) -26))
             (owner (owner-of fleet))
             (do-switch nil)
             (movable (and (eq owner *player*) (star-of fleet))) ; FIXME for hyperspace communication
             (color1 (pstyle-label-color (style-of owner)))
             (color2 (color-lighten color1)))

        ;;; Compute stars in range of fleet, set their highlight attribute
        (when movable
          (orf highlighted-stars
               (loop with table = (make-hash-table) ; FIXME, quadratic, cache the distances.
                     for star across (stars *universe*)
                     when (and (star-in-range-of-fleet star fleet counts)
                               (not (eql star (star-of fleet))))
                     do (setf (gethash star table) t)
                     finally (return table))))

        ;; Run the starmap first. We have to do our own presentation
        ;; query here to handle the ship movement, otherwise we'd just
        ;; call gadget-paint like the other panels do.
        ;; (gadget-paint starmap (child-uic uic 0 0 :active (>= (uic-my uic) bottom)))
        
        (let ((*starmap-display-under-hook*
               (lambda ()
                 (when (and movable (not closing))
                   (maphash (lambda (star foo)
                              (declare (ignore foo))
                              ;; Set it every frame, because they zero it every frame. Less bookkeeping.
                              (setf (highlight-target-of star) 255))
                            highlighted-stars))

                 (when (and target (not closing))
                   (draw-line (fleet-screen-vector (camera-vector-of starmap) fleet) (screen-coord-of target)
                              :color (if too-far #(120 120 120 255) #(255 255 255 255))
                              :pattern-offset (logand (ash (uic-time uic) -15) 31))))))
          (query-starmap starmap (child-uic uic 0 0 :active (>= (uic-my uic) bottom))
                         :allow-middle-click t
                         :object-clicked (lambda (star)
                                           (cond ((and movable (typep star 'star))
                                                  (setf target (if (eql star (star-of fleet)) nil star)
                                                        too-far (not (star-in-range-of-fleet star fleet counts)))
                                                  (flush-labels label-table)
                                                  (when (and (clicked? uic +middle+)
                                                             (fleet-panel-ready-to-send panel))
                                                    (setf do-switch t))) ; AUGH
                                                 (t (starmap-select starmap star))))))

        (draw-panel-background uic bottom)

        (when do-switch
          (fleet-panel-commit-and-switch panel)
          (return-from run-panel))
        
        ;; Fleets can be in three states: en route, departing, or
        ;; orbiting. "Departing" fleets have both their destination
        ;; and star slots set, and their orders can still be changed.

        ;; Now, the fleet controls.
        (let ((dest (or (and (not too-far) target) (destination-of fleet)))
              (title-label))

          (deflabel :title (:face :gothic :size 20) "~A fleet ~A ~A~A"
                    (name-of (race-of owner)) ; FIXME multiplayer
                  (cond
                    ((not dest) "orbiting")
                    ((not (or target (star-of fleet))) "en route to" )
                    ((and target (not (star-of fleet))) "changing course for")
                    (t "departing for"))
                  (name-of (or dest (star-of fleet)))                      
                  (if (null dest)
                      ""
                      (format nil " (ETA ~:D turns)" (fleet-compute-eta-to fleet dest))))
          (setf title-label (label :title))
          (when (fleetpanel-counts-empty? fleet counts)
             (deflabel :noships (:face :gothic :size 20) "No ships selected")
             (setf title-label (label :noships)))
          (draw-img-deluxe title-label 17 top color1))

        (loop with y = (+ top 2 44)
              for stack in (stacks-of fleet)
              for x from (+ 5 42) by 84 
              as thumb = (design-thumbnail (stack-design stack))
              as num-sending = (gethash stack counts (stack-count stack))
              as num-total = (stack-count stack)
              as stack-color = (if (zerop num-sending) #(110 110 110 255) color2)
              do
              (flet ((setnum (n)
                       (when (or (zerop n) (and (not (zerop n)) (eql 0 (gethash stack counts))))
                         (setf highlighted-stars nil))
                       (unless (= n (gethash stack counts (stack-count stack)))
                         (invalidate-label stack))
                       (if (= n num-total)
                           (remhash stack counts)
                           (setf (gethash stack counts) n))))
                (draw-img-deluxe thumb x y stack-color)
                (if (< num-sending num-total)
                    (deflabel stack (:align-x :center :face (if (zerop num-sending) :italic :sans))
                      "~A~[~:; (~:*~:D/~:D)~]" (name-of (stack-design stack)) num-sending num-total)
                    (deflabel stack (:align-x :center)
                      "~A~[~;~:; (~:*~:D)~]" (name-of (stack-design stack)) num-sending))
                (draw-img (label stack) x (+ y 50))
                (when movable
                  (setnum (draw-adjust-buttons uic x (+ y 67) num-sending num-total)))
                (when (and (clicked? uic +left+) (pointer-in-img-rect uic thumb x y))
                  (invalidate-label stack)
                  (setf highlighted-stars nil)
                  (cond
                    ((zerop (hash-table-count counts))
                     (loop for s in (stacks-of fleet) unless (eq stack s) do (invalidate-label s) (setf (gethash s counts) 0)))
                    ((zerop (gethash stack counts 1)) (remhash stack counts))
                    (t (setnum 0))))))

        ;; Buttons
        (deflabel :send (:face :bold :size 14) "Send Now")
        (let ((y (+ top -20))
              (button-pad 36)
              (x (- (uic-width uic) 12)))
          (flet ((allot (label)
                   (- (uic-width uic) 68)
                   #+NIL
                   (let ((adj (+ 5 button-pad (img-width (label label)))))
                     (decf x adj)
                     (+ x (ash adj -1)))))
            (if (fleet-panel-ready-to-send panel)
                (when (or (run-labelled-button uic (label :send) (allot :send) y :color color1))
                  (fleet-panel-commit-and-switch panel))
                (when (and.. fleet
                             (eql (fleet-state fleet) :orbiting)
                             (planet-of (star-of fleet))
                             (not (owner-of $))
                             (fleet-find-colony-ship-for fleet $$))
                  (deflabel :colonize (:face :bold :size 14) "Colonize")
                  (when (run-labelled-button uic (label :colonize) (allot :colonize) y :color color1)
                    (establish-colony (owner-of fleet) (fleet-find-colony-ship-for fleet (planet-of (star-of fleet)))))))))
))))

(defun fleet-panel-ready-to-send (panel)
  (with-slots (fleet counts target label-table too-far) panel
    (and target 
         (not too-far) 
         (not (fleetpanel-counts-empty? fleet counts))
         (not (eql target (destination-of fleet))))))

(defun fleet-panel-commit-and-switch (panel)
  (with-slots (fleet target label-table) panel
    (fleet-panel-commit panel)
    (flush-labels label-table)
    (cond
      ;; If any ships remain in the source fleet, continue viewing that fleet.
      ((stacks-of fleet) (values))
      ;; Nobody left? Close the panel.
      (t (setf fleet nil) (close-panels)))))
  
  


(defun fleet-panel-commit (panel)
  ;; Returns new fleet!!
  (with-slots (fleet target counts too-far highlighted-stars) panel
    ;; Split the fleet and link into universe.
    (when (and target (not too-far))
      (let ((new (split-fleet fleet counts :star (star-of fleet) :loc (loc fleet))))
        (unless (zerop (fleet-num-ships new)) (send-fleet new target))
        (clrhash counts)
        (setf highlighted-stars nil
              target nil)))))

(defmethod finish-for-turn ((panel fleet-panel))
  (fleet-panel-commit panel))

(defun flush-labels (label-table)
  (maphash (lambda (key img) (declare (ignore key)) (free-img img)) label-table)
  (clrhash label-table))

(defmethod finalize-object ((panel fleet-panel))
  (with-slots (label-table fleet) panel
    (flush-labels label-table)
    (fleet-panel-commit panel)))

;;;; UI Glue

(defun ui-finish-turn ()
  (with-slots (panel) *gameui*
    (finish-for-turn panel)))  

(defun update-ui-for-new-turn ()
  ;; If there are planels open, update them.
  (with-slots (panel closing-panel starmap) *gameui*
    (unless closing-panel
      (cond
        ((typep panel 'star-panel)
         (starmap-select starmap (star-of panel)))
        ((typep panel 'planet-panel) 
         (activate-panel (make-instance 'planet-panel :starmap starmap :planet (planet-of panel))))
        ((typep panel 'colony-panel)
         (activate-panel (make-instance 'colony-panel :starmap starmap :colony (colony-of panel)
                                        :init-panel (panel-of panel) 
                                        :init-closing-panel (child-panel-close? panel)
                                        :init-panel-y (panel-y-of panel))))
        ((typep panel 'fleet-panel)
         (activate-panel (make-instance 'fleet-panel :starmap starmap :fleet (fleet-of panel))))))))

;;;; AUGH

(defclass shipyard-panel (panel)
  ((colony :accessor colony-of :initarg :colony)
   (label-table :initform (make-hash-table)))
  (:default-initargs :panel-height 170))

(defmethod run-panel ((panel shipyard-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels (host-of panel)))
  (with-slots (colony) panel
    (let ((top (- bottom (panel-height panel) -22))
          (color (pstyle-label-color (style-of *player*))))
      (draw-panel-background uic bottom)
      (draw-img (global-label :gothic 20 "Select Ship Construction") 13 top)
      (loop with y = (+ top 57)
            with lighter = (color-lighten color)
            for design across (ship-designs-of *player*)
            for x from (+ 5 42) by 84
            as thumb = (and design (design-thumbnail design))
            do
            (when design
              (let* ((col (if (eql design (building-design-of colony)) lighter color)))
                (draw-img-deluxe thumb x y col)
                (draw-img-deluxe (name-label-of design) (- x (ash (img-width (name-label-of design)) -1)) (+ y 50) lighter)
                (when (and (pointer-in-img-rect uic thumb x y) (released? uic +left+))
                  (setf (building-design-of colony) design)))))
      (when (run-labelled-button uic (global-label :bold 14 "Idle Shipyard") (- (uic-width uic) 80) (- bottom 33) 
                                 :color color)
        (setf (building-design-of colony) nil)
        (close-panels (host-of panel))))))

(defmethod name-label-of :around ((design design))
  (or (call-next-method)
      (setf (slot-value design 'name-label)
            (render-label design :sans 11 (name-of design)))))

  

;;;; Fuck me, I'm out of duct tape!
