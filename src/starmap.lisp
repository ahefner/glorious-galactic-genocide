;;;; -*- Coding: latin-1 -*-

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
            ;; If there's precisely one object under the cursor, and
            ;; it isn't already selected, select it.
            ((and objects (not (second objects)) 
                  (not (eql (first objects) *selected-object*)))
             (starmap-select gadget (first objects)))
            ;; Otherwise scroll to the pointer coordinate. Note that
            ;; this allows double-clicking to select and scroll to an
            ;; object.
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
              as v = (perspective-transform (v- (loc star) camera))
              as x = (round (v.x v)) as y = (round (v.y v))
              do
              (when (eql star *selected-object*)
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
  (with-slots (panel closing-panel) *gameui*
    (when panel 
      (setf (slot-value *gameui* 'panel-y) (+ (img-height (img :gamebar-left)) (panel-height new-panel)))
      (finalize-object panel))
    (setf panel new-panel
          closing-panel nil)))

(defun starmap-select (starmap object)
  (typecase object
    (star
     (let* ((star (and (typep object 'star) object))
            (explored (explored? *player* star))
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

;;;; Starmap panels

(defgeneric run-panel (panel uic bottom))

;;; A UI panel is essentially a gadget. It probably should be a
;;; gadget, and the only serious distinction is some stupidity with
;;; the way panels overlap and allocate space bottom-up that foils my
;;; notion of how child gadget should behave.

(defclass panel (dynamic-object) 
  ((panel-height :accessor panel-height :initarg :panel-height)
   (starmap :initarg :starmap)))

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
  ((planet :accessor :planet-of :initarg :planet)
   (name-label :initform nil)
   (class-label :initform nil)
   (owner-label :initform nil)
   (col1-labels :initform nil)
   (col2-labels :initform nil)
   (area-label :initform nil))
  (:default-initargs :panel-height 168))

(defmethod run-panel ((panel planet-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap planet name-label class-label owner-label col1-labels col2-labels area-label) panel
    (let* ((*selected-object* (star-of planet))           
           (owner (and.. (colony-of planet) (owner-of $)))
           (maxpop-owner (and owner (planet-max-population planet owner)))
           (maxpop-us (planet-max-population planet *player*))
           (style (style-of (or owner *player*)))
           (color1 (pstyle-label-color style))
           (color2 (color-lighten color1))
           (terrains (terrains-of planet))           
           (col1 (make-cursor :left *planet-panel-col1-left* :y (- bottom *planet-panel-col1-baseline*)))
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
                    (flet ((show (name units)
                             (cond
                               ((zerop units) (format nil "No ~A" name))
                               ((= units 1) (format nil "Minimal ~A" name))
                               (t (format nil "~,1F% ~A (~:D Mm�)" (* 100 units (/ area)) name (* units units-to-mm))))))
                      (list (show "Solid Land"        (aref terrains 0))
                            (show "Surface Water"     (aref terrains 1))
                            (show "Surface Ice"       (aref terrains 2))
                            (show "Volcanic Activity" (aref terrains 3)))))))
      (incf (cursor-y col1) 9)
      (cursor-draw-img col1
        (orf area-label (render-label panel :sans 11 (format nil "Total Surface Area: ~:D Mm�" (* area units-to-mm)))))

      (with-slots (habitability production-modifier) planet
        (cursor-draw-lines col2
          (orf col2-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (remove nil
                     (list
                      (and (not (eql owner *player*)) (format nil "Distance: ~:D LY" (distance-from-player *player* planet)))
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
  (with-slots (name-label class-label col1-labels col2-labels area-label) panel
    (free-img name-label)
    (free-img class-label)
    (free-img area-label)
    (map nil #'free-img col1-labels)
    (map nil #'free-img col2-labels)))

;;;; Colony Control Panel

(defclass colony-panel (panel)
  ((colony :accessor colony-of :initarg :colony)
   (name-label :initform nil)
   (class-label :initform nil)
   (owner-label :initform nil)
   (stats-labels :initform nil)
   (production-label :initform nil))
  (:default-initargs :panel-height 168))

(defun draw-panel-background (uic bottom)
  (let* ((left (img :panel-left))
         (right (img :panel-right))
         (edge-top (- bottom (img-height left))))
    (draw-bar left right *panel-fill* 0 edge-top (uic-width uic))
    (fill-rect 0 0 (uic-width uic) edge-top 7 7 7 244)))

(defmethod run-panel ((panel colony-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap colony name-label class-label owner-label stats-labels production-label) panel
    (let* ((*selected-object* (star-of colony))
           (player (owner-of colony))
           (planet (planet-of colony))
           (color1 (pstyle-label-color (style-of player)))
           (color2 (color-lighten color1))
           (*planet-panel-col1-baseline* (+ *planet-panel-col1-baseline* (if (not (eql player *player*)) 7 0)))
           (col1 (make-cursor :left *planet-panel-col1-left* :y (- bottom *planet-panel-col1-baseline*))))

      (gadget-paint starmap (child-uic uic 0 0 :active (>= (uic-my uic) bottom)))
      (draw-panel-background uic bottom)
      
      (draw-img (nth-value 1 (planet->images planet)) *planet-panel-px* (- bottom *planet-panel-py*))
      (cursor-draw-img col1 (orf name-label (render-label panel :gothic 20 (format nil "Colony ~A" (name-of colony)))) color1)
      (cursor-newline col1)
      (unless (eql *player* player)
        (cursor-draw-img col1 (orf owner-label (render-label panel :sans 11 (format nil "Owned by ~A" (name-of player)))))
        (cursor-newline col1))
      (cursor-draw-img col1 (orf class-label (render-label panel :sans 11 (planet-type-description (planet-type-of planet)))) color2)
      (cursor-newline col1)
      (incf (cursor-y col1) 9)
      (cursor-draw-lines col1 
       (orf stats-labels
            (mapcar (lambda (string) (render-label panel :sans 11 string))
                    (list (format nil "Population: ~:D Million (max. ~:D)"
                                  (population-of colony) (compute-max-population colony))
                          (format nil "Industry: ~:D Factories (max. ~:D)"
                                  (factories-of colony) (max-factories colony))
                          (format nil "Pollution: ~:D" (whenzero "None" (pollution-of planet)))
                          (format nil "~A Missile Bases" (whenzero "No" 0))))))
      (incf (cursor-y col1) 9)
      (cursor-draw-img col1 
                       (orf production-label 
                        (render-label panel :sans 11
                         (format nil "Available Production: ~:D of ~:D BC" 
                                 (unallocated-production-of colony)
                                 (production-of colony)))))

      (when (and (uic-active uic) (pointer-in-radius* uic 71 *planet-panel-px* *planet-panel-py*))
        (draw-img (show-planet-label) *planet-panel-px* (- bottom *planet-panel-py* -50))
        (when (clicked? uic +left+)
          (activate-panel (make-instance 'planet-panel :planet (planet-of colony) :starmap starmap))))

)))


(defmethod finalize-object ((panel colony-panel))
  (with-slots (name-label class-label stats-labels production-label) panel
    (free-img name-label)
    (free-img class-label)
    (free-img production-label)
    (map nil #'free-img stats-labels)))

;;;; Star Panel

(defclass star-panel (panel)
  ((star :initarg :star)
   (name-label :initform nil)
   (class-label :initform nil)
   (distance-label :initform nil)
   (blurb-label :initform nil))
  (:default-initargs :panel-height 120))

(defmethod run-panel ((panel star-panel) uic bottom)
  (when (and (uic-active uic) (released? uic +right+)) (close-panels))
  (with-slots (starmap star name-label distance-label class-label blurb-label) panel
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
                                            (format nil "Distance: ~:D LY" (distance-from-player *player* star)))))
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
    
  
  