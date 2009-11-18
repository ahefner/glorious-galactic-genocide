;;;; Starmap

(in-package :g1)

(ffi:clines "#include \"sys.h\"")

;;;; Game definitions

(defclass universe ()
  ((stars :accessor stars :initarg :stars)
   (min-bound :accessor min-bound-of)
   (max-bound :accessor max-bound-of)
   (namelist :initform (shuffle (constellation-name-list)))))

(defclass ent ()
  ((loc :accessor loc :initarg :loc)))

(defclass star (ent)
  ((name :accessor name-of :initarg :name)
   (style :initform (random 4))
   (label-img :initform nil)
   (spectral-class :reader spectral-class :initarg :spectral-class)))

;;;; Map generator

(defun choose-random-spectral-class ()
  (random-choice 
   (1 'O)                                   ; Blue
   (1 'B)                                   ; Blue/white
   (1 'A)                                   ; White
   (1 'F)                                   ; White (yellowish) (green, to make things interesting)
   (1 'G)                                   ; Yellow
   (1 'K)                                   ; Orange
   (3 'M)))                                 ; Red



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

(defun make-test-universe ()
  (time
   (let ((uni (make-instance 'universe)))
     (with-slots (stars min-bound max-bound) uni
       (generate-random-starmap uni 80 4)
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
  (declare (ignore starmap uic))
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
           (interp (expt 0.1 (uic-delta-t uic)))
           (camera (vec (v2.x scroll-coord) (v2.y scroll-coord) zoom)))
      (multiple-value-bind (pointer-x pointer-y)
          (inverse-perspective-transform camera (uic-mx uic) (uic-my uic) (ash *starfield-depth* -1))

        (when (clicked? uic +left+)
          (print (list :at pointer-x pointer-y))
          (setf scroll-target (v2 (round pointer-x) (round pointer-y))))

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
              with pointer-radius-sq = (square 19)
              as v = (perspective-transform (v- (loc star) camera))
              as pointer-distance-sq = (+ (square (- (v.x v) (uic-mx uic)))
                                          (square (- (v.y v) (uic-my uic))))
              do
              (when (<= pointer-distance-sq pointer-radius-sq)
                (draw-img (img :halo-0) (round (v.x v)) (round (v.y v))))
              (draw-star star v))))))        

(defconstant +perspective-foo+ 0.0014)  ; ~ 1/714

(defun perspective-transform (v)
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

(defun draw-star (star v)
 (with-vector (v)
  (let* ((x (round v.x))
         (y (round v.y))
         (img (star->image star))
         (label-height 12)
         (ly (+ y label-height (ash (img-height img) -1))))
    (with-slots (label-img) star
      (unless label-img
        (setf label-img (render-label (name-of star) label-height :align-x :center)))
      (draw-img img x y)
      (draw-img label-img x ly)))))

