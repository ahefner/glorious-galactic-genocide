(in-package :g1)

(defclass research-ui (gadget)
  ((player :initarg :player)
   (unfold-rate :initform nil)
   (unfold :initform 0)
   (unfolded :initform nil)
   (inspector-tech :initform nil)
   (close-inspector :initform nil)
   (inspector-y :initform nil)   
   (alpha :initform 0)
   (alpha-target :initform 255)))

(defmethod small-name-label-of :around ((tech tech))
  (or (call-next-method)
      (setf (small-name-label-of tech) (global-label :sans 11 (name-of tech)))))

(defmethod big-name-label-of :around ((tech tech))
  (or (call-next-method)
      (setf (big-name-label-of tech) (global-label :gothic 20 (name-of tech)))))

(defgeneric print-tech-stats (cursor tech)
  (:method (cursor tech)
    (declare (ignore cursor tech))))

;;; Some people may call this block names such "hack" or
;;; "kludge". Philistines, they are.

(macrolet ((cout (label) `(cursor-draw-img cursor ,label))
           (pair (this that)
             `(let ((this ,this) (that ,that))
                (push (cons (if (stringp this) (global-label :sans 11 this) this)
                            (if (stringp that) (global-label :sans 11 that) that))
                      table) )))
  (let (table)
    (flet ((flush-table (cursor)
             (setf table (nreverse table))
             (let ((max (reduce #'max table :initial-value 0 :key (lambda (pair) (img-width (car pair))))))
               (dolist (pair table)
                 (cursor-advance cursor (- max (img-width (car pair))))
                 (cursor-draw-img cursor (car pair))
                 (cursor-advance cursor 8)
                 (cursor-draw-img cursor (cdr pair) #(255 255 255))
                 (cursor-newline cursor)))
             (setf table nil)))
      
      (defmethod print-tech-stats :after (cursor tech)
        (declare (ignore tech))
        (flush-table cursor))

      (defmethod print-tech-stats (cursor tech)
        (declare (ignore tech))
        (pair "Type" "Other"))

      (defmethod print-tech-stats (cursor (tech special-tech))
        (declare (ignore tech))
        (pair "Type" "Ship Equipment"))

      (defmethod print-tech-stats (cursor (tech engine))
        (pair "Type" "Engine")
        (pair "Maximum Speed" (gtxt (format nil "~F LY/turn" (engine-speed tech)))))

      (defmethod print-tech-stats (cursor (tech weapon))
        (pair "Type" (etypecase tech
                       (beam "Beam Weapon")
                       (particle-weapon "Particle Weapon")
                       (energy-weapon "Energy Weapon")
                       (missile "Missile")
                       (torpedo "Torpedo")
                       (projectile "Projectile Weapon")
                       (bomb "Bomb")
                       (weapon "Weapon")))
        (pair "Damage" (distribution->string (damage-of tech)))
        (if (typep tech 'missile-weapon)
            (pair "Speed" (gtxt (format nil "~F" (projectile-speed-of tech))))
            (pair "Range" (gtxt (format nil "~F km" (range-of tech)))))
        (unless (zerop (weapon-targetting-bonus-of tech))
          (pair "Targetting" (gtxt (format nil "+~D" (weapon-targetting-bonus-of tech)))))
        (unless (= 1.0 (shield-scaling-of tech))
          (pair "Shield Bypass" (gtxt (format nil "~F" (shield-scaling-of tech))))))
  
      (defmethod print-tech-stats (cursor (tech fuel))
        (pair "Type" "Power Source")
        (pair "Range" (gtxt (format nil "~D LY" (range-bonus tech)))))

      (defmethod print-tech-stats (cursor (tech hull))
        (pair "Type" (if (typep tech 'armor) "Armored Hull" "Hull"))
        (unless (zerop (armor-level-of tech))
          (pair "Absorbtion" (gtxt (write-to-string (armor-level-of tech)))))
        (pair "Hull Strength" (gtxt (format nil "~Fx" (hull-modifier-of tech)))))
  
      (defmethod print-tech-status (cursor (tech shield))
        (pair "Type" "Shield")
        (pair "Absorbtion" (gtxt (write-to-string (shield-level-of tech))))))))

(defconstant +research-inspector-height+ 160)

(let ((typeset nil)
      (lasttech nil))
  (defun run-ui-research-inspector (uic tech top &key (panel t))
    (when panel
      (draw-bar* (img :upanel-left) (img :upanel-right) (texture :upanel-fill) 0 top (uic-width uic)))
    (let* ((b (+ top 64))
           (baseline (+ top 50))
           (col2-x 250)
           (col2-width (min 600 (- (uic-width uic) col2-x 40)))
           (cursor (make-cursor :left 40 :y baseline :color (lighter-color))))
      (when (< b (uic-height uic))
        (fill-rect 0 b (uic-width uic) (uic-height uic) 20 20 20 244))
      (draw-img-deluxe (big-name-label-of tech) 16 (+ top 30) (label-color))
      (unless (eql tech lasttech)
        (setf typeset (typeset-text *word-map* col2-width (description-of tech))))
      (print-tech-stats cursor tech)
      (draw-typeset-text typeset col2-x baseline #(255 255 255 255)))))

(defmethod gadget-paint ((gadget research-ui) uic)
  (with-slots (player alpha alpha-target 
               unfold unfolded unfold-rate 
               inspector-tech inspector-y close-inspector) gadget
    (let ((color (pstyle-label-color (style-of player)))
          (closing? (zerop alpha-target))
          (iymin (- (uic-height uic) +research-inspector-height+ )))
      ;; Handle fade in/out.
      (when (released? uic +right+)
        (if inspector-tech
            (setf close-inspector t)
            (setf alpha-target 0)))
      ;; FIXME, framerate-dependent
      (setf alpha (clamp (+ alpha (if (and (zerop unfold) closing?) -13 13)) 0 255))
      (unless (= alpha 255)
        (gadget-paint (next-gadget gadget) (child-uic uic 0 0 :active nil)))

      (fill-rect 0 0 (uic-width uic) (uic-height uic) 0 0 0 alpha)
      
      (orf unfold-rate (* 200.0 (log (length (technologies-of player)) 1.5)))
      
      ;; When fade out complete, pop this gadget.
      (when (and (zerop alpha) (zerop alpha-target))
        (pop-gadget gadget))

      (when (and (= alpha 255) (not unfolded))
        (incf unfold (* unfold-rate (uic-delta-t uic))))

      (when closing? (setf unfold (max 0 (- unfold (* 3.5 unfold-rate (uic-delta-t uic))))))
      
      (with-elts (color r g b)
        (draw-img-deluxe* (global-label :gothic 30 "Technology") 7 30 r g b (min (round unfold) 255)))

      (let ((wub (max 0 (- (round unfold) 128))))
        (loop with y0 = 52
              with x = 20
              for tech in (technologies-of player) 
              for y from y0 by 14
              as label = (small-name-label-of tech)
              until (<= wub 0) do
              (when (>= y (- (uic-height uic) 10))
                (setf y y0
                      x (+ x 200)))
              (draw-img-deluxe* label x y 255 255 255 (clamp wub 0 255))
              (when (and (pointer-in-img-rect uic label x y) (clicked? uic +left+))
                (setf inspector-tech tech
                      close-inspector nil
                      inspector-y (or inspector-y (uic-height uic))))
              (decf wub 40)              
              finally (when (>= wub 255) (setf unfolded t))))

      (when (and close-inspector (>= inspector-y (uic-height uic)))
        (setf inspector-tech nil))

      (let ((iytarget (if close-inspector (uic-height uic) iymin)))
        (when inspector-tech
          (setf inspector-y (clamp (+ inspector-y 
                                      (* (signum (- iytarget inspector-y))
                                         (* 80.0 (sqrt (abs (- inspector-y iytarget))) (uic-delta-t uic))))
                                   iymin
                                   (uic-height uic))))
        (when inspector-tech
          (run-ui-research-inspector uic inspector-tech (round inspector-y)))))))

(defun prompt-for-research ()
  (when (some #'null (research-projects-of *player*))
    (printl "Imagine the game has just nagged you to select new research projects.")))

(defun unpresented-techs (player)
  (loop for tech in (technologies-of player)
        unless (gethash tech (presented-technologies-of player))
        collect tech))

(defun present-new-techs ()
  ;; TEMPORARY HACK:
  #+NIL
  (setf (gethash (find-tech 'ion-drive) (presented-technologies-of *player*)) nil
        (gethash (find-tech 'laser-beam) (presented-technologies-of *player*)) nil)
 
  (cond
    ;; If there are unpresented techs, present them, and let the
    ;; present-tech-ui segue into tech selection itself. This is the
    ;; usual case.
    ((unpresented-techs *player*)
     (activate-new-gadget (make-instance 'fade-transition-gadget
                                         :child (make-instance 'present-tech-ui :player *player*))))
    ;; Otherwise prompt for research (it will check if it's necessary
    ;; itself). This only occurs at the end of the first turn, when
    ;; there are no new techs, but research hasn't been selected yet.
    (t (prompt-for-research))))


;;;;

(defclass fade-transition-gadget (gadget)
  ((child :initarg :child)
   (level :initform 0.0)
   (color :initform #(0 0 0) :initarg :color)
   (state :initform :in)
   (rate  :initform 500 :initarg rate)))

(defmethod gadget-paint ((gadget fade-transition-gadget) uic)
  (with-slots (child level color state rate) gadget
    (unless (= level 0.0)
      (call-next-method gadget (child-uic uic 0 0 :active nil)))
    (setf level (clamp (+ level (* (case state (:out -1.0) (otherwise 1.0))
                                   rate
                                   (max 0.0001 (uic-delta-t uic))))
                       #|min|# 0.0 #|max|# 255.0))
    (with-vector (c color)
      (fill-rect 0 0 (uic-width uic) (uic-height uic) c.x c.y c.z (round level)))
    (cond
      ((and (eq state :in) (> level 254.0))
       (setf state t)
       (activate-new-gadget child))
      ((and (eq state t) (eq *gadget-root* gadget))
       (setf state :out))
      ((and (eq state :out) (< level 1.0))
       (pop-gadget gadget)))))

;;;; Present new tech

(defclass present-tech-ui (gadget)
  ((player :initarg :player)))

(defmethod gadget-paint ((gadget present-tech-ui) uic)
  (call-next-method)
  (with-slots (player) gadget
    (let ((new-techs (unpresented-techs player)))
      (when (released? uic (logior +left+ +right+))
        (setf (gethash (first new-techs) (presented-technologies-of *player*)) t)
        (pop new-techs))
      (cond
        ((not new-techs) (pop-gadget gadget))
        (t         
         (draw-img-deluxe (global-label :gothic 30 "New Discovery") 10 30 (label-color))
         (run-ui-research-inspector uic (first new-techs) (- (uic-height uic) +research-inspector-height+)))))))






