(in-package :g1)

(defclass browse-techs-ui (gadget)
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

(defvar *print-tech-show-costs* nil)

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

      (defmethod print-tech-stats :before (cursor tech)
        (declare (ignore cursor))
        (when *print-tech-show-costs*
          (pair "Research Cost" (gtxt (format nil "~:D" (tech-cost tech))))
          #+DONTLIKEIT (pair "Level" (gtxt (format nil "~:D" (level-of tech))))))
      
      (defmethod print-tech-stats :after (cursor tech)
        (declare (ignore tech))                 
        (flush-table cursor))

      (defmethod print-tech-stats (cursor tech)
        (declare (ignore tech cursor))
        (pair "Type" "Other"))

      (defmethod print-tech-stats (cursor (tech special-tech))
        (declare (ignore tech cursor))
        (pair "Type" "Ship Equipment"))

      (defmethod print-tech-stats (cursor (tech engine))
        (declare (ignore cursor))
        (pair "Type" "Engine")
        (pair "Maximum Speed" (gtxt (format nil "~F LY/turn" (engine-speed tech)))))

      (defmethod print-tech-stats (cursor (tech weapon))
        (declare (ignore cursor))
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
        (declare (ignore cursor))
        (pair "Type" "Power Source")
        (pair "Range" (gtxt (format nil "~D LY" (range-bonus tech)))))

      (defmethod print-tech-stats (cursor (tech hull))
        (declare (ignore cursor))
        (pair "Type" (if (typep tech 'armor) "Armored Hull" "Hull"))
        (unless (zerop (armor-level-of tech))
          (pair "Absorbtion" (gtxt (write-to-string (armor-level-of tech)))))
        (pair "Hull Strength" (gtxt (format nil "~Fx" (hull-modifier-of tech)))))
  
      (defmethod print-tech-stats (cursor (tech shield))
        (declare (ignore cursor))
        (pair "Type" "Shield")
        (pair "Absorbtion" (gtxt (write-to-string (shield-level-of tech))))))))

(defconstant +research-inspector-height+ 160)

(defun draw-bottom-panel (uic top)
  (let ((b (+ top 64)))
    (draw-bar* (img :upanel-left) (img :upanel-right) (texture :upanel-fill) 0 top (uic-width uic))
    (when (< b (uic-height uic))
      (fill-rect 0 b (uic-width uic) (uic-height uic) 20 20 20 244))))

(let ((typeset nil)
      (lasttech nil))
  (defun draw-ui-research-inspector (uic tech top panel new-discovery)
    (let* ((baseline (+ top 50))
           (col2-x 250)
           (col2-width (min 600 (- (uic-width uic) col2-x 40)))
           (cursor (make-cursor :left 40 :y baseline :color (lighter-color))))
      (when panel (draw-bottom-panel uic top))
      (when tech
        (let ((x 16))
          (when new-discovery
            (let ((label (global-label :gothic 20 "New Discovery: ")))
              (draw-img-deluxe label x (+ top 30) (label-color))
              (incf x (+ 6 (img-width label)))))
          (draw-img-deluxe (big-name-label-of tech) x (+ top 30) (label-color)))
        (unless (eql tech lasttech)
          (setf typeset (typeset-text *word-map* col2-width (description-of tech))))
        (print-tech-stats cursor tech)
        (draw-typeset-text typeset col2-x baseline #(255 255 255 255)))))
    
  ;; Oh, haha. I thought I had to factor the display code from the UI code. Turns out there isn't any UI code!
  (defun run-ui-research-inspector (uic tech top &key (panel t) new-discovery)
    (draw-ui-research-inspector uic tech top panel new-discovery)))

;;;; FIXME: Could be simplified using the new fade in-and-out
;;;; gadget. Already works, not making the effort.

(defmethod gadget-run ((gadget browse-techs-ui) uic)
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
        (gadget-run (next-gadget gadget) (child-uic uic :active nil)))

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

(defun unpresented-techs (player)
  (loop for tech in (technologies-of player)
        unless (gethash tech (presented-technologies-of player))
        collect tech))

;;;; Present new tech

(defclass present-techs-panel (panel)
  ((player :initarg :player)
   (enqueued-successor :initform nil)
   (last-tech :initform nil)))

(defmethod panel-height ((panel present-techs-panel))
  (declare (ignore panel))
  +research-inspector-height+)

(defmethod run-panel ((panel present-techs-panel) uic top)
  (with-slots (player last-tech enqueued-successor) panel
    (let ((new-techs (unpresented-techs player)))
      (when (released? uic (logior +left+ +right+))
        (setf (gethash (first new-techs) (presented-technologies-of *player*)) t)
        (pop new-techs))
      (when new-techs (setf last-tech (first new-techs)))
      (when (not new-techs)             
        (bottom-panel-request-close panel))
      (run-ui-research-inspector uic last-tech top :new-discovery t))))

;;;; Select research

(defclass select-research-panel (panel)
  ((player :initarg :player)
   (inspector-tech :initform nil)))

(defmethod panel-height ((panel select-research-panel))
  (declare (ignore panel))
  329)

(defmethod run-panel ((panel select-research-panel) uic top)
  (with-slots (player inspector-tech) panel
    (let ((available (available-techs-of player)))
      (draw-bottom-panel uic top)
      (draw-img-deluxe (global-label :gothic 20 "Select Technology") 16 (+ top 30) (label-color))
      (loop with x0 = 20  with x = x0
            with col-width = 200
            with y = (+ top 52)
            for tech in available
            as label = (small-name-label-of tech) do
            (draw-img-deluxe label x y (if (eql tech inspector-tech) (label-color) #(255 255 255 255)))
            (when (and (pointer-in-img-rect uic label x y)
                       (clicked? uic +left+)) ; FIXME: implicit active check
              (snd-click)
              (setf inspector-tech tech))
            (incf x col-width)
            (when (> (+ x 180) (uic-width uic))
              (setf x x0
                    y (+ y 14))))
      (when inspector-tech
        (let ((*print-tech-show-costs* t))
          (draw-ui-research-inspector uic inspector-tech (+ top 30 4 (* 15 10)) nil nil))
        (when (run-labelled-button uic (global-label :bold 14 "Research This")
                                   (- (uic-width uic) 74)
                                   (+ top (- (panel-height panel) 31))
                                   :color (label-color))
          ;; TODO
          (begin-research-project player inspector-tech)
          (bottom-panel-request-close panel))))))

(defun add-research-choice-panels (host)
  (loop for project across (research-projects-of *player*)
        when (null project) 
        do (enqueue-next-panel host (make-instance 'select-research-panel :player *player*))))
