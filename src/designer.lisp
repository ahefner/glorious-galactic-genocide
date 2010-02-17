;;;; Ship Design UI

(ffi:clines "#include \"sys.h\"")

(in-package :g1)

(defconstant +designer-fade-rate+ (/ 1.5))

(defun copy-ship-design (design)
  ;; Slot number isn't preserved because this isn't the real design.
  (let ((copy (make-instance 'design                             
               :name (name-of design)
               :techs (copy-seq (design-tech-slots design))
               :engine (engine-of design)
               :range-bonus (range-bonus-of design)
               :slot-num nil)))
    (prog1 copy
      ;; Recompute derived attributes.
      (analyze-design copy)
      ;; This is a copy, so preserve the original cost.
      (setf (cost-of copy) (cost-of design)))))

(defclass designer (gadget)
  ((ship-type :initform nil :initarg :ship-type)
   (design :initform nil :initarg :design)
   (player :initarg :player)
   (background-transition :initform (make-instance 'in-and-out :rate +designer-fade-rate+))
   (schematic-transition :initform nil)))

(defun open-designer-ui ()
  (activate-new-gadget 
   (make-instance 'fade-transition-gadget                  
                  :child (make-instance 'designer
                                        :player *player*
                                        :design (print (get-nth-design *player* 1))
                                        #|:ship-type (working-design-of *player*)|#))))

(defun prompt-for-design (uic)
  ;; TODO
  (find-ship-type "Modular Cruiser"))

(defun subfoo (x y)
  (/ (max 0.0 (- x y)) (- 1.0 y)))

(defstruct hardpoint-layout line-points label-base)

(let ((layouts (make-hash-table)))
  (defun reset-hardpoint-layout (hardpoint)
    (setf (gethash hardpoint layouts) (make-hardpoint-layout :line-points (list (v2 0 0)))))
  (defun save-hardpoint-layout (ship-type hardpoint layout)
    (setf (file (ship-asset-path ship-type (name-of hardpoint))) layout))
  (defun ensure-hardpoint-layout (ship-type hardpoint)
    (or (gethash hardpoint layouts)
        (multiple-value-bind (layout found-p)
            (maybe-file (ship-asset-path ship-type (name-of hardpoint)))
          (cond
            (found-p (setf (gethash hardpoint layouts) layout))
            (t nil))))))

(defun designer-present-slot (slot tech layout)
  (let* ((upper-color (lighter-color))
         (line-points (hardpoint-layout-line-points layout))
         (lp (hardpoint-layout-label-base layout))
         (cursor (and lp (make-cursor :left (v2.x lp) :y (v2.y lp))))
         (face :gothic)
         (size 16)
         (space 3)
         (amount (and tech 1))
         (amount-label (and amount (global-label face 16 (format nil "~:D" amount))))
         (slot-label (global-label face size (name-of (or tech slot))))
         (empty-label (global-label face size (empty-string-of slot))))
    (when (>= (length line-points) 2) (draw-flexiline line-points))
    (when cursor
     (when (and amount-label (> amount 1))
       (cursor-draw-img cursor amount-label upper-color)
       (cursor-advance cursor space))
     (cursor-draw-img cursor slot-label upper-color)
     (unless tech
       (cursor-advance cursor space)
       (cursor-draw-img cursor empty-label upper-color)))
    ))

(defmethod gadget-run ((gadget designer) uic)
  (with-slots (player ship-type design background-transition schematic-transition) gadget
    (multiple-value-bind (level transition)
        (run-in-and-out background-transition (uic-delta-t uic))
      ;; We might inspect another player's design with this. Rebinding
      ;; *player* makes things simpler.
      (let ((*player* player))
        ;; Draw background.
        (let ((tex (filtered-texture "falloff.jpg"))
              (alpha (min 128 (f->b (* level 3)))))
          (bind-texobj tex)
          (set-blend-mode :replace-alpha)
          (draw-tile-scaled 0 0 (uic-width uic) (uic-height uic)
                            0 0 (texture-width tex) (texture-height tex)
                            (color-with-alpha (label-color) alpha))
          (set-blend-mode :blend)
          (draw-img-deluxe (global-label :gothic 26 "Ship Design") 10 30
                           (color-with-alpha (label-color) (f->b (* level 1)))))

        ;; This is a bit silly, really.
        (when design
          (setf ship-type (design-type design)))

        ;; Have we a ship-type?
        (cond 
          ((not ship-type)
           (setf ship-type (prompt-for-design uic))
           (when (and ship-type (not design))
             (setf design (make-design "Foobar 2000" ship-type))))
          (t
           (unless schematic-transition
             (setf schematic-transition (make-instance 'in-and-out :rate +designer-fade-rate+)))
           (run-in-and-out schematic-transition (uic-delta-t uic))

           (let ((simg (img (ship-asset-name ship-type "schem-low.png")))
                 (fade-level (level-of schematic-transition))
                 (fade-delay 0.00)
                 (cx (truncate (uic-width uic) 2))
                 (cy (truncate (uic-height uic) 2)))
             (draw-img-deluxe simg cx cy
                              (color-with-alpha
                               (lighter-color)
                               (f->b (subfoo fade-level fade-delay))))
             (loop for slot in (slots-of ship-type)
                   for tech across (design-tech-slots design)
                   as layout = (ensure-hardpoint-layout ship-type slot)
                   do
                   (cond 
                     (layout (designer-present-slot slot tech layout))
                     (t #+NIL (warn "Missing layout for slot ~A of ~A" slot ship-type))))
             )))

        (when (released? uic +right+)
          (let ((close-rate (* 3.0 +designer-fade-rate+)))
            (io-request-close background-transition close-rate)
            (when schematic-transition
              (io-request-close schematic-transition close-rate))))
        (when (eql transition :closed)
          (pop-gadget gadget))))))

(defmethod gadget-key-pressed ((gadget designer) uic keysym char)
  (declare (ignore char gadget))
  (when (and (not (zerop (logand +alt-mask+ (uic-modifiers-pressed uic))))
             *devmode*
             (eql keysym (keysym :E)))
    (activate-new-gadget (make-instance 'hardpoint-editor))))

(defclass hardpoint-editor (gadget)
  ((editing-slot :initform nil)         ; The hardpoint.
   (layout :initform nil)
   (constraint :initform 0)
   (apply-constraint? :initform nil)
   (label-cache :initform nil)))

(defun get-hardpoint-editor-ship-type (gadget)
  (let ((design (slot-value (next-gadget gadget) 'design)))
    (and design (design-type design))))

(defmethod gadget-key-pressed ((gadget hardpoint-editor) uic keysym char)
  (declare (ignore uic))
  (when (eql keysym (keysym :escape))
    (pop-gadget gadget))
  (with-slots (editing-slot layout constraint apply-constraint?) gadget
    ;; Toggle angle constraint?
    (when (eql keysym (keysym :tab))
      (setf apply-constraint? (not apply-constraint?)))
    ;; Adjust angle constraint?
    (when (eql keysym (keysym :q)) (decf constraint 15))
    (when (eql keysym (keysym :w)) (decf constraint 5))
    (when (eql keysym (keysym :e)) (incf constraint 5))
    (when (eql keysym (keysym :r)) (incf constraint 15))

    ;; Save changes.
    (when (eql keysym (keysym :return))
      (save-hardpoint-layout (get-hardpoint-editor-ship-type gadget) editing-slot layout)
      (setf editing-slot nil))

    ;; Select hardpoint?
    (let* ((n (position char "1234567890"))
           (slots (and.. (get-hardpoint-editor-ship-type gadget) (slots-of $))))
      (when (and n (< n (length slots)))
        (setf editing-slot (elt slots n)
              layout (reset-hardpoint-layout editing-slot))))))

(defun hed/pointer-to-coordinate (uic)
  ;; FIXME/TODO: Correct for scaling.
  (v2 (uic-mx uic) (uic-my uic)))

(defmethod gadget-run ((gadget hardpoint-editor) uic)
  (call-next-method gadget (child-uic uic :active nil))
  (let* ((hardpoints (slots-of (get-hardpoint-editor-ship-type gadget)))
         (label-string (format nil "Hardpoint editor: Press 1..~A" (length hardpoints))))
    (when (null hardpoints)
      (format t "Can't edit when there's no design!")
      (pop-gadget gadget))

    (with-slots (editing-slot layout label-cache constraint apply-constraint?) gadget
      (when editing-slot
        (setf label-string
              (format nil "~A (~D,~D) ~:[~*~;Constrained: ~D degrees~]"
                      (name-of editing-slot) (uic-mx uic) (uic-my uic)
                      apply-constraint? constraint))
        
        (symbol-macrolet ((points (hardpoint-layout-line-points layout)))
          (cond
            ((and apply-constraint? (second points))
             (setf (first points) (v2+ (second points)
                                       (v2angle (degrees->radians constraint) 
                                                (v2len (v2- (hed/pointer-to-coordinate uic)
                                                            (second points)))))))
            (t
             (setf (first points) (hed/pointer-to-coordinate uic))))

          (when (second points)
            (setf (hardpoint-layout-label-base layout)
                  (v2 (+ 10 (reduce #'min points :key #'v2.x :end 2))
                      (+ -6 (v2.y (first points))))))

          (when (and (clicked? uic +right+) (second points))
            (pop points))

          (when (clicked? uic +left+)
            (push (hed/pointer-to-coordinate uic) points))))
      (draw-img (cachef (label-cache label-string :delete free-img)
                  (render-label gadget :sans 13 label-string))
                5 (- (uic-height uic) 8)))
    ))