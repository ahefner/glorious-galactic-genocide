;;;; Ship Design UI

;;; * The designer runs unreasonably slow (roughly half the frame
;;;   rate of the star map). So far as I know, this is due to the
;;;   code that draws the curvy lines (and quite likely the code
;;;   drawing the polylines underlying it) being unreasonably
;;;   inefficient. This sucks, but it isn't quite slow enough for me
;;;   to care. It'd probably piss me off on an Atom netbook..

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
   (pointer-slot :initform nil :accessor pointer-slot-of)
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

(defstruct hardpoint-layout line-points label-base)

(with-vars ((layouts (make-hash-table)))
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

(defun designer-choose-scale (width height)
  (if (and (>= width 1200) (>= height 720))
      1.5
      1))

(defun designer-transform-point (uic ship-type v)
  (multiple-value-bind (scale offset) (designer-scale-and-offset uic ship-type)
    (let ((x (v2.x v)) (y (v2.y v)))
      (v2 (cond
            ((<= x -1000) 0)
            ((>= x  1000) (uic-width uic))
            (t (round (+ (v2.x offset) (* scale x)))))
          (round (+ (v2.y offset) (* scale y)))))))

(defun designer-untransform-point (uic ship-type v)
  (multiple-value-bind (scale offset) (designer-scale-and-offset uic ship-type)
    (v2 (cond ((< (v2.x v) 8) -1000)
              ((>= (v2.x v) (- (uic-width uic) 8)) 1000)
              (t (round (/ (- (v2.x v) (v2.x offset)) scale))))
        (round (/ (- (v2.y v) (v2.y offset)) scale)))))

(defun designer-compute-label-point (points)
  (and (second points)
       (v2 (+ 8 (reduce #'min points :key #'v2.x :end 2))
           (+ -6 (v2.y (first points))))))

;;; Present slot (draw flexiline and labels). Returns whether the
;;; pointer is in the slot or not.  Unlike most of these, doesn't
;;; implement any pointer interaction other than the obvious rectangle
;;; test. That's left to the caller.

(defun designer-present-slot (slot tech layout uic ship-type &key transition editable)
  (flet ((transform (v) (designer-transform-point uic ship-type v)))
    (let* ((line-points (hardpoint-layout-line-points layout))           
           (transformed-points (mapcar #'transform line-points))
           (fade-window (* 0.20 (uic-height uic)))
           (fade-max (float (uic-height uic) 0.0f0))
           (alpha1
            (f->b
             (if (or (null line-points) (not transition)) 1.0
                 (unit-ramp-in (v2.y (first transformed-points))
                               (+ fade-window (v2.y (first transformed-points)))
                               (* fade-max (/ 0.8) (clamp transition 0.0 0.8))))))
           (alpha2
            (f->b
             (if (or (null line-points) (not transition)) 1.0
                 (unit-ramp-in (v2.y (first transformed-points))
                               (+ fade-window (v2.y (first transformed-points)))
                               (* fade-max (/ 0.8) (clamp (- transition 0.2) 0.0 0.8))))))
           (upper-color (color-with-alpha (lighter-color) alpha1))
           (line-color (vector 255 255 255 alpha1))
           (text-color (vector 255 255 255 alpha2))
           (lp (designer-compute-label-point transformed-points))
           (face :gothic)
           (cursor (and lp (make-cursor :left (v2.x lp) :y (v2.y lp))))
           (label-highlight-padding 10)
           (pointer-inside nil)
           (size 16)
           (space 3)
           (amount (and tech 1))
           (amount-label (and amount (global-label face 16 (format nil "~:D" amount))))
           (slot-label (global-label face size (name-of (or tech slot)))))
      (when (>= (length line-points) 2)
        (draw-flexiline transformed-points :color line-color))
      (when cursor
        ;; If editable (and transition complete), draw highlight.
        (when (and editable (>= alpha1 254))
          (let ((left (v2.x lp))
                (right (+ (v2.x lp) 
                          (img-width slot-label)
                          (if (and amount-label (> amount 1))
                              (+ space (img-width amount-label))
                              0))))
            (draw-bar* (img "designer/below-slot-left.png")
                       (img "designer/below-slot-right.png")
                       #(0 0 0 174)
                       (- left label-highlight-padding)
                       (- (v2.y lp) 16)
                       (+ (- right left) (* 2 label-highlight-padding)))))
        ;; Draw the amount label and slot name.
        (when (and amount-label (> amount 1))
          (cursor-draw-img cursor amount-label upper-color)
          (cursor-advance cursor space))
        (cursor-draw-img cursor slot-label upper-color)
        ;; Compare pointer position to slot bounds
        (when (pointer-in-rect* uic
                                (v2.x lp) (- (v2.y lp) (img-ascent slot-label))
                                (cursor-x cursor) (+ (v2.y lp) (img-descent slot-label)))
          (setf pointer-inside t))
        ;; Draw tech description or empty slot message
        (flet ((draw (img) 
                 (draw-img-deluxe img (v2.x lp) (+ (v2.y lp) 20) text-color)))
          (cond 
            ((and amount (designer-description-of tech))
             (draw (global-label :sans 11 (designer-description-of tech))))
            ((null amount)
             (draw (global-label :sans 11 (empty-string-of slot))))))
        ;; Return whether or not the pointer is over the slot title
        (values pointer-inside)))))

(defun designer-scale-and-offset (uic ship-type)
  (let* ((scale (designer-choose-scale (uic-width uic) (uic-height uic)))
         (adjust (v2 (truncate (- (uic-width uic) (* 800 scale)) 2)
                     (truncate (- (uic-height uic) (* 480 scale)) 2)))
         (schem-offset (v2+ (v2scale (schematic-position-of ship-type) scale)
                            adjust)))
    (values scale schem-offset)))

(defvar *designer-label-cache* (make-hash-table :test 'eq))

(defun designer/edit-name (designer uic design cx cy)
  (declare (ignore designer))
  (let ((gadget (make-instance 'designer-line-editor 
                               :led (led (name-of design) (length (name-of design)))
                               :field-id :name
                               :insert-filter (editor-limit-string-length 13)
                               :screen-vector (v2 cx cy)
                               :face :gothic :size 14
                               :accept-fn (lambda (newname)
                                            ;; MAJOR FIXME/TODO: Validate name.
                                            (setf (name-of design) (string-trim " " newname))))))
    (line-editor/move-cursor-to-pointer gadget uic)
    (activate-new-gadget (make-instance 'dim-background-gadget))
    (activate-new-gadget gadget)))

(defun designer/currently-editing? (field-id)
  (and (typep *gadget-root* 'designer-line-editor)
       (eql field-id (field-id-of *gadget-root*))))

(defun designer/cancel (designer uic design cx cy)
  (declare (ignore uic design cx cy))
  (designer/request-close designer))

(defparameter *designer-overlay-fields*
  (list
   (list :name    '( 77 .   2) '(256 .  21) :display 'name-of :highlight "designer-name-highlight.png" :hx 0 :hy 0 :editor 'designer/edit-name :edit-hides-label t)
   (list :type    '( 32 .  23) '(183 .  44) :display (lambda (d) (name-of (design-type d))))
   (list :model   '(222 .  23) '(258 .  44) :display (lambda (d) (format nil "~@R" (model-number-of d))) :face :italic)
   (list :length  '( 48 .  46) '(126 .  65) :display (lambda (d) (format nil "~:D m" (length-meters-of (design-type d)))))
   (list :mass    '(162 .  46) '(258 .  65) :display (lambda (d) (format nil "~:D tonnes" (weight-of d))))
   (list :beamdef '( 26 .  67) '( 44 .  86) :display 'beam-defense-of)
   (list :targacc '( 69 .  67) '( 87 .  86) :display 'target-acc-of)
   (list :ecm     '(108 .  67) '(127 .  86) :display 'ecm-level-of)
   (list :thrust  '(163 .  67) '(188 .  86) :display 'thrust-of)
   (list :speed   '(219 .  67) '(258 .  86) :display (lambda (d) (format nil "~,1F c" (* 0.5 (speed-of d)))))
   (list :date    '( 32 .  89) '(126 . 108) :display 'design-date-of)
   (list :cost    '(160 .  89) '(258 . 108) :display (lambda (d) (format nil "~:D GC" (cost-of d))))
   (list :approve '(  2 . 110) '(126 . 134) :button t :highlight "designer-button-approve.png" :hx 0 :hy 108)
   (list :cancel  '(128 . 110) '(258 . 134) :button t :highlight "designer-button-cancel.png" :hx 0 :hy 108 :editor 'designer/cancel)))

(defun designer-draw-overlay (designer uic design transition &key editable)
  (let* ((bx (- (uic-width  uic) 263))
         (by (- (uic-height uic) 140))
         (mv (uic-mouse-vector uic))
         (alpha (f->b (* transition 2.0)))
         (color (color-with-alpha (color-darken (label-color)) alpha))
         (white (vector 255 255 255 alpha))
         (base (v2 bx by)))
    ;; 
    (draw-img-deluxe (imgblock :designer-overlay) bx by (vector 255 255 255 alpha))
    ;; Overlay regions
    (loop for entry in *designer-overlay-fields* do
          (destructuring-bind (key umin umax &key 
                                   highlight hx hy 
                                   (display (constantly nil))
                                   (face :gothic)
                                   (height 14)
                                   (editor nil)
                                   (edit-hides-label nil)
                                   (button nil))
              entry
            (let* ((min (v2+ base umin))
                   (max (v2+ base umax))
                   (cx (+ (v2.x min) (ash (- (v2.x max) (v2.x min)) -1)))
                   (cy (round (+ (v2.y min) (* 0.72 (- (v2.y max) (v2.y min))))))
                   (string (and.. (funcall display design) (princ-to-string $))))
              (when (and string (not (and (designer/currently-editing? key) edit-hides-label)))
                (draw-img-deluxe
                 (cachef ((gethash key *designer-label-cache*) string :delete free-img)
                   (render-label :designer face height string :align-x :center))
                 cx cy white))
              (cond
                ((not (and (v2<= min mv) (v2<= mv max))) (values))
                ((and (not editable) (not button)) (values))
                ((not editor) (values))
                ((designer/currently-editing? key) (values))
                (highlight
                 (set-blend-mode :add)
                 (draw-img-deluxe (imgblock highlight)
                                  (+ (or hx (v2.x min)) bx)
                                  (+ (or hy (v2.y min)) by)
                                  color)
                 (set-blend-mode :blend)
                 (when (released? uic)
                   (funcall editor designer uic design cx cy)))
                (t (fill-rect min max color))))))))

(defmethod gadget-run ((gadget designer) uic)
  (with-slots (player ship-type design background-transition schematic-transition pointer-slot) gadget
    (multiple-value-bind (level transition state)
        (run-in-and-out background-transition (uic-delta-t uic))

      ;; Deactivate UIC during fades. No good letting the
      ;; player start editing things while the screen is fading out.
      (unless (eq t state)
        (setf uic (child-uic uic :active nil)))

      ;; We might inspect another player's design with this. Rebinding
      ;; *player* makes things simpler.
      (let ((*player* player)
            (editable? t))
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
        (when design (setf ship-type (design-type design)))

        ;; Have we a ship-type?
        (cond
          ;; No? Ask for one.
          ((not ship-type)
           (setf ship-type (prompt-for-design uic))
           (when (and ship-type (not design))
             (setf design (make-design "Foobar 2000" ship-type))))
          ;; Yes? Show it and allow editing.
          (t
           (unless schematic-transition
             (setf schematic-transition (make-instance 'in-and-out :rate (* 0.5 +designer-fade-rate+))))
           (run-in-and-out schematic-transition (uic-delta-t uic))

           (multiple-value-bind (scale offset) (designer-scale-and-offset uic ship-type)
             (let ((simg (imgblock (ship-asset-name ship-type (if (eql scale 1)
                                                                  "schem-low.png"
                                                                  "schem-mid.png"))))
                   (fade-level (level-of schematic-transition))
                   (fade-delay 0.00))
               ;; Schematic.
               (draw-img-deluxe simg (v2.x offset) (v2.y offset)
                                (color-with-alpha
                                 (lighter-color)
                                 (f->b (bias-unit fade-level fade-delay))))
               ;; Slots.
               (loop with new-pointer-slot = nil
                     for slot in (slots-of ship-type)
                     for tech across (design-tech-slots design)
                     as layout = (ensure-hardpoint-layout ship-type slot)
                     do
                     (cond
                       (layout 
                        (when (designer-present-slot
                               slot tech layout uic ship-type
                               :editable (and editable? (eq pointer-slot slot))
                               :transition fade-level)
                          ;; Pointer is in the slot:
                          (when (and editable? (clicked? uic))
                            (printl :edit (name-of slot) slot))
                          (setf new-pointer-slot slot)))
                       (t (warn "Missing layout for slot ~A of ~A" slot ship-type)))
                     finally
                     (setf pointer-slot new-pointer-slot))
               ;; Stats.
               (designer-draw-overlay gadget uic design level :editable editable?)))))
        
        ;; Close/fade out behavior
        (when (released? uic +right+) (designer/request-close gadget))
        (when (eql transition :closed)
          (exit-gadget gadget))))))

(defun designer/request-close (designer)
  (with-slots (background-transition schematic-transition) designer
    (let ((close-rate (* 3.0 +designer-fade-rate+)))
      (io-request-close background-transition close-rate)
      (when schematic-transition
        (io-request-close schematic-transition close-rate)))))

(defmethod gadget-key-pressed ((gadget designer) uic keysym char)
  (declare (ignore char))
  (cond
    ((eql keysym (keysym :escape))
     (designer/request-close gadget))
    ((and (not (zerop (logand +alt-mask+ (uic-modifiers-pressed uic))))
          *devmode*
          (eql keysym (keysym :E)))
     (activate-new-gadget (make-instance 'hardpoint-editor)))))

;;;; ----------------------------------------------------------------------

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
    (exit-gadget gadget))
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

(defun hed/pointer-to-coordinate (uic ship-type)
  (designer-untransform-point uic ship-type (v2 (uic-mx uic) (uic-my uic))))

(defmethod gadget-run ((gadget hardpoint-editor) uic)
  (call-next-method gadget (child-uic uic :active nil))
  (let* ((ship-type (get-hardpoint-editor-ship-type gadget))
         (v (hed/pointer-to-coordinate uic ship-type))
         (hardpoints (slots-of ship-type))
         (label-string (format nil "Hardpoint editor: Press 1..~A" (length hardpoints))))
    (when (null hardpoints)
      (format t "Can't edit when there's no design!")
      (exit-gadget gadget))

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
                                                (v2len (v2- v
                                                            (second points)))))))
            (t (setf (first points) v)))

          (when (and (clicked? uic +right+) (second points))
            (pop points))

          (when (clicked? uic +left+)
            (push v points))))
      (draw-img (cachef (label-cache label-string :delete free-img)
                  (render-label gadget :sans 13 label-string))
                5 (- (uic-height uic) 8))) ))
