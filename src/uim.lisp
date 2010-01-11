(in-package :g1)

(ffi:clines "#include \"sys.h\"")

(defun child-uic (uic dx dy &key width height (active t))
  (let ((new (copy-uic uic)))
    (setf (uic-active new) (and active (uic-active uic))) ; Don't override deactivation by a parent.
    (incf (uic-abx new) dx)
    (incf (uic-aby new) dy)
    (decf (uic-mx  new) dx)
    (decf (uic-my  new) dy)
    (when width  (setf (uic-width  new) width))
    (when height (setf (uic-height new) height))
    new))

(defun reparent-gadget (gadget new-parent)
  (setf (next-gadget (parent-gadget gadget)) new-parent
        (parent-gadget new-parent) (parent-gadget gadget)
        (next-gadget new-parent) gadget
        (parent-gadget gadget) new-parent))

(defgeneric gadget-key-pressed (gadget uic keysym char)
  (:method (gadget uic keysym char)
    (declare (ignore gadget uic keysym char))))

(defgeneric gadget-key-released (gadget uic keysym)
  (:method (gadget uic keysym)
    (declare (ignore gadget uic keysym))))

(defgeneric gadget-paint (gadget uic)
  (:method ((gadget null) uic)
    (declare (ignore gadget uic)))
  (:method ((gadget gadget) uic)
    (gadget-paint (next-gadget gadget) uic)))

(defun reactivate-gadget (gadget)
  (setf *gadget-root* gadget))

(defun activate-new-gadget (gadget)
  (setf (next-gadget gadget) *gadget-root* 
        *gadget-root* gadget))

(defun pop-gadget (gadget)
  (assert (not (null (next-gadget gadget))))
  (reactivate-gadget (next-gadget gadget)))

(defun update-modifier-masks (uic last-uic)
  (let ((now (uic-modifiers uic))
        (then (uic-modifiers last-uic)))
    (setf (uic-modifiers-pressed  uic) (logand now  (logxor now then))
          (uic-modifiers-released uic) (logand then (logxor now then)))))

(defun gettime ()
  (call :unsigned-int "usectime"))

;;;; Mouse grabbing semantics: Grabbing is only intended to last the
;;;; duration that the left button is held. Once you've grabbed the
;;;; mouse, the global UIC is deactivated until the grab (or the
;;;; button) is released.
(defun grab-mouse (grab-id)
  (cond
    (*grab-id* (format *trace-output* "Attempt to grab pointer by ~A, but already grabbed by ~A" grab-id *grab-id*))
    (t (setf *grab-id* grab-id))))

(defun release-mouse (grab-id)
  (unless (eql *grab-id* grab-id)
    (format *trace-output* "Release grab attempt by ~A, but *grab-id* is ~A~%" grab-id *grab-id*))
  (setf *grab-id* nil))

(defun initial-uic ()
  (make-uic :abx 0 :aby 0 
            :width (cx :int "window_width") :height (cx :int "window_height")
            :amx 0 :amy 0 :mx 0 :my 0                                  
            :buttons 0   :buttons-pressed 0   :buttons-released 0
            :modifiers 0 :modifiers-pressed 0 :modifiers-released 0
            :active t
            :time (gettime) :delta-t 0.0))

(defvar *swank-running* nil)

(defun start-swank (background-p)
  (unless *swank-running*
    (setf *swank-running* t)   
    (require :asdf)
    (eval (read-from-string 
           "(push '(MERGE-PATHNAMES \".sbcl/systems/\" (USER-HOMEDIR-PATHNAME)) asdf:*central-registry*)"))
    (eval (read-from-string "(asdf:oos 'asdf:load-op :swank)"))
    #+NIL
    (flet ((run () (eval (read-from-string "(swank:create-server :port 0)"))))
      (if background-p (mp:process-run-function 'swank-process #'run) (run)))))


(defun uim-sdl-run ()
  (loop named runloop 
        with *grab-id* = nil
        with please-set-video-mode = nil
        with last-uic = (initial-uic)
        as *presentation-stack* = nil
        as uic = (copy-uic last-uic)
        as gadget = *gadget-root*
        do #| Gather events and build new UIC |#
        (setf (uic-width uic)  (cx :int "window_width")
              (uic-height uic) (cx :int "window_height")
              (uic-buttons-pressed uic) 0
              (uic-buttons-released uic) 0
              (uic-buttons uic) (c :int "(int)SDL_GetMouseState(NULL, NULL)")
              (uic-active uic) (not *grab-id*)
              (uic-time uic) (gettime)              
              ;; The clamp below implies that if your machine can't
              ;; maintain 10 fps, the game will start to slow down.  I
              ;; I want to keep the maximum delta-t in a reasonable
              ;; range, so the rest of the code doesn't have to
              ;; accomodate freak occurences like a multisecond pause.
              (uic-delta-t uic) (clamp (/ (logand (- (uic-time uic) (uic-time last-uic))
                                                  #xFFFFFFFF)
                                          1000000.0f0)
                                       0.0f0 0.1f0))

        (loop as pending = (c :int "SDL_PollEvent(&cur_event)")
              as type = (cx :int "cur_event.type")
              until (zerop pending)
              do
              (cond
                ((eql type (cx :int "SDL_QUIT"))
                 (return-from runloop))

                ((eql type (cx :int "SDL_VIDEORESIZE"))
                 (c "window_width = cur_event.resize.w")
                 (c "window_height = cur_event.resize.h")
                 (setf please-set-video-mode t)
                 (sleep 0.1))           ; Stupid Linux/X11 hack.
                
                ((eql type (cx :int "SDL_MOUSEMOTION"))
                 (setf (uic-amx uic) (cx :int "cur_event.motion.x")
                       (uic-mx  uic) (cx :int "cur_event.motion.x")
                       (uic-amy uic) (cx :int "cur_event.motion.y")
                       (uic-my  uic) (cx :int "cur_event.motion.y")))
                
                ((eql type (cx :int "SDL_KEYDOWN"))
                 (setf (uic-modifiers uic) (cx :int "cur_event.key.keysym.mod"))
                 (update-modifier-masks uic last-uic)
                 (let ((char (ignore-errors (code-char (cx :int "(int)cur_event.key.keysym.unicode")))))
                   (when (and *devmode* (not (zerop (logand +alt-mask+ (uic-modifiers uic)))))
                     (when (and (eql char #\r))
                       (reload-modified-sources))
                     (when (and (eql char #\s))
                       (start-swank (not (zerop (logand +control-mask+ (uic-modifiers uic)))))))
                   (gadget-key-pressed gadget uic
                                       (cx :int "(int)cur_event.key.keysym.sym")
                                       (and (char/= char #\Nul) char))))

                ((eql type (cx :int "SDL_KEYUP"))
                 (setf (uic-modifiers uic) (cx :int "cur_event.key.keysym.mod"))
                 (update-modifier-masks uic last-uic)
                 (gadget-key-released gadget uic (cx :int "(int)cur_event.key.keysym.sym")))
                
                ((eql type (cx :int "SDL_MOUSEBUTTONDOWN"))
                 (logiorf (uic-buttons-pressed uic) (ash 1 (1- (cx :int "cur_event.button.button")))))
                
                ((eql type (cx :int "SDL_MOUSEBUTTONUP"))
                 (logiorf (uic-buttons-released uic) (ash 1 (1- (cx :int "cur_event.button.button")))))
                
                #+NIL
                ((eql type (cx :int "SDL_VIDEOEXPOSE"))
                 (repaint))))
        (when please-set-video-mode
          (c "sys_setvideomode()")
          (setf please-set-video-mode nil))
        (when (released? uic +left+) (setf *grab-id* nil))
        (repaint uic)
        (setf last-uic uic)))


;;;; Helper functions - Clicked/Released are edge-triggered and the
;;;; mask is a disjunction. Held mask is a conjunction.

;;;; Would it be helpful for clicked/released (and maybe held) to check if the UIC is active? Probably..

(defun clicked? (uic &optional (button-mask +left+))
  (not (zerop (logand (uic-buttons-pressed uic) button-mask))))

(defun released? (uic &optional (button-mask +left+))
  (not (zerop (logand (uic-buttons-released uic) button-mask))))

(defun held? (uic button-mask)
  (= button-mask (logand (uic-buttons uic) button-mask)))

(defun no-modifiers (uic)
  (zerop (logand (uic-modifiers-pressed uic) (logior +alt-mask+ +control-mask+))))

(defun pointer-normsq* (uic x y) (+ (square (- x (uic-mx uic))) (square (- y (uic-my uic)))))
(defun pointer-in-radius* (uic radius x y) (<= (pointer-normsq* uic x y) (square radius)))
  

;;;; Presentations

(defun push-new-presentation (object type children)
  (push (make-presentation :object object :type (or type object) :children children)
        *presentation-stack*))


;;;; Gadgetry - Image Button

(defun run-img-button (uic img-up img-down x y &key (clicked-sound :click-mid))
  (let ((in (and (uic-active uic) (pointer-in-img-rect uic img-up x y))))
    (draw-img (if (and in (held? uic +left+)) img-down img-up) x y)
    (and in (released? uic +left+) (prog1 t (and clicked-sound (play-sound clicked-sound))))))

;;;; Gadgetry - Labelled Button

(defun labelled-button-rect (button-style label x y &key min-width (center-x t))
  (let* ((style (button-style-released button-style))
         (label-width (max (or min-width 0) (if label (img-width label) 0)))
         (bar-width (+ label-width (bar-style-width style)))
         (lx (if center-x (- x (ash bar-width -1)) x)))
  (values lx y (+ lx bar-width) (+ y (bar-style-height style)))))

(defun run-labelled-button (uic label x y &key min-width (center-x t) (color (vector 255 255 255)) (style *button-a*)
                            (clicked-sound (sound-effect :click-high)))
  (let ((in (and (uic-active uic)
                 (multiple-value-call #'pointer-in-rect* uic
                   (labelled-button-rect style label x y :min-width min-width :center-x center-x)))))
    (draw-button style label (and in (held? uic +left+)) x y :min-width min-width :center-x center-x :color color)
    (and in (released? uic +left+) 
         (prog1 t (and clicked-sound (play-sound clicked-sound))))))

;;;; Gadgetry - Slider

(defun run-slider (id uic x y value range &optional disable)
  (let ((fill (min 160 (round (* 160 (if (zerop range) 0 (/ value range))))))
        (in (pointer-in-rect* uic x y (+ x 160) (+ y 17))))
    (bind-texobj *slider160*)
    (draw-tile x y (+ x fill) (+ y 17) 0 0 (if disable #(150 150 150) #(255 255 255)))
    (draw-tile (+ x fill) y (+ x 160) (+ y 17) fill 0 (if disable #(100 100 100) #(190 190 190)))
    (when (and id in (clicked? uic +left+)) (grab-mouse id))
    (if (or (eq id *grab-id*)
            (and (uic-active uic) in (held? uic +left+)))
        (round (* range (/ (clamp (- (uic-mx uic) x) 0 160) 160)))
        value)))

;;;; Panels

(defgeneric dismiss-panel (panel)
  (:method :before (panel) (setf (closing-p panel) t))
  (:method (panel) (declare (ignore panel)) (values)))

(defun draw-panel-background (uic bottom)
  (let* ((left (img :panel-left))
         (right (img :panel-right))
         (edge-top (- bottom (img-height left))))
    (draw-bar* left right *panel-fill* 0 edge-top (uic-width uic))
    (fill-rect 0 0 (uic-width uic) edge-top 7 7 7 244)))

(defun run-hosted-panel (uic host bottom)
  (let* ((pointer-in-host (< (uic-my uic) bottom))
         (child-uic (child-uic uic 0 0 :active (not pointer-in-host))))
    (with-slots (panel panel-y closing-panel) host
      (cond 
        (panel
         (let* ((target (if closing-panel 0 (+ bottom (panel-height panel))))
                (dist (- target panel-y))
                (rate (* 10 (if (< dist 0)
                                (min -1 (- (floor (* (uic-delta-t uic) (sqrt (- dist))))))
                                (max  1 (ceiling  (* (uic-delta-t uic) (sqrt dist))))))))
           (setf panel-y (clamp (+ panel-y rate) 0 (+ bottom (panel-height panel))))
           (when closing-panel (setf (uic-active child-uic) nil))
           (run-panel panel child-uic panel-y)
           (when (and closing-panel (<= panel-y bottom))
             (finalize-object panel)
             (setf panel nil
                   panel-y 0
                   closing-panel nil))))
        (t (gadget-paint (next-gadget host) child-uic))))))

(defun close-panels (&optional (host *gameui*))
  (with-slots (panel closing-panel) host
    (when panel 
      ;; Tricky: This might be called when the panel is already
      ;; closing, even every frame, so only play the sound once.
      (unless closing-panel (play-sound :close1))
      (dismiss-panel panel)
      (setf closing-panel t))))

(defun activate-panel (new-panel &key (host *gameui*) (update-p nil))
  (with-slots (panel closing-panel) host
    (when panel 
      (setf (panel-y-of host) (+ (- (panel-y-of host) (panel-height panel))
                                 (panel-height new-panel)))
      (finalize-object panel))
    (unless update-p
      (play-sound (if panel :click-high :open1)))
    (setf panel new-panel
          (host-of panel) host
          closing-panel nil)))

(defun update-panel  (new-panel &key (host *gameui*))
  (activate-panel new-panel :host host :update-p t))

;;;; Cursor Layout Utility

(defun cursor-draw-img (cursor img &optional (color (cursor-color cursor)))
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

(defun cursor-advance (cursor advance)
  (unless (cursor-x cursor)
    (setf (cursor-x cursor) (cursor-left cursor)))
  (setf (cursor-newline-p cursor) nil)
  (incf (cursor-x cursor) advance))
