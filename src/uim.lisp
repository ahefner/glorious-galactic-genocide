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
    (declare (ignore gadget uic keysym char))))

(defgeneric gadget-paint (gadget uic)
  (:method ((gadget null) uic)
    (declare (ignore gadget uic)))
  (:method ((gadget gadget) uic)
    (gadget-paint (next-gadget gadget) uic)))

(defun activate-gadget (gadget)
  (setf *gadget-root* gadget))

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


;;;; Helper functions

(defun clicked? (uic button-mask)
  (= button-mask (logand (uic-buttons-pressed uic) button-mask)))

(defun released? (uic button-mask)
  (= button-mask (logand (uic-buttons-released uic) button-mask)))

(defun held? (uic button-mask)
  (= button-mask (logand (uic-buttons uic) button-mask)))

(defun pointer-normsq* (uic x y) (+ (square (- x (uic-mx uic))) (square (- y (uic-my uic)))))
(defun pointer-in-radius* (uic radius x y) (<= (pointer-normsq* uic x y) (square radius)))
  

;;;; Presentations

(defun push-new-presentation (object type children)
  (push (make-presentation :object object :type (or type object) :children children)
        *presentation-stack*))


;;;; Gadgetry - Buttons

(defun run-img-button (uic img-up img-down x y)
  (let ((in (and (uic-active uic) (pointer-in-img-rect uic img-up x y))))
    (draw-img (if (and in (held? uic +left+)) img-down img-up) x y)
    (and in (released? uic +left+))))

(defun labelled-button-rect (button-style label x y &key min-width (center-x t))
  (let* ((style (button-style-released button-style))
         (label-width (max (or min-width 0) (if label (img-width label) 0)))
         (bar-width (+ label-width (bar-style-width style)))
         (lx (if center-x (- x (ash bar-width -1)) x)))
  (values lx y (+ lx bar-width) (+ y (bar-style-height style)))))

(defun run-labelled-button (uic label x y &key min-width (center-x t) (color (vector 255 255 255)) (style *button-a*))
  (let ((in (and (uic-active uic)
                 (multiple-value-call #'pointer-in-rect* uic
                   (labelled-button-rect style label x y :min-width min-width :center-x center-x)))))
    (draw-button style label (and in (held? uic +left+)) x y :min-width min-width :center-x center-x :color color)
    (and in (released? uic +left+))))

;;;; Gadgetry - Sliders

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

