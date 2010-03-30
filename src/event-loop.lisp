;;;; Separate event loop from the rest of the UI code, because it's
;;;; coupled to some statically linked C symbols and thus can't be
;;;; hotloaded, whereas the bulk of the UI code has no such problem.

(in-package :g1)

;;(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0)))

(ffi:clines "#include \"sys.h\"
static SDL_Event cur_event;
")

(defun initial-uic ()
  (make-uic :abx 0 :aby 0 
            :width (cx :int "window_width") :height (cx :int "window_height")
            :amx 0 :amy 0 :mx 0 :my 0                                  
            :buttons 0   :buttons-pressed 0   :buttons-released 0
            :modifiers 0 :modifiers-pressed 0 :modifiers-released 0
            :active t
            :time (gettime) :delta-t 0.0))

(defun gettime ()
  (call :unsigned-int "usectime"))

(defun uim-sdl-run () 
  (loop named runloop 
        with *grab-id* = nil
        with please-set-video-mode = nil
        with last-uic = (initial-uic)
        with idle = nil
        with key-repeat = nil
        as *presentation-stack* = nil
        as uic = (copy-uic last-uic)
        as gadget = *gadget-root*
        as key-repeat-required = (not (not (requires-key-repeat? gadget)))
        do

        #| Enable/Disable key repeat mode according to preference of root gadget: |#
        (unless (eql key-repeat key-repeat-required)
          (setf key-repeat key-repeat-required)
          (if key-repeat
              (c "SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL)")
              (c "SDL_EnableKeyRepeat(0, SDL_DEFAULT_REPEAT_INTERVAL)")))

        #| If not visible, idle. If idling, block waiting for an event. |#
        (when (zerop (c :int "SDL_GetAppState() & SDL_APPACTIVE"))
          (printl :idle)
          (setf idle t))
;;; ISSUE: A problem for netplay, where we don't want to block
;;; without polling the socket...
        (when idle
          (c "SDL_WaitEvent(NULL)"))

        #| Gather events and build new UIC |#

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

        (tagbody 
         top
           (restart-case 
               (progn
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
                          #+linux (sleep 0.1)) ; Stupid Linux/X11 kludge.
                
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
                              ;; Alt-r: Reload sources
                              (when (and (eql char #\r)) (reload-modified-sources))
                              ;; Alt-s: Start swank server (background)
                              (when (and (eql char #\s)) (start-swank :background-p t)))
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

                 ;; UIC is never active during a grab, hence '%released?', not 'released?'
                 (when (%released? uic +left+) (setf *grab-id* nil))

                 (repaint uic)
                 ;; We should have some way of asking the gadgets if they are idle. We don't, so unidle:
                 (setf idle nil)
                 ;;(incf *total-frames*)
                 
                 (setf last-uic uic))
             (retry ()
               :report (lambda (stream) (format stream "Retry UI loop"))
               (go top))
             )))
)