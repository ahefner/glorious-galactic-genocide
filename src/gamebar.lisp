;;;; Game status bar and associated menus

(in-package :g1)

(defclass gameui (gadget panel-host-mixin)
  (starmap))

(defmethod panel-height ((gadget gameui))
  (declare (ignore gadget))
  ;; This is perverse. Gameui isn't a panel!
  (img-height (img :gamebar-left)))

(defmethod gadget-key-pressed ((gadget gameui) uic keysym char)
  (with-slots (starmap panel closing-panel) gadget
    (cond      
      ((and (eql char #\Space) (no-modifiers uic))
       (client-do-next-turn gadget))
      ((and panel (not closing-panel)) (gadget-key-pressed panel uic keysym char))
      (t (gadget-key-pressed starmap uic keysym char)))))

(defmethod gadget-key-released ((gadget gameui) uic keysym)
  (with-slots (starmap panel closing-panel) gadget
    (cond ((and panel (not closing-panel)) (gadget-key-released panel uic keysym))
          (t (gadget-key-released starmap uic keysym)))))

(defun create-gameui (universe)
  (let* ((gameui (make-instance 'gameui))
         (homeworld (aref (colonies *player*) 0))
         (loc (loc homeworld))
         (camera (vec (v.x loc) (v.y loc) 0.0f0)))
    (prog1 gameui
      (with-slots (starmap next-gadget) gameui
        (setf starmap (make-instance 'debug-starmap :universe universe
                                     :camera camera
                                     :camera-target (vscale camera 1.0)) ; Must copy.. don't ask..
              (parent-gadget starmap) gameui
              next-gadget starmap)))))

(defun client-do-next-turn (gamebar)
  (with-slots (starmap) gamebar
    (activate-new-gadget
     (make-instance 'sequencer-gadget
      :functions
      (list
       ;; At the end of a turn:
       ;;  * Finish-for-turn on an open panel (needed to send fleets on their way)
       ;;  * Prompt-for-research (needed for first turn research choices)
       #'ui-finish-turn
       (lambda () (incf (windup-factor-of starmap) 1.5))
       (lambda () (next-turn *universe*))
       ;; At the beginning of the next turn:
       ;;  * Compute sequence of modal UI panels and open the bottom-panel-host if needed.
       ;;  * Play event sounds
       ;;  * Update open starmap panels
       #'update-ui-for-new-turn)))))

(with-vars (labels)
  (defun draw-status-bar (gameui x0)
    (let ((color (pstyle-label-color (style-of *player*)))
          (y 19))
      (mapcar (lambda (label xoff) (draw-img-deluxe label (+ x0 xoff) y color))
              (cachef (labels (year-of *universe*) :delete (lambda (list) (dolist (img list) (free-img img))))
                (mapcar (lambda (x) (render-label gameui :gothic 20 x))
                        (list (format nil "Year ~D" (year-of *universe*))
                              (format nil "Pop: ~:D m" (round (reduce #'+ (colonies *player*) :key #'population-of)))
                              (format nil "Prod: ~:D BC" (round (reduce #'+ (colonies *player*) :key #'production-of))))))
              (list 0 112 240)))))

(defmethod gadget-run ((gadget gameui) uic)
  ;; Run inferior UI elements first, because we have to draw on top of them.
  (let ((bottom (panel-height gadget)))
    (run-hosted-panel uic gadget bottom)
    
    (draw-bar* (img :gamebar-left) (img :gamebar-right) (texture :gamebar-fill) 0 0 (uic-width uic))
    (draw-img (imgblock :status-bar) 99 0)
    (draw-status-bar gadget 125)

    (let* ((game-label (global-label :bold 14 "Game"))
           (turn-label (global-label :bold 14 "Next Turn"))
           ;;(research-label (global-label :bold 14 "Research"))
           (color (pstyle-label-color (style-of *player*)))
           ;; Button states:
           (clicked-game (run-labelled-button uic game-label 16 3 :center-x nil :color color))
           (clicked-turn (run-labelled-button uic turn-label (- (uic-width uic) 68) 3 :color color))
           #+NIL (clicked-research (run-labelled-button uic research-label (- (uic-width uic) 68 110) 3 :color color)))
      
      (cond
        (clicked-game (printl "You clicked the Game button!"))
        (clicked-turn (client-do-next-turn gadget))
        ((and (< (uic-my uic) bottom) (released? uic +left+)) (close-panels))))
    
    (when *debug-show-packset* (debug-show-packset))

;;    (run-shader-test uic)
    
    (values)))




