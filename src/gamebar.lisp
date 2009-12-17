;;;; Game status bar and associated menus

(in-package :g1)

(defclass gameui (gadget panel-host-mixin)
  ((universe :reader universe-of :initarg :universe)
   (starmap)))

(defmethod gadget-key-pressed ((gadget gameui) uic keysym char)
  (with-slots (starmap panel closing-panel) gadget
    (cond 
      ((eql char #\Space)
       (client-do-next-turn gadget))
      ((and panel (not closing-panel)) (gadget-key-pressed panel uic keysym char))
      (t (gadget-key-pressed starmap uic keysym char)))))

(defmethod gadget-key-released ((gadget gameui) uic keysym)
  (with-slots (starmap panel closing-panel) gadget
    (cond ((and panel (not closing-panel)) (gadget-key-released panel uic keysym))
          (t (gadget-key-released starmap uic keysym)))))

(defun create-gameui (universe)
  (let* ((gameui (make-instance 'gameui :universe universe))
         (homeworld (aref (colonies *player*) 0))
         (loc (loc homeworld))
         (camera (vec (v.x loc) (v.y loc) 0.0f0)))

    (prog1 gameui
      (with-slots (starmap next-gadget) gameui
        (setf starmap (make-instance 'debug-starmap :universe universe
                                     :camera camera
                                     :camera-target (vscale camera 1.0)) ; Copy.. don't ask..
              (parent-gadget starmap) gameui
              next-gadget starmap)))))

(defun close-panels ()
  (with-slots (panel closing-panel) *gameui*
    (when panel 
      (dismiss-panel panel)
      (setf closing-panel t))))

(defun client-do-next-turn (gamebar)
  (with-slots (universe starmap) gamebar
    (ui-finish-turn)
    (next-turn universe)
    (update-ui-for-new-turn)
    (incf (windup-factor-of starmap) 1.5)))

(defmethod gadget-paint ((gadget gameui) uic)
  ;; Run inferior UI elements first, because we have to draw on top of them.
  (let* ((gb-height (img-height (img :gamebar-left)))
         (pointer-in-gamebar (< (uic-my uic) gb-height))
         ;;(pointer-in-panel (and panel (not pointer-in-gamebar) (< (uic-my uic) panel-y)))
         (child-uic (child-uic uic 0 0 :active (not pointer-in-gamebar))))
    (with-slots (panel panel-y closing-panel starmap) gadget
      (cond 
        (panel
         (let* ((target (if closing-panel 0 (+ gb-height (panel-height panel))))
                (dist (- target panel-y))
                (rate (* 10 (if (< dist 0)
                                (min -1 (- (floor (* (uic-delta-t uic) (sqrt (- dist))))))
                                (max  1 (ceiling  (* (uic-delta-t uic) (sqrt dist))))))))
           (setf panel-y (clamp (+ panel-y rate) 0 (+ gb-height (panel-height panel))))
           (when closing-panel (setf (uic-active child-uic) nil))
           (run-panel panel child-uic panel-y)
           (when (and closing-panel (<= panel-y gb-height))
             (finalize-object panel)
             (setf panel nil
                   panel-y 0
                   closing-panel nil))))
        (t (gadget-paint (next-gadget gadget) child-uic)))
  
      (draw-bar* (img :gamebar-left) (img :gamebar-right) *gamebar-fill* 0 0 (uic-width uic))
      (draw-img (imgblock :status-bar) 99 0)

      (let* ((game-label (global-label :bold 14 "Game"))
             (turn-label (global-label :bold 14 "Next Turn"))
             (research-label (global-label :bold 14 "Research"))
             (color (pstyle-label-color (style-of *player*)))
             ;; Button states:
             (clicked-game (run-labelled-button uic game-label 16 3 :center-x nil :color color))
             (clicked-turn (run-labelled-button uic turn-label (- (uic-width uic) 68) 3 :color color))
             #+NIL (clicked-research (run-labelled-button uic research-label (- (uic-width uic) 68 110) 3 :color color)))
        
        (cond
          (clicked-game (printl "You clicked the Game button!"))
          (clicked-turn (client-do-next-turn gadget))
          ((and pointer-in-gamebar (released? uic +left+)) (close-panels))))

      (when *debug-show-packset* (debug-show-packset))
      
      (values))))




