;;;; Game status bar and associated menus

(in-package :g1)

(defclass gameui (gadget panel-host-mixin)
  (starmap))

(defmethod panel-height ((gadget gameui))
  (img-height (img :gamebar-left)))

(defmethod gadget-key-pressed ((gadget gameui) uic keysym char)
  (with-slots (starmap panel closing-panel) gadget
    (cond 
      ((and (eql char #\r) (no-modifiers uic))
       (activate-new-gadget (make-instance 'research-ui :player *player*)))
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
                                     :camera-target (vscale camera 1.0)) ; Copy.. don't ask..
              (parent-gadget starmap) gameui
              next-gadget starmap)))))

(defun client-do-next-turn (gamebar)  
  (with-slots (starmap) gamebar
    (ui-finish-turn)
    (next-turn *universe*)
    (update-ui-for-new-turn)
    (incf (windup-factor-of starmap) 1.5)))

(let (labels)
  (defun draw-status-bar (gameui x0)
    (let ((color (pstyle-label-color (style-of *player*)))
          (y 19))
      (mapcar (lambda (label xoff) (draw-img-deluxe label (+ x0 xoff) y color))
              (cachef (labels (year-of *universe*) :delete (lambda (list) (dolist (img list) (free-img img))))
                (mapcar (lambda (x) (render-label gameui :gothic 20 x))
                        (list (format nil "Year ~D" (year-of *universe*))
                              (format nil "Pop: ~:D mil" (reduce #'+ (colonies *player*) :key #'population-of)))))
              (list 0 112)))))

(defmethod gadget-paint ((gadget gameui) uic)
  ;; Run inferior UI elements first, because we have to draw on top of them.
  (let ((bottom (panel-height gadget)))
    (run-hosted-panel uic gadget bottom)
    
    (draw-bar* (img :gamebar-left) (img :gamebar-right) *gamebar-fill* 0 0 (uic-width uic))
    (draw-img (imgblock :status-bar) 99 0)
    (draw-status-bar gadget 125)

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
        ((and (< (uic-my uic) bottom) (released? uic +left+)) (close-panels))))
    
    (when *debug-show-packset* (debug-show-packset))
    
    ;;(run-shader-test uic)
    
    (values)))




