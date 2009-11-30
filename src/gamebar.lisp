;;;; Game status bar and associated menus

(in-package :g1)

(defclass gameui (gadget)
  ((universe :reader universe-of :initarg :universe)
   (starmap)
   (panel :initform nil)
   (panel-y :initform 0)))

(defun create-gameui (universe)
  (let* ((gameui (make-instance 'gameui :universe universe))
         (homeworld (aref (colonies *player*) 0))
         (loc (loc homeworld))
         (camera (v2 (round (v.x loc)) (round (v.y loc)))))

    (prog1 gameui
      (with-slots (starmap next-gadget) gameui
        (setf starmap (make-instance 'debug-starmap :universe universe
                                     :scroll-coord camera
                                     :scroll-target camera)
              (parent-gadget starmap) gameui
              next-gadget starmap)))))




(defun img-bounds* (img x y)
  (let ((x (- x (img-x-offset img)))
        (y (- y (img-y-offset img))))
    (values x y (+ x (img-width img)) (+ y (img-height img)))))

(defun in-img-rect (uic img x y)
  (multiple-value-bind (x0 y0 x1 y1) (img-bounds* img x y)
    (and (<= x0 (uic-mx uic)) (<= y0 (uic-my uic))
         (< (uic-mx uic) x1) (< (uic-my uic) y1))))

(defun run-img-button (uic img-up img-down x y)
  (let ((in (and (uic-active uic) (in-img-rect uic img-up x y))))
    (draw-img (if (and in (held? uic +left+)) img-down img-up) x y)
    (and in (released? uic +left+))))

(defmethod gadget-paint ((gadget gameui) uic)
  ;; Run inferior UI elements first, because we have to draw on top of them.
  (let* ((pointer-in-gamebar (< (uic-my uic) (img-height (img :gamebar-left))))
                                        ;(pointer-in-panel (and panel (not pointer-in-gamebar) (< (uic-my uic) panel-y)))
         (child-uic (child-uic uic 0 0 :active (not pointer-in-gamebar))))
    (with-slots (panel panel-y) gadget
      (if panel
          (run-panel panel child-uic panel-y)
          (gadget-paint (next-gadget gadget) child-uic))))
  
  (draw-bar (img :gamebar-left) (img :gamebar-right) *gamebar-fill* 0 0 (uic-width uic))
  ;;(draw-img (imgblock :game-button-up) 16 3)
  (when (run-img-button uic (imgblock :game-button-up) (imgblock :game-button-down) 16 3)
    (printl "You clicked the Game button!"))
  (let ((up (imgblock :next-turn-button-up))
        (down (imgblock :next-turn-button-down)))
    (when (run-img-button uic up down (- (uic-width uic) 16 (img-width up)) 3)
      (printl "You clicked Next Turn!")))
  
  (values))




