(in-package :g1)

;; Needed for keysyms =/
#+ecl (ffi:clines "#include <SDL/SDL.h>")

;;;; Line editor buffer state

(deftuple led buffer index)

(defun led/cursor-left (led)
  (led-from led :index (max 0 (1- (led-index led)))))

(defun led/cursor-right (led)
  (led-from led :index (min (length (led-buffer led)) (1+ (led-index led)))))

(defun led/left-segment  (led) (subseq (led-buffer led) 0 (led-index led)))
(defun led/right-segment (led) (subseq (led-buffer led) (led-index led)))

(defun led/insert-char (led char)
  (assert (characterp char))
  (led (concatenate 'string (led/left-segment led) (list char) (led/right-segment led))
       (1+ (led-index led))))

(defun led/home (led) (led-from led :index 0))
(defun led/end (led)  (led-from led :index (length (led-buffer led))))

(defun seq-remove-first (seq) (if (empty? seq) seq (subseq seq 1)))
(defun seq-remove-last (seq)  (subseq seq 0 (max 0 (1- (length seq)))))

(defun led/forward-delete (led)
  (led-from led :buffer (concatenate 'string (led/left-segment led) (seq-remove-first (led/right-segment led)))))

(defun led/backward-delete (led)
  (led (concatenate 'string (seq-remove-last (led/left-segment led)) (led/right-segment led))
       (max 0 (1- (led-index led)))))

;;;; Line editor gadget

(defclass line-editor (gadget key-repeat-mixin)
  ((led :initarg :led)
   (center :initform nil :initarg :center)
   (screen-vector :initarg :screen-vector)
   (accept-fn :accessor accept-fn :initform nil :initarg :accept-fn)
   (insert-filter :initform (constantly t) :initarg :insert-filter)   
   (text-face :initform :sans :initarg :face)
   (text-size :initform 11    :initarg :size)
   (image-map :initform (make-hash-table :test 'equal))))

(defmethod gadget-accept-value ((gadget line-editor) value)
  (and.. (accept-fn gadget) (funcall $ value))
  (exit-gadget gadget))

(defmethod finalize-object ((gadget line-editor))
  (loop for cacheobj being the hash-values of (slot-value gadget 'image-map)
        as img = (cacheobj-derived cacheobj) 
        do (free-img img)))

(defun throbber (time period)
  (when (typep time 'uic) (setf time (uic-time time)))
  (sin (* 2 pi (/ time 1000000.0 period))))

(defun bi->uni (x) (* 0.5 (+ x 1.0)))

(defun line-editor/origin (editor)
  (with-slots (led screen-vector center text-face text-size) editor
    (if (not center)
        screen-vector
        (v2- screen-vector (v2 (ash (compute-string-width text-face text-size (led-buffer led)) -1) 0)))))

(defun line-editor/cursor-from-x (gadget x)
  (with-slots (led text-face text-size) gadget
    (let* ((string (led-buffer led))
           (widths (compute-char-positions text-face text-size string))
           (idx (position-if (lambda (max-x) (<= x max-x)) widths))
           (end-x (if (empty? widths) 0 (aref widths (1- (length widths)))))
           (padding 20))
      (and (>= x 0) 
           (or idx
               (and (< x (+ padding end-x)) (length widths)))))))

(defun line-editor/cursor-from-pointer (gadget uic)
  (let ((v (line-editor/origin gadget)))
    (line-editor/cursor-from-x gadget (- (uic-mx uic) (v2.x v)))))

(defun line-editor/move-cursor-to-pointer (gadget uic)
  (with-slots (led) gadget
    (and.. (line-editor/cursor-from-pointer gadget uic)
           (setf led (led-from led :index $)))))

(defun line-editor/y-in-bounds (gadget y)
  (with-slots (text-size screen-vector) gadget
    (and (>= y (- (v2.y screen-vector) text-size))
         (<  y (+ (v2.y screen-vector) (* 0.25 text-size))))))

(defmethod gadget-run ((gadget line-editor) uic)
  (call-next-method gadget (child-uic uic :active nil))
  (with-slots (led image-map text-face text-size) gadget
    (flet ((cached-label (key value)
             (cachef ((gethash key image-map) value :delete free-img) 
               (render-label gadget text-face text-size value))))

      ;; Left click moves cursor.
      (when (clicked? uic +left+)
        (let ((idx (and (line-editor/y-in-bounds gadget (uic-my uic))
                        (line-editor/cursor-from-pointer gadget uic))))        
          (if idx
              (setf led (led-from led :index idx))
              (gadget-accept-value gadget (led-buffer led)))))

      ;; Right click (release) aborts edit.
      (when (released? uic +right+) (exit-gadget gadget))

      (let* ((cursor-width 5)
             (origin (line-editor/origin gadget))
             (x (v2.x origin))
             (y (v2.y origin))
             (cx (+ 1 x (compute-string-width text-face text-size (led/left-segment led))))
             (alpha ())
             (color (label-color))
             (pulse (color-with-alpha color (f->b (bias-unit (bi->uni (throbber uic 0.5)) -0.20))))
             (content-img (cached-label :contents (led-buffer led))))
        (draw-img content-img x y)
        (draw-img-deluxe (img :caret-glow) cx y pulse)
        (draw-img-deluxe (img :caret) cx y color)))))

(defun insertable-character? (character)
  ;; A better test might be whether the font has a glyph for them..
  (and character
       (<= (char-code character) 255)
       (not (eql character #\Newline))
       (not (eql character #\Return))
       (not (eql character #\Escape))
       ;; I don't think ECL has a #\Delete.
       (not (eql character #\Backspace))
       (not (eql character #\Rubout))))

(defun editor-limit-string-length (max-length)
  (lambda (string index char)
    (declare (ignore index char))
    (< (length string) max-length)))

(defmethod gadget-key-pressed ((gadget line-editor) uic keysym char)
  (declare (ignore uic))
  (printl :key keysym char)
  (with-slots (led insert-filter) gadget
    (flet ((ed (fn &rest args) (setf led (apply fn led args))))
      (cond
        ((eql keysym (keysym :delete)) (ed 'led/forward-delete))
        ((eql keysym (keysym :backspace)) (ed 'led/backward-delete))
        ((eql keysym (keysym :left))  (ed 'led/cursor-left))
        ((eql keysym (keysym :right)) (ed 'led/cursor-right))
        ((eql keysym (keysym :home))  (ed 'led/home))
        ((eql keysym (keysym :end))   (ed 'led/end))
        ((eql keysym (keysym :escape)) (exit-gadget gadget))
        ((eql keysym (keysym :return)) (gadget-accept-value gadget (led-buffer led)))
        ((and (insertable-character? char)
              (funcall insert-filter (led-buffer led) (led-index led) char))         
         (ed 'led/insert-char char))))))


;;;; Subclass of the editor used by the ship designer.

;;; This is only here because ECL whines about the forward reference
;;; if I put it in designer.lisp. I can't abide more build pollution,
;;; and I'm not adding this file as a compile-time dependency.

(defclass designer-line-editor (line-editor)
  ((field-id :accessor field-id-of :initarg :field-id))
  (:default-initargs :center t))

