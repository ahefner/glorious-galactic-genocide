;;;; In theory, I'm aiming to keep details of SDL and OpenGL from
;;;; leaking into the bulk of the game code, so that the Windows port
;;;; could be converted to DirectX without too much difficulty.

(in-package :g1)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0)))

(ffi:clines "#include \"sys.h\"")
(ffi:clines "#include <GL/gl.h>")
(ffi:clines "#include \"text-render.h\"")
(ffi:clines "#include <SDL/SDL_image.h>")

(defun gl-error-to-string (number)
  (case number
    (#x0                                :GL_NO_ERROR)
    (#x0500                         :GL_INVALID_ENUM)
    (#x0501                        :GL_INVALID_VALUE)
    (#x0502                    :GL_INVALID_OPERATION)
    (#x0503                       :GL_STACK_OVERFLOW)
    (#x0504                      :GL_STACK_UNDERFLOW)
    (#x0505                        :GL_OUT_OF_MEMORY)))
    
(defun check-gl-error (&optional (context ""))
  (let ((err (c :int "glGetError()")))
    (unless (zerop err)
      (cerror "Fuck it" "OpenGL error 0x~X (~A) ~A" err (gl-error-to-string err) context))))

(defstruct gltexobj texid width height)

(let ((texobjs (make-array 8)))
  (defun bind-texobj (obj &key (unit 0) force)
    (cond
      ((and (not force) (eq (aref texobjs unit) obj))
       (ffi:c-inline (unit) (:int) (values) "glActiveTexture(GL_TEXTURE0 + #0);")
       (check-gl-error "bind-texobj activate on cache hit"))
      (t
       (ffi:c-inline (unit) (:int) (values) "glActiveTexture(GL_TEXTURE0 + #0);")
       (setf (aref texobjs unit) obj)
       (c "glEnable(GL_TEXTURE_2D)")
       (check-gl-error "bind-texobj activate and enable")
       (c "glBindTexture(GL_TEXTURE_2D, #0)" :int (gltexobj-texid obj))
       (check-gl-error "bind-texobj bind texture")
       (c "glMatrixMode(GL_TEXTURE)")
       (c "glLoadIdentity()")
       (c "glScaled(1.0/(#0), 1.0/(#1), 1.0)" :int (gltexobj-width obj) :int (gltexobj-height obj))
       (check-gl-error "bind-texobj configure matrix")))))

(defun alloc-texid ()
  (ffi:c-inline () () :unsigned-int
   " { GLuint id; glGenTextures(1, &id); @(return 0)=id; }"))

(defun upload-sdl-surface (surface &key min-filter mag-filter)
  (let ((id (alloc-texid)))
    (c "glBindTexture(GL_TEXTURE_2D, #0)" :unsigned-int id)
    (unless (find min-filter (list (cx :int "GL_NEAREST") (cx :int "GL_LINEAR")))
      (c "glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE)"))      
    (ffi:c-inline (surface) (:pointer-void) (values)
     "{ SDL_Surface *surf = #0; 
        int mode = (surf->format->BytesPerPixel==3)? GL_RGB : GL_RGBA;
        glTexImage2D(GL_TEXTURE_2D, 0, mode, surf->w, surf->h, 0, mode, GL_UNSIGNED_BYTE, surf->pixels); 
      }")
    (check-gl-error)
    (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,#0)" :int (or min-filter (cx :int "GL_NEAREST")))
    (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,#0)" :int (or mag-filter (cx :int "GL_NEAREST")))
    (check-gl-error)
    id))

(defstruct (texture (:include gltexobj)))

(defun load-texture-file (filename &key min-filter mag-filter)
  (let ((surface (call :pointer-void "IMG_Load" :cstring filename)))
    (prog1 (make-texture :texid (upload-sdl-surface surface :min-filter min-filter :mag-filter mag-filter)
                         :width  (c :int "((SDL_Surface *)#0)->w" :pointer-void surface)
                         :height (c :int "((SDL_Surface *)#0)->h" :pointer-void surface))
      (call "SDL_FreeSurface" :pointer-void surface))))

(defun set-color* (r g b a)
  (call "glColor4ub" :unsigned-byte r :unsigned-byte g :unsigned-byte b :unsigned-byte a))

(defun set-color (v)
  (call "glColor4ub" 
        :unsigned-byte (aref v 0)
        :unsigned-byte (aref v 1)
        :unsigned-byte (aref v 2)
        :unsigned-byte (if (= 4 (length v)) (aref v 3) 255)))

(defun do-draw-tile (x0 y0 x1 y1 tx ty)
  (let ((width (- x1 x0))
        (height (- y1 y0)))
    (call "glTexCoord2i" :int tx :int ty)
    (call "glVertex2i" :int x0 :int y0)
    (call "glTexCoord2i" :int (+ tx width) :int ty)
    (call "glVertex2i" :int x1 :int y0)
    (call "glTexCoord2i" :int (+ tx width) :int (+ ty height))
    (call "glVertex2i" :int x1 :int y1)
    (call "glTexCoord2i" :int tx :int (+ ty height))
    (call "glVertex2i" :int x0 :int y1)))

(defun draw-tile (x0 y0 x1 y1 tx ty &optional (color #(255 255 255 255)))
  (set-color color)
  (c "glBegin(GL_QUADS)")
  (do-draw-tile x0 y0 x1 y1 tx ty)
  (c "glEnd()"))

(defun do-dual-draw-tile (x0 y0 x1 y1 u0 v0 u1 v1)
  (ffi:c-inline (x0 y0 x1 y1 u0 v0 u1 v1) (:int :int :int :int :int :int :int :int) (values)
   "{ int x0 = #0, y0 = #1, x1 = #2, y1 = #3, u0 = #4, v0 = #5, u1 = #6, v1 = #7, width = x1-x0, height = y1-y0;
      glMultiTexCoord2i(GL_TEXTURE0, u0, v0);
      glMultiTexCoord2i(GL_TEXTURE1, u1, v1);
      glVertex2i(x0, y0);
      glMultiTexCoord2i(GL_TEXTURE0, u0+width, v0);
      glMultiTexCoord2i(GL_TEXTURE1, u1+width, v1);
      glVertex2i(x0+width, y0);
      glMultiTexCoord2i(GL_TEXTURE0, u0+width, v0+height);
      glMultiTexCoord2i(GL_TEXTURE1, u1+width, v1+height);
      glVertex2i(x0+width, y0+height);
      glMultiTexCoord2i(GL_TEXTURE0, u0, v0+height);
      glMultiTexCoord2i(GL_TEXTURE1, u1, v1+height);
      glVertex2i(x0, y0+height); }"
   :one-liner nil))

(defun do-triple-draw-tile (x0 y0 x1 y1 u0 v0 u1 v1 u2 v2)
  (ffi:c-inline (x0 y0 x1 y1 u0 v0 u1 v1 u2 v2) (:int :int :int :int :int :int :int :int :int :int) (values)
   "{ int x0 = #0, y0 = #1, x1 = #2, y1 = #3, u0 = #4, v0 = #5, u1 = #6, v1 = #7, u2=#8, v2=#9, width = x1-x0, height = y1-y0;
      glMultiTexCoord2i(GL_TEXTURE0, u0, v0);
      glMultiTexCoord2i(GL_TEXTURE1, u1, v1);
      glMultiTexCoord2i(GL_TEXTURE2, u2, v2);
      glVertex2i(x0, y0);
      glMultiTexCoord2i(GL_TEXTURE0, u0+width, v0);
      glMultiTexCoord2i(GL_TEXTURE1, u1+width, v1);
      glMultiTexCoord2i(GL_TEXTURE2, u2+width, v2);
      glVertex2i(x0+width, y0);
      glMultiTexCoord2i(GL_TEXTURE0, u0+width, v0+height);
      glMultiTexCoord2i(GL_TEXTURE1, u1+width, v1+height);
      glMultiTexCoord2i(GL_TEXTURE2, u2+width, v2+height);
      glVertex2i(x0+width, y0+height);
      glMultiTexCoord2i(GL_TEXTURE0, u0, v0+height);
      glMultiTexCoord2i(GL_TEXTURE1, u1, v1+height);
      glMultiTexCoord2i(GL_TEXTURE2, u2, v2+height);
      glVertex2i(x0, y0+height); }"
   :one-liner nil))


(defun render-starfield-opengl (x y)
  ;;; Vanilla OpenGL starfield renderer
  (c "glEnable(GL_TEXTURE_2D)")
  (c "glColor3f(1.0f, 1.0f, 1.0f)")
  (bind-texobj *stars00*)
  (c "glBlendFunc(GL_ONE, GL_ZERO)")
  (draw-tile 0 0 (c :int "window_width") (c :int "window_height") (ash x -1) (ash y -1))

  (bind-texobj *stars03*)
  (c "glBlendFunc(GL_ONE, GL_ONE)")
  (draw-tile 0 0 (c :int "window_width") (c :int "window_height") (round (* 0.58 x)) (round (* 0.58 (+ y -25))))

  (bind-texobj *stars01*)
  (c "glBlendFunc(GL_ONE, GL_ONE)")
  (draw-tile 0 0 (c :int "window_width") (c :int "window_height") (round (* 0.666 x)) (round (* 0.666 y))))

(defun render-starfield-multitex (x y)
  ;;; Render starfield using multiple texture units
  (c "glActiveTexture(GL_TEXTURE0)")
  (bind-texobj *stars00* :unit 0)
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)")

  (c "glActiveTexture(GL_TEXTURE1)")
  (bind-texobj *stars03* :unit 1)
  (c "glEnable(GL_TEXTURE_2D)")
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD)")

  (c "glActiveTexture(GL_TEXTURE2)") 
  (bind-texobj *stars01* :unit 2)
  (c "glEnable(GL_TEXTURE_2D)")
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD)")

  (c "glBlendFunc(GL_ONE, GL_ZERO)")
  (c "glBegin(GL_QUADS)")
  (do-triple-draw-tile 0 0 (c :int "window_width") (c :int "window_height")
                     (ash x -1) (ash y -1)
                     (round (* 0.58 x)) (round (* 0.58 (+ y -25)))
                     (round (* 0.666 x)) (round (* 0.666 y)))
  (c "glEnd()")

  ;; Cleanup GL state for rest of game:
  (c "glActiveTexture(GL_TEXTURE1)")
  (c "glDisable(GL_TEXTURE_2D)")

  (c "glActiveTexture(GL_TEXTURE2)")
  (c "glDisable(GL_TEXTURE_2D)")

  (c "glActiveTexture(GL_TEXTURE0)")
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)"))

(defun render-starfield (x y)
  ;; The difference on my laptop? 130 fps versus 111 fps. Screw it.
  (render-starfield-opengl x y)
  #+NIL (render-starfield-multitex x y))

(defun paint-begin ()
;;  (c "glClearColor(#0, #1, #2, 0.0)" :float (random 1.0) :float (random 1.0) :float (random 1.0))
;;  (c "glClear(GL_COLOR_BUFFER_BIT)")
  (c "glDisable(GL_DEPTH_TEST)")
  (c "glDisable(GL_CULL_FACE)")
  (c "glShadeModel(GL_FLAT)")
  (c "glEnable(GL_BLEND)")
  (c "glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)")
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)")

  (c "glMatrixMode(GL_PROJECTION)")
  (c "glLoadIdentity()")
  (c "glOrtho(0, window_width, window_height, 0, -1, 1)")
  (check-gl-error))

(defun paint-finish ()
  (call "glFinish")
  (check-gl-error)
  (c "SDL_GL_SwapBuffers()"))

;;;; Images get loaded from individual files and packed into one
;;;; texture. In contrast to texture structs (see above), we retain
;;;; the SDL surface, as we may need to upload the image at any time.

(defstruct img width height resident-p x y x-offset y-offset surface pixels name owner)

(defun img-is-free? (img)
  (not (or (img-surface img) (img-pixels img))))

(defun free-img (img)  
  (cond
    ((null img) (values))
    ((eql (img-owner img) *global-owner*)
     (format *trace-output* "Not releasing global image ~W.~%" (img-name img))
     (return-from free-img))
    ((img-surface img) (call "SDL_FreeSurface" :pointer-void (img-surface img)))
    ((img-pixels img)  (call "free" :pointer-void (img-pixels img)))
    (t (warn "Attempt to free IMG ~A with no underlying surface or pixel pointer. Odd." img)))
  (when img
    (setf (img-surface img) nil
          (img-pixels img) nil)))

(defun load-image-file (filename &optional x-offset y-offset)
  ;;(format t "~&Loading ~A~%" filename)
  (let* ((surface (call :pointer-void "IMG_Load" :cstring filename))
         (width  (cx :int "((SDL_Surface *)#0)->w" :pointer-void surface))
         (height (cx :int "((SDL_Surface *)#0)->h" :pointer-void surface)))
    (make-img :width width
              :height height
              :resident-p nil
              :x-offset (or x-offset (round width 2))
              :y-offset (or y-offset (round height 2))
              :surface surface
              :pixels (cx :pointer-void "((SDL_Surface *)#0)->pixels" :pointer-void surface)
              :name filename)))

(let ((img-lookaside (make-hash-table))           ; Absurdly premature optimization
      (img-hash (make-hash-table :test 'equal)))
  (defun img (name)
    (or (gethash name img-lookaside)
        (let ((img (or (gethash name img-hash)
                       (load-image-file (apath name)))))
          (setf (img-owner img) *global-owner*
                (gethash name img-hash) img                
                (gethash name img-lookaside) img)))))

(let ((tex-lookaside (make-hash-table))           ; More Absurdly premature optimization
      (tex-hash (make-hash-table :test 'equal)))
  (defun texture (name &key min-filter mag-filter)
    (or (gethash name tex-lookaside)
        (let ((tex (or (gethash name tex-hash)
                       (load-texture-file (apath name) :min-filter min-filter :mag-filter mag-filter))))
          (setf (gethash name tex-hash) tex
                (gethash name tex-lookaside) tex)))))

(defun imgblock (name)
  (let ((img (img name)))
    (prog1 img
      (setf (img-x-offset img) 0
            (img-y-offset img) 0))))

(defun img-detach (img)
  (setf (img-resident-p img) nil
        (img-x img) nil
        (img-y img) nil))

(defun draw-img (img x y)
  (draw-img-deluxe* img x y 255 255 255 255))

(defun draw-img-deluxe* (img x y r g b a)
  ;;; FIXME: Inefficient. Batch these inside one glBegin.

  ;;; FIXME: Check whether the damn thing is on screen before we consider uploading a texture.
  (bind-texobj *packset*)
  (packset-ensure *packset* img)
  (c "glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)")
  ;; TODO: Clamp and set border color. 
  ;;(c "glTexParameter(GL_TEXTURE_2D, TEXTURE_BORDER_COLOR, whatever))

  (c "glBegin(GL_QUADS)")
  (call "glColor4ub" :unsigned-byte r :unsigned-byte g :unsigned-byte b :unsigned-byte a)

  (ffi:c-inline (x y (img-x img) (img-y img) (img-width img) (img-height img) (img-x-offset img) (img-y-offset img))
                (:int :int :int :int :int :int :int :int)
                (values)
                "{ int x = #0, y = #1, ix = #2, iy = #3, w = #4, h = #5, xo = #6, yo = #7;
                   x -= xo;
                   y -= yo;
                   glTexCoord2i(ix, iy);
                   glVertex2i(x, y);
                   glTexCoord2i(ix+w, iy);
                   glVertex2i(x+w, y);
                   glTexCoord2i(ix+w, iy+h);
                   glVertex2i(x+w, y+h);
                   glTexCoord2i(ix, iy+h);
                   glVertex2i(x, y+h);
                 }")
  (c "glEnd()"))

(defun draw-img-deluxe (img x y color)
  (draw-img-deluxe* img x y (aref color 0) (aref color 1) (aref color 2) (if (= 4 (length color)) (aref color 3) 255)))

(defun draw-line (u v &key (pattern-offset 0) (color #(255 255 255 255)))
  (let ((texture (texture :line-dashed
                          :mag-filter (cx :int "GL_LINEAR")
                          ;; On fglrx, we need GL_LINEAR on the min filter to get a smooth line.
                          :min-filter (cx :int "GL_LINEAR"))))
    (bind-texobj texture)
    (set-color color)
    (c "glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)")
    (ffi:c-inline ((v2.x u) (v2.y u) (v2.x v) (v2.y v)
                   (texture-width texture) (texture-height texture)
                   pattern-offset)
                  (:int :int :int :int :int :int :int)
                  (values)
;; Sure, I could do this in lisp, and it'd be less code, but ECL's
;; compiler is a joke, and I bitterly resent avoidable runtime
;; dispatch and float consing, even when performance is completely
;; irrelevant.
"
{
   int texwidth = #4, texheight = #5, pattern_offset = #6;
   float x0 = #0, y0 = #1, x1 = #2, y1 = #3;
   float dx = x1-x0, dy = y1-y0;
   float nx = dy, ny = -dx;
   float len = sqrtf(nx*nx + ny*ny);
   float thickness = 8.0;
   float b = -pattern_offset - fmodf(len*0.5, texwidth);
   if (len >= 1.0) {
     nx = nx * thickness / (2.0 * len);
     ny = ny * thickness / (2.0 * len);
     glBegin(GL_QUADS);
     glTexCoord2f(b, 0.0);
     glVertex2f(x0-nx, y0-ny);
     glTexCoord2f(b+len, 0.0);
     glVertex2f(x1-nx, y1-ny);
     glTexCoord2f(b+len, texheight);
     glVertex2f(x1+nx, y1+ny);
     glTexCoord2f(b, texheight);
     glVertex2f(x0+nx, y0+ny);
     glEnd();
   }
}
")))

#+NIL
(defun simple-draw-line (u v)
  (c "glDisable(GL_TEXTURE_2D)")
  (c "glBegin(GL_LINES)")
  (c "glColor4ub(255,0,0,255)")
  (call "glVertex2f" :float (v.x u) :float (v.y u))
  (call "glVertex2f" :float (v.x v) :float (v.y v))
  (c "glEnd()")
  (c "glEnable(GL_TEXTURE_2D)")
  (check-gl-error))

;;;; Text renderer

(defun render-label (owner face height string &key (align-x :left) (align-y :baseline))
  (let* ((facenum (ecase face
                    (:sans 0)
                    (:bold 1)
                    (:italic 2)
                    (:bold-italic 3)
                    (:gothic 4)))
         (cimage (call :pointer-void "render_label"
                       :unsigned-int facenum
                       :unsigned-int #xFFFFFF :unsigned-int height :cstring string))
         (img (make-img :width  (cx :int "((image_t)#0)->w" :pointer-void cimage)
                        :height (cx :int "((image_t)#0)->h" :pointer-void cimage)
                        :name string
                        :owner owner
                        :pixels (cx :pointer-void "((image_t)#0)->pixels" :pointer-void cimage)
                        :x-offset (cx :int "((image_t)#0)->x_origin" :pointer-void cimage)
                        :y-offset (cx :int "((image_t)#0)->y_origin" :pointer-void cimage))))
    #+NIL
    (format t "~&Rendered label ~W: ~Dx~D~%"
            string
            (cx :int "((image_t)#0)->w" :pointer-void cimage)
            (cx :int "((image_t)#0)->h" :pointer-void cimage))
    (case align-x
      (:left)
      (:center (setf (img-x-offset img) (ash (img-width img) -1)))
      (:right  (setf (img-x-offset img) (img-width img))))
    (case align-y
      (:top (setf (img-y-offset img) 0))
      (:baseline)
      (:bottom (setf (img-y-offset img) (img-height img))))

    ;; Put it in the packset, since we're probably going to use it immediately.
    (packset-ensure *packset* img)

    img))

(let ((global-labels (make-hash-table :test 'equal)))
  (defun global-label (&rest args)
    (orf (gethash args global-labels)
         (destructuring-bind (face size string) args
           (render-label *global-owner* face size string)))))

(defun gtxt (string) (global-label :sans 11 string))

(defun player-label (key face size string)
  (orf (gethash key (owned-images-of *player*))
       (render-label *player* face size string)))

;;;; Various utilities

(defun fill-rect (x0 y0 x1 y1 r g b a)
  (call "glColor4ub" :unsigned-byte r :unsigned-byte g :unsigned-byte b :unsigned-byte a)
  (c "glDisable(GL_TEXTURE_2D)")
  (c "glBegin(GL_QUADS)")
  (ffi:c-inline (x0 y0 x1 y1) (:int :int :int :int) (values)
                " { glVertex2i(#0,#1); glVertex2i(#2,#1); glVertex2i(#2,#3); glVertex2i(#0,#3); }")
  (c "glEnd()")
  (c "glEnable(GL_TEXTURE_2D)")
  (values))

(defun bar-style-width (style)
  (+ (img-width (bar-style-left style)) (img-width (bar-style-right style))))

(defun bar-style-height (style) (img-height (bar-style-left style)))

(defun draw-bar* (left-img right-img fill-tile x y width)
  (let ((left-x (img-width left-img))
        (right-x (- width (img-width right-img)))
        (fill-y (img-height left-img)))
  (draw-img right-img (+ x right-x (img-x-offset left-img)) (+ y (img-y-offset left-img)))
  (draw-img left-img (+ x (img-x-offset left-img)) (+ y (img-y-offset left-img)))
  (bind-texobj fill-tile)
  (draw-tile (+ x left-x) y (+ x right-x) (+ y fill-y) 0 0)))

(defun draw-bar (style x y width)
  (draw-bar* (bar-style-left style) (bar-style-right style) (bar-style-fill style) x y width))

(defun img-bounds* (img x y)
  (let ((x (- x (img-x-offset img)))
        (y (- y (img-y-offset img))))
    (values x y (+ x (img-width img)) (+ y (img-height img)))))

(defun pointer-in-rect* (uic x0 y0 x1 y1)
  (and (<= x0 (uic-mx uic)) (<= y0 (uic-my uic))
       (< (uic-mx uic) x1) (< (uic-my uic) y1)))

(defun pointer-in-img-rect (uic img x y)
  (multiple-value-bind (x0 y0 x1 y1) (img-bounds* img x y)
    (pointer-in-rect* uic x0 y0 x1 y1)))

(defun draw-button (button-style label pressed x top &key (center-x t) min-width (baseline-adjust 0) (color (vector 255 255 255)))
  (let* ((bar-style (if pressed (button-style-pressed button-style) (button-style-released button-style)))
         (label-width (max (or min-width 0) (if label (img-width label) 0)))
         (bar-width (+ label-width (bar-style-width bar-style)))
         (lx (if center-x (- x (ash bar-width -1)) x)))
    (draw-bar bar-style lx top bar-width)
    (when label
      (draw-img-deluxe label
                       (+ (img-x-offset label)
                          (if center-x
                              (- x (ash label-width -1))
                              (+ lx (img-width (bar-style-left bar-style)))))
                       (+ top (+ baseline-adjust (button-style-baseline button-style)))
                       color))))

;;;; Packed texture manager - pack multiple images into one texture.

(defstruct packstack x0 y0 width height parent)
(defstruct (packset (:constructor %make-packset) (:include gltexobj))
  stack (images (make-array 256 :adjustable t :fill-pointer 0)))

(defun make-packset (width height)
  (let ((ps (%make-packset :width width
                           :height height
                           :texid (alloc-texid)
                           :stack nil)))
    (packset-clear ps)
    ps))

;;; FIXME: When I've peeked at the packset texture, there were
;;; overlapping (clipped) images, making me suspect that this routine
;;; either isn't called upon repack or fails to clear the image as
;;; expected. Strange.
(defun packset-clear (ps)
  (check-gl-error)
  (bind-texobj ps)
  (ffi:c-inline ((packset-texid ps) (packset-width ps) (packset-height ps)) (:int :int :int) (values)
                "{ void *tmp = calloc(#1*#2, 4);
                   static int blub = 1;
                   unsigned px = 0xFF000000 | ((blub&1)? 0xFF : 0x00) | ((blub&2)? 0xFF00 : 0x00) | ((blub&4)? 0xFF0000 : 0x00);
                   for (int i=0; i<(#1*#2); i++) ((unsigned *)tmp)[i] = px;
                   blub++;
                   glBindTexture(GL_TEXTURE_2D, #0);
                   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, #1, #2, 0, GL_RGBA, GL_UNSIGNED_BYTE, tmp);
                   free(tmp);
                 }")
  (check-gl-error)
  ;; We need to do this to get pixel-accurate rendering, at least on
  ;; the R300 DRI driver on my laptop, otherwise some filtering might
  ;; sneak in and smear lines or fuck up the edges of rectangles.
  (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)")
  (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)")
  (packset-reset ps))

(defun packset-reset (ps)
  (map nil #'img-detach (packset-images ps))
  (setf (packset-stack ps) (make-packstack :x0 0 :y0 0 
                                           :width (packset-width ps) 
                                           :height (packset-height ps)
                                           :parent nil)))

(defun packset-fit (packset object)

  (unless (not (img-resident-p object))
    (format t "~&Odd. ~A is already resident, death imminent.." object))

  (assert (not (img-resident-p object)))
  (tagbody insert
     (let ((stack (packset-stack packset))
           (width  (img-width object))
           (height (img-height object)))
       (cond
         ;; Image fits in current region, allocate here.
         ((and (<= width (packstack-width stack))
               (<= height (packstack-height stack)))         
          (packset-alloc packset width height object)
          (return-from packset-fit t))
         ;; TODO: We should be able to grow the region in height (need
         ;; to walk up the stack and grow the parents too). (This
         ;; becomes harder if we follow through on the idea below, but
         ;; I'll have to think about it).

         ;; Image doesn't fit here, pop the stack and try again.
         ;; FIXME: This is dumb. If the region is big enough to be
         ;; worth saving, we ought to just move it to the bottom of
         ;; the stack. A few wide text strings could wipe out our
         ;; whole texture otherwise.
         ((packstack-parent stack)
          (setf (packset-stack packset) (packstack-parent stack))
          (go insert))
         ;; Image can't fit in texture!
         (t (return-from packset-fit nil))))))

(defun packset-ensure (packset object)
  (cond
    ((img-resident-p object) (values))
    ;; Upload if the object fits..
    ((packset-fit packset object)
     (vector-push-extend object (packset-images packset))
     (packset-upload-object object))
    ;; Otherwise, repack.
    (t (vector-push-extend object (packset-images packset))
       (packset-repack-for-object packset object))))

(defvar *ps-suppress-upload* nil)

(defun packset-upload-object (object)
  (check-gl-error "packset upload precheck")
  (bind-texobj *packset*)
  (let ((x (img-x object))
        (y (img-y object))
        (width (img-width object))
        (height (img-height object)))
    (assert (not (img-is-free? object)))
    #+NIL
    (format t "~&Uploaded ~A at (~D,~D) [~Dx~D]~%" object x y width height)
    (unless *ps-suppress-upload*
      (c "glTexSubImage2D(GL_TEXTURE_2D, 0, #0, #1, #2, #3, GL_RGBA, GL_UNSIGNED_BYTE, #4)"
         :int x :int y :int width :int height :pointer-void (img-pixels object)))

    ;; WORKAROUND for fglrx: If at first you don't succeed, try again.
    (when (zerop (c :int "(glGetError() == GL_NO_ERROR)"))
      (format *trace-output* "Driver fuckup? glTexSubImage2D failed.~%")
      (printl :ps-upload :x x :y y :width width :height height :pointer-void (img-pixels object))
      (bind-texobj *packset* :unit 0 :force t)
      (c "glTexSubImage2D(GL_TEXTURE_2D, 0, #0, #1, #2, #3, GL_RGBA, GL_UNSIGNED_BYTE, #4)"
         :int x :int y :int width :int height :pointer-void (img-pixels object)))

    (check-gl-error "packset subimage upload")))

(defun packset-alloc (packset width height object)
  (let* ((current (packset-stack packset))
         (next (make-packstack :x0 (+ width  (packstack-x0 current))
                               :y0 (packstack-y0 current)
                               :width  (- (packstack-width current) width)
                               :height height)))    
    (setf (img-resident-p object) t
          (img-x object) (packstack-x0 current)
          (img-y object) (packstack-y0 current))
    (cond
      ;; Shrink current section if vertical space remaining.
      ((< height (packstack-height current))
       (decf (packstack-height current) height)
       (incf (packstack-y0 current) height)
       (setf (packset-stack packset) next
             (packstack-parent next) current))
      ;; Otherwise replace it.
      (t (setf (packstack-parent next) (packstack-parent current)
               (packset-stack packset) next)))))

(defun packset-repack-for-object (ps object)
  (bind-texobj ps)
  (format t "~&Repacking for ~A..~%" object)
  ;; First, try sorting by height and reinserting the images.
  ;;(packset-reset ps)  
  (packset-clear ps)
  (printl :survived-reset)
  ;; Technically, SORT isn't required to give us an vector that has a
  ;; fill pointer when the input vector has one, but as it happens ECL
  ;; does, so I'll rely on that.. because any Lisp that doesn't is
  ;; fucking INSANE. Note that I'll make the same claim about
  ;; DELETE-IF, but ECL fails on that front.
  (setf (packset-images ps) (sort (sane-delete-if #'img-is-free? (packset-images ps)) #'> :key #'img-height))
  ;;(print (packset-images ps))
  (cond
    ;; Can we repack everything into one texture? If so, upload and we're done.
    ((every (lambda (obj) (prog1 (packset-fit ps obj) #+NIL (print obj))) (packset-images ps))
     ;;(print (packset-images ps))
     (loop for img across (packset-images ps) do (packset-upload-object img)))
    ;; If not, we need to throw some things out.
    (t
     ;; Reset previous attempt at allocation.
     (packset-clear ps)
     
     (format t "~&-- Fuck! Packset contents: --~%")
     (loop for item across (packset-images ps)
           do (format t "~& -  ~A~%" item))
     (setf (fill-pointer (packset-images ps)) 0)

     ;; Quick hack, although perfectly reasonable:
     (format t "~&Fitting ~A~%" object)
     (assert (packset-fit ps object))   ; XXX
     (vector-push-extend object (packset-images ps))
     (format t "~&Upload ~A~%" object)
     (packset-upload-object object)
     

     #+NIL
     (error "I'm one lazy fucker, right? Still need to allocate ~A" object))))

(defun debug-show-packset ()
  (fill-rect 64 64 (+ 64 (packset-width *packset*)) (+ 64 (packset-height *packset*)) 0 0 0 255)
  (draw-tile 64 64 (+ 64 (packset-width *packset*)) (+ 64 (packset-height *packset*)) 0 0))


;;;; Shader test

(defun gl-get-integer (enum)
  (prog1 (ffi:c-inline (enum) (:int) :int "{ GLint tmp; glGetIntegerv((GLenum)#0, &tmp); @(return 0) = tmp; }"
                       :one-liner nil)
    (check-gl-error)))

(defun alloc-program-id ()
  (ffi:c-inline () () :unsigned-int
   " { GLuint id; glGenProgramsARB(1, &id); @(return 0)=id; }"))

(defun sgr (&rest modes) (format t "~C[~{~D~^;~}m" #\Esc modes)) ; on loan from Shuffletron

(defun program-shader (source &key (signal-error t) (error-stream *trace-output*) (name "anonymous"))
  (check-gl-error)
  (c "glProgramStringARB(GL_FRAGMENT_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB, #0, #1)"
     :unsigned-int (length source) :cstring source)
  (let ((err (call :int "glGetError")))
    (cond
      ((zerop err) t)
      ((/= err (cx :int "GL_INVALID_OPERATION"))
       (error "Unknown error ~D (0x~X) from glProgramStringARB" err err))
      (t (let* ((position (gl-get-integer (cx :int "GL_PROGRAM_ERROR_POSITION_ARB")))
                (inline (< position (length source)))
                (string (c :cstring "glGetString(GL_PROGRAM_ERROR_STRING_ARB)"))
                (output (format nil "Shader (~A) error, position ~:D~%~A"                                
                                name position string)))
           (when error-stream
             (format error-stream "~A~%" output)
             (when inline
               (format error-stream "------------------------------------------------------------~%")
               (sgr 32)
               (write-string (subseq source 0 position) error-stream)
               (sgr 1 33)
               (format error-stream ">>")
               (sgr 0 31)
               (write-string (subseq source position) error-stream)
               (sgr 0)
               (format error-stream "------------------------------------------------------------~%")))
           (when signal-error (error "~A" output))
           nil)))))
    
    

(let ((init nil)
      (id nil))
  (defun ensure-shader-test-init ()
    (check-gl-error)
    (unless init
      (setf init t
            id (alloc-program-id))
      (c "glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, #0)" :unsigned-int id)
      (check-gl-error "bind fragment program")
      (let ((shader-src
"!!ARBfp1.0
# Normal mapped sprite shader
OPTION ARB_precision_hint_fastest;
ATTRIB tc = fragment.texcoord;          # first set of texture coordinates
ATTRIB ulight = fragment.color.primary; # abuse primary color as light vector. not ideal.

PARAM uadjust = { 0.5, 0.5, 0.5, 0 };

OUTPUT outColor = result.color;

TEMP color, surfnorm, tmp, light;

TEX surfnorm, tc, texture[1], 2D;     # Sample normal map
TEX color,    tc, texture[0], 2D;     # Sample color map
SUB light, ulight, uadjust;           # Same same
SUB surfnorm, surfnorm, uadjust;      # Adjust from unsigned to signed range (-0.5,0.5)
# MUL surfnorm, surfnorm, {2.0, 2.0, 2.0, 1.0};
DP3 tmp, surfnorm, light;             # Diffuse coefficient
#MUL tmp, tmp, tmp;
ADD tmp, tmp, tmp;                    # (correct range of inputs)
ADD tmp, tmp, tmp;                    # (correct range of inputs)
MUL tmp, tmp, color;

MOV tmp.a, surfnorm.a;
MOV outColor, tmp;


    
END
"))
        (program-shader shader-src)
))))
      
      
(defun unorm (vector)
  (v+ (vec 0.5 0.5 0.5) (vscaleto vector 0.5)))

#+NIL
(defun do-draw-ship-sprite (x0 y0 x1 y1 tx ty)
  (ffi:c-inline (x0 y0 x1 y1 tx ty) (:int :int :int :int :int :int) (values)
   "{ int x0 = #0, y0 = #1, x1 = #2, y1 = #3, tx = #4, ty = #5, width = x1-x0, height = y1-y0;
      glMultiTexCoord2i(GL_TEXTURE0, tx, ty);
      glMultiTexCoord2i(GL_TEXTURE1, tx, ty);
      glVertex2i(x0, y0);
      glMultiTexCoord2i(GL_TEXTURE0, tx+width, ty);
      glMultiTexCoord2i(GL_TEXTURE1, tx+width, ty);
      glVertex2i(x0+width, y0);
      glMultiTexCoord2i(GL_TEXTURE0, tx+width, ty+height);
      glMultiTexCoord2i(GL_TEXTURE1, tx+width, ty+height);
      glVertex2i(x0+width, y0+height);
      glMultiTexCoord2i(GL_TEXTURE0, tx, ty+height);
      glMultiTexCoord2i(GL_TEXTURE1, tx, ty+height);
      glVertex2i(x0, y0+height); }"
   :one-liner nil))

(defun do-draw-ship-sprite (x y tw th angle zoom)
  (ffi:c-inline (x y tw th angle zoom) (:int :int :int :int :float :float) (values)
   "{ float angle = #4, zoom = #5;
      int x = #0, y = #1, width = #2, height = #3;
      int tx = 0, ty = 0;
      float cx = x, cy = y, hw = zoom * width * 0.5f, hh = zoom * height * 0.5f;
      float adx = hw *  cos(angle), ady = hh * sin(angle);     
      float idx = hw * -sin(angle), idy = hh * cos(angle);

      glMultiTexCoord2i(GL_TEXTURE0, tx, ty);
      glMultiTexCoord2i(GL_TEXTURE1, tx, ty);
      glVertex2f(cx - adx - idx, cy - ady - idy);

      glMultiTexCoord2i(GL_TEXTURE0, tx+width, ty);
      glMultiTexCoord2i(GL_TEXTURE1, tx+width, ty);
      glVertex2f(cx + adx - idx, cy + ady - idy);

      glMultiTexCoord2i(GL_TEXTURE0, tx+width, ty+height);
      glMultiTexCoord2i(GL_TEXTURE1, tx+width, ty+height);
      glVertex2f(cx + adx + idx, cy + ady + idy);

      glMultiTexCoord2i(GL_TEXTURE0, tx, ty+height);
      glMultiTexCoord2i(GL_TEXTURE1, tx, ty+height);
      glVertex2f(cx - adx + idx, cy - ady + idy);

    }"
   :one-liner nil))

(defun run-shader-test (uic)
  (ensure-shader-test-init)
  (let* ((x0 360)
         (y0 300)
         (angle (/ (uic-mx uic) 200.0))
         (zoom (/ (+ 1.0 (/ (max 0 (- (uic-my uic) 100)) 300.0))))
         (pz 10)
         (light (unorm (vec (single (* -140 (cos angle))) (single (* -140 (sin angle))) (single 70)))))
    ;;(printl :angle angle :zoom zoom :light light)
    (with-vector (l light)
      (call "glColor3f" :float l.x :float l.y :float l.z))
    (c "glEnable(GL_FRAGMENT_PROGRAM_ARB)")
    (check-gl-error "enable fragment program")
    (let ((nm  (texture :scout-normals :min-filter (cx :int "GL_LINEAR_MIPMAP_NEAREST") :mag-filter (cx :int "GL_LINEAR")))
          (tex (texture :scout-texture :min-filter (cx :int "GL_LINEAR_MIPMAP_NEAREST") :mag-filter (cx :int "GL_LINEAR")))
          (lights (texture :scout-lights :min-filter (cx :int "GL_LINEAR_MIPMAP_NEAREST") :mag-filter (cx :int "GL_LINEAR"))))
      (bind-texobj tex :unit 0)
      (bind-texobj nm  :unit 1)
      (c "glBegin(GL_QUADS)")
      (do-draw-ship-sprite x0 y0 (gltexobj-width tex) (gltexobj-height tex) angle zoom)
      (c "glEnd()")
      (c "glDisable(GL_FRAGMENT_PROGRAM_ARB)")
      (c "glActiveTexture(GL_TEXTURE1)")
      (c "glDisable(GL_TEXTURE_2D)")
      (bind-texobj lights :unit 0)
      (set-color* 255 255 255 255)
      (c "glBlendFunc(GL_SRC_ALPHA, GL_ONE)")
      (c "glBegin(GL_QUADS)")
      (do-draw-ship-sprite x0 y0 (gltexobj-width tex) (gltexobj-height tex) angle zoom)
      (c "glEnd()"))
   
    (check-gl-error)))


;;;; I might as well put the audio code here too.

(defstruct sound-effect name pointer length)

(defun load-wav-file (filename)
  (multiple-value-bind (pointer length)
      (ffi:c-inline (filename) (:cstring) (values :pointer-void :int)
       "{
          SDL_AudioSpec spec;
          void *data;
          unsigned length;
          if (!SDL_LoadWAV(#0, &spec, &data, &length)) length = 0;
          @(return 0) = data;
          @(return 1) = length; 
        }")
    (printl :wavfile :pointer pointer :length length)
    (when (zerop length)
      (format t "~&Error loading ~A (~A)~%" filename (c :cstring "SDL_GetError()")))
    (make-sound-effect :name filename :pointer pointer :length (truncate length 2))))

(let ((sfx-lookaside (make-hash-table))
      (sfx-hash (make-hash-table :test 'equal)))
  (defun sound-effect (name)
    (or (gethash name sfx-lookaside)
        (let ((sound (or (gethash name sfx-hash)
                         (etypecase name
                           (sound-effect name)
                            (string (load-wav-file (format nil "~A/sfx/~A" (asset-base) name)))
                            (symbol (load-wav-file (format nil "~A/sfx/~A.wav" 
                                                           (asset-base) 
                                                           (string-downcase (symbol-name name)))))))))
          (setf (gethash name sfx-hash) sound
                (gethash name sfx-lookaside) sound)))))

(defun play-sound (sound-effect)
  (setf sound-effect (sound-effect sound-effect))
  (call "play_sound_effect" 
        :pointer-void (sound-effect-pointer sound-effect)
        :int (sound-effect-length sound-effect)))

(defun snd-click () (play-sound :click-high))
