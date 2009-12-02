;;;; In theory, I'm aiming to keep details of SDL and OpenGL from
;;;; leaking into the bulk of the game code, so that the Windows port
;;;; could be converted to DirectX without too much difficulty.

(in-package :g1)

(ffi:clines "#include \"sys.h\"")
(ffi:clines "#include <GL/gl.h>")
(ffi:clines "#include \"text-render.h\"")
(ffi:clines "#include <SDL/SDL_image.h>")

(defun check-gl-error ()
  (let ((err (c :int "glGetError()")))
    (unless (c :int "#0 == GL_NO_ERROR" :int err)
      (error "OpenGL error ~D" err))))

(defstruct gltexobj texid width height)

(defun bind-texobj (obj)
  (c "glBindTexture(GL_TEXTURE_2D, #0)" :int (gltexobj-texid obj))
  (c "glMatrixMode(GL_TEXTURE)")
  (c "glLoadIdentity()")
  (c "glScaled(1.0/(#0), 1.0/(#1), 1.0)" :int (gltexobj-width obj) :int (gltexobj-height obj)))

(defun alloc-texid ()
  (ffi:c-inline () () :unsigned-int
   " { GLuint id; glGenTextures(1, &id); @(return 0)=id; }"))

(defun upload-sdl-surface (surface)
  (let ((id (alloc-texid)))
    (c "glBindTexture(GL_TEXTURE_2D, #0)" :unsigned-int id)
    (ffi:c-inline (surface) (:pointer-void) (values)
     "{ SDL_Surface *surf = #0; 
        int mode = (surf->format->BytesPerPixel==3)? GL_RGB : GL_RGBA;
        glTexImage2D(GL_TEXTURE_2D, 0, mode, surf->w, surf->h, 0, mode, GL_UNSIGNED_BYTE, surf->pixels); 
      }")
    (check-gl-error)
    (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)")
    (c "glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)")
    (check-gl-error)
    id))

(defstruct (texture (:include gltexobj)))

(defun load-texture-file (filename)
  (let ((surface (call :pointer-void "IMG_Load" :cstring filename)))
    (prog1 (make-texture :texid (upload-sdl-surface surface)
                         :width  (c :int "((SDL_Surface *)#0)->w" :pointer-void surface)
                         :height (c :int "((SDL_Surface *)#0)->h" :pointer-void surface))
      (call "SDL_FreeSurface" :pointer-void surface))))

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

(defun draw-tile (x0 y0 x1 y1 tx ty)
  (c "glBegin(GL_QUADS)")
  (do-draw-tile x0 y0 x1 y1 tx ty)
  (c "glEnd()"))

(defun set-tile-color (r g b a)
  (call "glColor4ub" :unsigned-byte r :unsigned-byte g :unsigned-byte b :unsigned-byte a))

(defun render-starfield (x y)
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
  (draw-tile 0 0 (c :int "window_width") (c :int "window_height") (round (* 0.666 x)) (round (* 0.666 y)))
)

(defun paint-begin ()
;;  (c "glClearColor(#0, #1, #2, 0.0)" :float (random 1.0) :float (random 1.0) :float (random 1.0))
;;  (c "glClear(GL_COLOR_BUFFER_BIT)")
  (c "glDisable(GL_DEPTH_TEST)")
  (c "glDisable(GL_CULL_FACE)")
  (c "glEnable(GL_BLEND)")
  (c "glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)")
  (c "glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)")

  (c "glMatrixMode(GL_PROJECTION)")
  (c "glLoadIdentity()")
  (c "glOrtho(0, window_width, window_height, 0, -1, 1)")
  (check-gl-error))

(defun paint-finish ()
  (call "glFlush")
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
    ((img-surface img) (call "SDL_FreeSurface" :pointer-void (img-surface img)))
    ((img-pixels img)  (call "free" :pointer-void (img-pixels img)))
    (t (warn "Attempt to free IMG ~A with no underlying surface or pixel pointer. Odd." img)))
  (when img
    (setf (img-surface img) nil
          (img-pixels img) nil)))

(defun load-image-file (filename &optional x-offset y-offset)
  (format t "~&Loading ~A~%" filename)
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
  (unless (img-resident-p img)
    (packset-ensure *packset* img))
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

(defun fill-rect (x0 y0 x1 y1 r g b a)
  (call "glColor4ub" :unsigned-byte r :unsigned-byte g :unsigned-byte b :unsigned-byte a)
  (c "glDisable(GL_TEXTURE_2D)")
  (c "glBegin(GL_QUADS)")
  (ffi:c-inline (x0 y0 x1 y1) (:int :int :int :int) (values)
                " { glVertex2i(#0,#1); glVertex2i(#2,#1); glVertex2i(#2,#3); glVertex2i(#0,#3); }")
  (c "glEnd()")
  (c "glEnable(GL_TEXTURE_2D)")
  (values))

(defun draw-bar (left-img right-img fill-tile x y width)
  (let ((left-x (img-width left-img))
        (right-x (- width (img-width right-img)))
        (fill-y (img-height left-img)))
  (draw-img right-img (+ x right-x (img-x-offset left-img)) (+ y (img-y-offset left-img)))
  (draw-img left-img (+ x (img-x-offset left-img)) (+ y (img-y-offset left-img)))
  (bind-texobj fill-tile)
  (draw-tile (+ x left-x) y (+ x right-x) (+ y fill-y) 0 0)))

;;;; Text renderer

(defun render-label (owner face height string &key (align-x :left) (align-y :baseline))
  (let* ((facenum (ecase face
                    (:sans 0)
                    (:gothic 1)))
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
    img))

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
  (ffi:c-inline ((packset-texid ps) (packset-width ps) (packset-height ps)) (:int :int :int) (values)
                "{ void *tmp = calloc(#1*#2, 4);
                   glBindTexture(GL_TEXTURE_2D, #0);
                   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, #1, #2, 0, GL_RGBA, GL_UNSIGNED_BYTE, tmp);
                   free(tmp);
                 } ")
  (check-gl-error)
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
    ;; Upload if the object fits..
    ((packset-fit packset object)
     (vector-push-extend object (packset-images packset))
     (packset-upload-object object))
    ;; Otherwise, repack.
    (t (vector-push-extend object (packset-images packset))
       (packset-repack-for-object packset object))))

(defun packset-upload-object (object)
  ;; Beware! Must bind the packset texture before calling this!
  (let ((x (img-x object))
        (y (img-y object))
        (width (img-width object))
        (height (img-height object)))
    (assert (not (img-is-free? object)))
    #+NIL
    (format t "~&Uploaded ~A at (~D,~D) [~Dx~D]~%" object x y width height)
    (c "glTexSubImage2D(GL_TEXTURE_2D, 0, #0, #1, #2, #3, GL_RGBA, GL_UNSIGNED_BYTE, #4)"
       :int x :int y :int width :int height :pointer-void (img-pixels object))
    (check-gl-error)))

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
  (packset-reset ps)  
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
     (packset-reset ps)
     (error "I'm one lazy fucker, right? Still need to allocate ~A" object))))

(defun debug-show-packset ()
  (fill-rect 64 64 (+ 64 (packset-width *packset*)) (+ 64 (packset-height *packset*)) 0 0 0 255)
  (set-tile-color 255 255 255 255)
  (draw-tile 64 64 (+ 64 (packset-width *packset*)) (+ 64 (packset-height *packset*)) 0 0))

