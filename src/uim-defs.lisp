(in-package :g1)

(ffi:clines "#include <SDL/SDL.h>")

(defstruct uic
  ;; Bounding rectangle, in absolute screen coordinates. OpenGL will
  ;; have transformation applied so that abx/aby is the origin for
  ;; drawing operations.
  abx aby width height
  ;; Pointer position. amx/amy are in absolute pixel
  ;; coordinates. mx/my are transformed relative to abx/aby.
  amx amy mx my
  ;; Button and modifier state
  buttons
  buttons-pressed
  buttons-released
  modifiers
  modifiers-pressed
  modifiers-released
  time delta-t)

(defclass gadget ()
  ((next :accessor next-gadget :initarg :next-gadget :initform nil)))

(defmacro keysym (name)
  `(cx :int ,(format nil "SDLK_~A" (if (= 1 (length (string name))) 
                                       (string-downcase (string name))
                                       name))))

