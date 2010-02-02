;;;; This system loads portable (non-UI) parts of G1 into a conformant
;;;; ANSI CL implementation. This is so that I can develop and test
;;;; the game simulation logic using SBCL.

(asdf:defsystem :g1-sim-rig
    :serial t
    :components
    ((:file "package")
     (:file "macrology")
     (:file "globals")
     (:file "util")
     (:file "star-names")
     (:file "sim-defs")
     (:file "math")
     (:file "sim")
     (:file "mapgen")
     (:file "techs")
     (:file "ships")
))

