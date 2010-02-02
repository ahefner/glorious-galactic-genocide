(in-package :g1)

(defvar *ship-types* (make-hash-table :test 'equal))

(defun find-ship-type (name) (gethash name *ship-types*))

(defmacro define-ship-type ((class) &rest initargs)
  `(setf (gethash ,(getf initargs :name) *ship-types*)
         (make-instance ',class ,@initargs)))

(defun weapon-mount (name) (make-instance 'weapon-mount :name name))
(defun battery-mount (name) (make-instance 'battery-mount :name name))
(defun special-mount (name) (make-instance 'special-mount :name name))

(define-ship-type (small-ship)
    :name "Interceptor"
    :slots (list 
            (weapon-mount "Port Weapon")
            (weapon-mount "Starboard Weapon")
            (special-mount "Accessory Mount")))

(define-ship-type (medium-ship)
    :name "Scout"
    :slots (list (weapon-mount "Port Weapon")
                 (weapon-mount "Starboard Weapon")
                 (battery-mount "Forward Battery")
                 (special-mount "Equipment Bay"))) 

(define-ship-type (large-ship)
    :name "Modular Cruiser"
    :slots (list (battery-mount "Forward Port Battery")
                 (battery-mount "Forward Starboard Battery")
                 (battery-mount "Aft Port Battery")
                 (battery-mount "Aft Starboard Battery")
                 (special-mount "Equipment Bay")
                 (special-mount "Secondary Equipment Bay")))

(define-ship-type (large-ship)
    :name "Cruiser"
    :slots (list (battery-mount "Forward Battery")
                 (battery-mount "Port Battery")
                 (battery-mount "Starboard Battery")
                 (special-mount "Equipment Bay")
                 (special-mount "Secondary Equipment Bay")))

(define-ship-type (huge-ship)
    :name "Battleship"
    :slots (list (battery-mount "Primary Battery")
                 (battery-mount "Aft Battery")
                 (battery-mount "Forward Port Battery")
                 (battery-mount "Forward Starboard Battery")
                 (battery-mount "Aft Port Battery")
                 (battery-mount "Aft Starboard Battery")
                 (special-mount "Primary Bay")
                 (special-mount "Secondary Bay")
                 (special-mount "Cargo Bay")))



