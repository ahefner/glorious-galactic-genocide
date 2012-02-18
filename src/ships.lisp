(in-package :g1)

(defvar *ship-types* (make-hash-table :test 'equal))

(defun find-ship-type (name) (gethash name *ship-types*))

(defmacro define-ship-type ((class) &rest initargs)
  `(let ((type (orf (gethash ,(getf initargs :name) *ship-types*)
                    (make-instance ',class))))
     (reinitialize-instance type ,@initargs)
     (ensure-numbered-slots type)))

(defun weapon-mount (name) (make-instance 'weapon-mount :name name))
(defun battery-mount (name) (make-instance 'battery-mount :name name))
(defun special-mount (name) (make-instance 'special-mount :name name))

(defun slots-list (&rest args)
  (list*
   (make-instance 'engine-mount :name "Engine")
   (make-instance 'hull-mount :name "Hull")
   args))

(define-ship-type (small-ship)
    :name "Interceptor"
    :slots (slots-list
            (weapon-mount "Port Weapon")
            (weapon-mount "Starboard Weapon")
            (special-mount "Accessory Mount"))
    :schematic-position (v2 35 75))

(define-ship-type (medium-ship)
    :name "Scout"
    :slots (slots-list
            (weapon-mount "Port Weapon")
            (weapon-mount "Starboard Weapon")
            (battery-mount "Forward Battery")
            (special-mount "Equipment Bay"))
    :schematic-position (v2 130 88))

(define-ship-type (large-ship)
    :name "Modular Cruiser"
    :meters 442
    :slots (slots-list
            (battery-mount "Port Forward Battery")
            (battery-mount "Starboard Forward Battery")
            (battery-mount "Port Aft Battery")
            (battery-mount "Starboard Aft Battery")
            (special-mount "Equipment Bay")
            (special-mount "Secondary Equipment Bay"))
    :schematic-position (v2 162 56))

(define-ship-type (large-ship)
    :name "Cruiser"
    :slots (slots-list
            (battery-mount "Forward Battery")
            (battery-mount "Port Battery")
            (battery-mount "Starboard Battery")
            (special-mount "Equipment Bay")
            (special-mount "Secondary Equipment Bay"))
    :schematic-position (v2 100 67))

(define-ship-type (huge-ship)
    :name "Battleship"
    :meters 1065
    :slots (slots-list
            (battery-mount "Primary Battery")
            (battery-mount "Aft Battery")
            (battery-mount "Port Forward Battery")
            (battery-mount "Starboard Forward Battery")
            (battery-mount "Port Aft Battery")
            (battery-mount "Starboard Aft Battery")
            (special-mount "Primary Bay")
            (special-mount "Secondary Bay")
            (special-mount "Cargo Bay"))
    :schematic-position (v2 13 117))



