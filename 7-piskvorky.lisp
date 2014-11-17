(load (current-pathname "micro-graphics/load.lisp"))
(load (current-pathname "07.lisp"))
(load (current-pathname "pomucky.lisp"))

; Konfigurace
(defvar *width* 50)
(defvar *fields-in-row* 3)
(defvar *gap-x* 45)
(defvar *gap-y* 15)

;; FIELD
(defclass field (picture) 
  ; Hodnoty:
  ;    -1 = Křížek
  ;     0 = Neutrální
  ;    +1 = Kolečko
  ((state :initform 0)))

; Vytvoří objekt reprezentující políčko piškvorek
(defun make-field (value)
  (let ((field (make-instance 'field)))
    (set-state field value)
    field))

; Gettery & Settery
(defmethod state ((self field))
  (slot-value self 'state))

; Vrátí polygon/rámeček okolo políčka, levy horni roh: 0,0
(defun border ()
         (let ((border (make-instance 'polygon)))
           (set-items border (list (make-point 0 0)
                                   (make-point 0 *width*)
                                   (make-point *width* *width*)
                                   (make-point *width* 0)))
           border))

;levy horni roh: 0,0
(defun krizek ()
  (let ((pic (make-instance 'picture))
        (half (/ *width* 2))
        (pol1 (make-instance 'polygon))
        (pol2 (make-instance 'polygon)))
    (set-items pol1 (list (make-point (- half) 0) (make-point half 0)))
    (set-items pol2 (list (make-point 0 (- half)) (make-point 0 half)))

    (set-thickness pol1 (/ half 3))  
    (set-thickness pol2 (/ half 3))
    (set-color pic :red)
    (set-propagate-color-p pic t);polozky obrazku cervene
    (set-items pic (list pol1 pol2))
    (move 
    (rotate pic (- pi 4) (make-point 0 0));otocime o 45stupnu
      half half);posun: levy horni roh je 0,0
))
     
;graficky objekt hráče - kolečko
(defun kolecko ()
         (let* ((cir (make-instance 'circle))
                (half (/ *width* 2))
                (thick (/ *width* 10))
                (r (-  half thick)))                               
           (set-radius cir r)
           (set-thickness cir thick)
           (set-color cir :green)
           (move cir half half);posun: levy horni roh je 0,0
))

(defmethod set-state ((self field) value) 
  (unless (and (>= value -1) (<= value 1))
    (error "Hodnota policka musi byt -1, 0, nebo 1"))
  
  (let ((value-image (cond ((= value -1) (cross))
                           ((= value +1) (kolco))
                           (t (make-instance 'empty-shape)))))

    (set-items field (list value-image (border)))
    (setf (slot-value self 'value) value)
    self))


; Převody
(defun deg2Rad (degrees) (* (/ degrees 180.0) pi))
(defun rad2Deg (radians) (* (/ radians pi) 180.0))



;; POUŽITÍ

; WINDOW musí být globální!
(defvar *win*)
(setf *win* (make-instance 'window))

;vyzkousime jednotlive graf prvky
(set-shape *win* (border))
(set-shape *win* (kolecko))
;(set-shape *win* (krizek))
(redraw *win*)


#|
(let ((p (make-instance 'picture))
      (fields (list (make-field -1)
                    (make-field 0)
                    (make-field 1)
                    (make-field 1)
                    (make-field -1)
                    (make-field 0)
                    (make-field 0)
                    (make-field 1)
                    (make-field -1)))
      (x 0)
      (y 0))

  (dolist (field fields)
    (move field 
          (* *field-width* x)
          (* *field-width* y))

    (setq x (+ x 1))
    (if (= x *fields-in-row*)
        (progn
          (setq x 0)
          (setq y (+ y 1)))))
          
  (let ((center (make-point (+ *gap-x* (* *fields-in-row* (/ *field-width* 2)))
                            (+ *gap-y* (* y (/ *field-width* 2))))))

    (set-items p fields)

    (move p *gap-x* *gap-y*)
    (scale p 1.1 center)
    (rotate p (deg2rad 45) center)

    (set-shape *win* p)
    (redraw *win*)))
|#
