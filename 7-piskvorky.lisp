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



      ; Vrátí objekt reprezentující rámeček okolo políčka
      (defun border ()
         (let ((border (make-instance 'polygon))
               (A (make-point 0 0))
               (B (make-point 0 *width*))
               (C (make-point *width* *width*))
               (D (make-point *width* 0)))

           (set-items border (list A B C D))
           border))

(defun krizek ()
  (let ((pic (make-instance 'picture))
        (half (/ *width* 2))
        (pol1 (make-instance 'polygon))
        (pol2 (make-instance 'polygon)))
    (set-items pol1 (list (make-point (- half) 0) (make-point half 0)))
    (set-items pol2 (list (make-point 0 (- half)) (make-point 0 half)))

    (set-thickness pol1 (/ half 3))  
    (set-thickness pol2 (/ half 3))
    (set-items pic (list pol1 pol2))
    pic))
  



       ; Vrátí objekt reprezentující značku prvního hráče - křížek
       (defun cross ()
         (let* ((cross (make-instance 'picture))
                (l1 (make-instance 'polygon))
                (l2 (make-instance 'polygon)))

           (let* ((a (/ *field-width* 5))
                  (b (- *field-width* a)))
             (set-items l1 (list (make-point a a) (make-point b b)))
             (set-items l2 (list (make-point a b) (make-point b a)))
      
             (set-thickness l1 a)
             (set-thickness l2 a))

           (set-color cross :red)
           (set-propagate-color-p cross t)
           (set-items cross (list l1 l2))
           cross))

       ; Vrátí objekt reprezentující značku druhého hráče - kolečko
       (defun kolco ()
         (let* ((c (make-instance 'circle))
                (half (/ *field-width* 2))
                (tenth (/ *field-width* 10))
                (s (make-point half half)))

           (set-center c s)
           (set-radius c (/ (+ half tenth) 2))
           (set-thickness c tenth)
           (set-color c :green)
           c))

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
(defun deg2Rad (degrees)
  (* (/ degrees 180.0) pi))

(defun rad2Deg (radians)
  (* (/ radians pi) 180.0))



;; POUŽITÍ

; WINDOW musí být globální!
(defvar *win*)
(setf *win* (make-instance 'window))


(set-shape *win* (border))
(set-shape *win* (krizek))
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
