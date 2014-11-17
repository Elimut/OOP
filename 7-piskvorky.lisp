(load (current-pathname "micro-graphics/load.lisp"))
(load (current-pathname "07.lisp"))
(load (current-pathname "pomucky.lisp"))

; Konfigurace
(defvar *width* 50)

;; FIELD
(defclass field (picture) 
  ; Hodnoty:
  ;    'X = Křížek     nebo-1/
  ;    nil = Neutrální nebo 0
  ;    'O = Kolečko    nebo 1?
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

;mozna lepe state policka jako -1,0,1 misto nil 'O 'X?
(defmethod set-state ((self field) value) 
  (unless (member value (list nil 'X 'O))
    (error "Hodnota policka musi byt nil, 'O, nebo 'X"))
  
  (let ((xo (cond ((eql value 'X) (krizek))
                  ((eql value 'O) (kolecko))
                  (t (make-instance 'empty-shape)))))
    (set-items self (list xo (border)));oramujeme
    (setf (slot-value self 'state) value)
    self
))

;; POUŽITÍ

; WINDOW musí být globální!
(defvar *win*)
(setf *win* (make-instance 'window))

;vyzkousime jednotlive graf prvky
(set-shape *win* (border))
(set-shape *win* (kolecko))
(set-shape *win* (make-field 'O))
;(set-shape *win* (krizek))

(defvar *board* (make-instance 'picture))
(let ((m 3);velikost M x N, 5 x 4 jeste vejde do okna
      (n 2)
      (fields nil))
(dotimes (x m)
  (dotimes (y n)
;(random 3) vraci 0,1 nebo 2
;vyzkouset loop + collect
    (setf fields (cons
                   (move
                     (make-field (nth (random 3) '(O X nil)))
                     (* x *width*)
                     (* y *width*))
                   fields))
    (set-items *board* fields)  
)))

(set-shape *win* *board*)
(redraw *win*)
