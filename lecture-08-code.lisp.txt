;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; OOP - kód k přednášce 13. listopadu 2014
;;;; Algebraické výrazy
;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída expression
;;;


(defclass expression ()
  ())

;; U metod deriv a expr-subst požadujeme, aby je potomci přepisovali,
;; u representation a simplify ne (zdůvodnění v textu).

(defmethod deriv ((expr expression) var)
  (error "Method deriv has to be rewritten"))

(defmethod expr-subst ((expr expression) var substituent)
  (error "Method expr-subst has to be rewritten"))

(defmethod representation ((expr expression))
  expr)

(defmethod simplify ((expr expression))
  expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída const
;;;

(defclass const (expression)
  ((value :initform 0 :initarg :value :reader value)))

(defmethod expr-subst ((expr const) var substituent)
  expr)

(defmethod deriv ((expr const) var)
  (make-instance 'const))

(defmethod representation ((expr const))
  (value expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída var
;;;

(defclass var (expression)
  ((name :initform 'x :initarg :name :reader name)))

(defmethod deriv ((expr var) var)
  (make-instance 'const :value (if (eql (name expr) 
                                        (name var)) 
                                   1 
                                 0)))

(defmethod expr-subst ((expr var) var substituent)
  (if (eql (name expr)
	   (name var))
      substituent
      var))

(defmethod representation ((expr var))
  (name expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída binary-expression a potomci
;;;

(defclass binary-expression (expression)
  ((expr-1 :initform (make-instance 'const) :initarg :expr-1 :reader expr-1)
   (expr-2 :initform (make-instance 'const) :initarg :expr-2 :reader expr-2)))

(defmethod expr-subst ((expr binary-expression) var substituent)
  (make-instance (type-of expr)
                 :expr-1 (expr-subst (expr-1 expr) var substituent)
                 :expr-2 (expr-subst (expr-2 expr) var substituent)))

(defmethod simplify ((expr binary-expression))
  (parse `(,(bin-expr-symbol expr)
	   ,(simplify (expr-1 expr))
	   ,(simplify (expr-2 expr)))))

(defmethod representation ((expr binary-expression)) 
  `(,(bin-expr-symbol expr)
    ,(representation (expr-1 expr))   
    ,(representation (expr-2 expr))))

(defmethod bin-expr-symbol ((expr binary-expression))
  (error "Method bin-expr-symbol has to be rewritten."))

(defclass +-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr +-expr))
  '+)

(defmethod zero-const-p ((expr expression))
  nil)

(defmethod zero-const-p ((expr const))
  (zerop (value expr)))

(defmethod simplify ((expr +-expr))
  (let* ((result (call-next-method))
	 (expr-1 (expr-1 result))
	 (expr-2 (expr-2 result)))
    (cond ((zero-const-p expr-1) expr-2)
	  ((zero-const-p expr-2) expr-1)
	  (t result))))

(defclass --expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr --expr))
  '-)

(defclass *-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr *-expr))
  '*)

(defmethod one-const-p ((e expression))
  nil)

(defmethod one-const-p ((e const))
  (= (value e) 1))

(defmethod simplify ((expr *-expr))
  (let* ((result (call-next-method))
	 (expr-1 (expr-1 result))
	 (expr-2 (expr-2 result)))
    (cond ((one-const-p expr-1) expr-2)
	  ((one-const-p expr-2) expr-1)
          ((or (zero-const-p expr-1)
               (zero-const-p expr-2)) (parse 0))
	  (t result))))

(defmethod deriv ((expr *-expr) var)
  (let ((expr-1 (expr-1 expr))
	(expr-2 (expr-2 expr)))
    (parse `(+ (* ,(deriv expr-1 var) ,expr-2)
	       (* ,expr-1 ,(deriv expr-2 var))))))

(defclass /-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr /-expr))
  '/)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funkce parse
;;;

(defvar *const-expr-class* 'const)
(defvar *var-expr-class* 'var)
(defvar *+-expr-class* '+-expr)
(defvar *--expr-class* '--expr)
(defvar **-expr-class* '*-expr)
(defvar */-expr-class* '/-expr)

(defun make-binary-expr (name expr-1 expr-2)  
  (make-instance (case name
                   (+ *+-expr-class*)
                   (- *--expr-class*)
                   (* **-expr-class*)
                   (/ */-expr-class*))
                 :expr-1 (parse expr-1)
                 :expr-2 (parse expr-2)))

(defun make-const (value)
  (make-instance *const-expr-class* :value value))

(defun make-var (name)
  (make-instance *var-expr-class* :name name))

(defun parse (repr)  
  (typecase repr
    (number (make-const repr))
    (symbol (make-var repr))  
    (list (apply 'make-binary-expr repr))
    (expression repr)
    (t (error "Nečitelná reprezentace výrazu"))))


#|

;; Testy:
(representation (parse '(* x y)))
(representation (deriv (parse '(* x y)) (parse 'x)))
(representation (simplify (deriv (parse '(* x y)) (parse 'x))))
|#

