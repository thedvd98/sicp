;; Chapter 3.2.3
;; ex 2.56
;; ex 2.57
;; ex 2.58

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond
   ((eq? a1 0) a2)
   ((eq? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else
    (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond
   ((eq? a1 0) 0)
   ((eq? a2 0) 0)
   ((eq? a1 1) a2)
   ((eq? a2 1) a1)
   ((and (number? a1) (number? a2)) (* a1 a2))
   (else
    (list '* a1 a2))))

(define (make-exponentiation a1 a2)
  (cond
   ((eq? a1 0) 0)
   ((eq? a2 0) 1)
   ((eq? a2 1) a1)
   (else
    (list '** a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))
(define (product? x)
  (and (pair? x) (eq? '* (car x))))
(define (exponentiation? x)
  (and (pair? x) (eq? '** (car x))))


(define (addend x)
  (and (sum? x) (cadr x)))
(define (augend x)
  (cond
   ((pair? (cdddr x))
    (make-sum (caddr x) (augend (cdr x))))
   (else
    (caddr x))))

(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (cond
   ((pair? (cdddr p))
    (make-product (caddr p) (multiplicand (cdr p))))
   (else
    (caddr p))))

(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))

(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp)
    (if (same-variable? exp var) 1 0))
   ((sum? exp)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
   ((product? exp)
    (make-sum
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))))
   ((exponentiation? exp)
    (make-product (exponent exp)
                  (make-exponentiation (base exp)
                                       (make-sum (base exp) (- 1))))
    )
   (else
    (print "Error"))))
