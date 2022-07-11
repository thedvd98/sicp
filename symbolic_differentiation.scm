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

;;

(define (prefix->infix exp)
  (if (not (pair? exp))
      exp
      (let ((op (car exp))
            (a1 (cadr exp))
            (a2 (caddr exp)))
        (list (prefix->infix a1) op (prefix->infix a2)))))

(define (infix->prefix exp)
  (if (not (pair? exp))
      exp
      (let ((op (cadr exp))
            (a1 (car exp))
            (a2 (caddr exp)))
        (list op (infix->prefix a1) (infix->prefix a2)))))

(define (getop exp)
  (cond
   ((null? exp) '())
   ((or (number? exp) (symbol? exp)) '())
   ((null? (cdr exp)) '())
   (else (cadr exp))))

(define (put-parenthesis exp)
  (define (iter exp)
    (cond
     ((null? exp) '())
     ((eq? (getop exp) '+)
      (print "piú: " exp)
      (list
       (iter (car exp))
       '+ (iter (cddr exp))))
     ((eq? (getop exp) '*)
      (print "per: " exp)
      (let ((product (list
                      (iter (car exp))
                      '*
                      (iter (caddr exp)))))
        (print "product: " product)
        (cons product (iter (cons '() (cdddr exp))))))
     ((eq? (getop exp) '**)
      (let ((e (list (car exp) '** (caddr exp))))
        (iter (cons e (cdddr exp)))))
     ((or (number? exp) (variable? exp))
      (print "number: " exp)
      exp)
     ((pair? (car exp))
      (print "ciao " (car exp))
      (iter (car exp)))
     (else
      exp)))
  (iter exp))

(define (put-parens exp op)
  (define (iter exp op)
    (cond
     ((null? exp) '())
     ((or (number? exp) (symbol? exp)) exp)
     ((not (pair? (cdr exp))) (car exp)) ;; when free last elem
     ((eq? (cadr exp) op)
      (let
          ((a (list
               (iter (car exp) op)
               (cadr exp)
               (iter (caddr exp) op))))
        (iter (cons a (cdddr exp)) op)))
     (else
      (list
       (iter (car exp) op)
       (cadr exp)
       (iter (cddr exp) op)))))
  (iter exp op))

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

(define (deriv-infix exp var)
  (prefix->infix (deriv (infix->prefix exp) var)))
