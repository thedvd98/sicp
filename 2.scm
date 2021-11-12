;ex 2.18

(define (append1 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append1 (cdr list1) list2))))

(define (reverse1 l)
  (if (null? l)
      '()
      (append1 (reverse1 (cdr l)) (list (car l)))))

;ex 2.19
(define us-coins (list 50 25 10 5 1))

(define (no-more? coin-values)
  (if (null? coin-values) #t #f))

(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

;ex 2.20

(define (same-parity a . w)
  (cond ((null? w) '())
        ((and (even? a) (even? (car w)))
              (cons (car w) (apply same-parity a (cdr w))))
        ((and (not (even? a)) (not (even? (car w))))
         (cons (car w) (apply same-parity a (cdr w))))
        (else
          (apply same-parity a (cdr w)))))

(define (map1 proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map1 proc (cdr items)))))


(define (square x)
  (* x x))

;ex 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list1 items)
  (map (lambda (x) (* x x)) items))


;2.22
;He should have used append
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(define (for-each1 proc items)
  (if (null? items)
      #t
      (begin
       (proc (car items))
       (for-each1 proc (cdr items)))))

; 2.27
;; (1 (2 3) 3 (1 2 3))
(define (deep-reverse l)
  (cond
    ((null? l) '())
    ((list? (car l))
     (append1
       (deep-reverse (cdr l))
       (cons (deep-reverse (car l)) '())))
    (else
      (append1 (deep-reverse (cdr l)) (list (car l))))))

;      (append1 (reverse1 (cdr l)) (list (car l)))


;2.28
(define (fringe x)
  (cond
    ((null? x) '())
    ((list? (car x))
     (append
       (fringe (car x))
       (fringe (cdr x))))
    (else
      (cons (car x) (fringe (cdr x)))      
      )))
