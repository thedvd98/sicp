;ex 2.18

(define (append1 list1 list2)
  (if (null? (cdr list1))
      list2
      (cons (car list1) (append1 (cdr list1) list2))
      )
  )

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
