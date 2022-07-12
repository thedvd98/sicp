(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((eq? (car set) x) #t)
   (else
    (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2)
    (cons (car set1)
          (intersection-set (cdr set1) set2)))
   (else
    (intersection-set (cdr set1) set2))))

;; ex 2.59
(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   ((not (element-of-set? (car set1) set2))
    (cons (car set1) (union-set (cdr set1) set2)))
   (else
    (union-set (cdr set1) set2))))

;; ex 2.60
;; Allow duplicates


(define (element-of-set-dup? x set)
  (cond
   ((null? set) #f)
   ((eq? (car set) x) #t)
   (else
    (element-of-set-dup? x (cdr set)))))

(define (adjoin-set-dup x set)
  (cons x set))


(define (intersection-set-dup set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set-dup? (car set1) set2)
    (cons (car set1)
          (intersection-set-dup (cdr set1) set2)))
   (else
    (intersection-set-dup (cdr set1) set2))))

(define (union-set-dup set1 set2)
  (append set1 set2))

