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

; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond
    ((null? mobile) 0)
    ((number? mobile) mobile)
    (else
      (+ (total-weight
           (cond
             ((null? (left-branch mobile)) 0)
             (else
               (total-weight
                 (branch-structure (left-branch mobile))))))
         (total-weight (cond
                         ((null? (right-branch mobile)) 0)
                         (else
                           (total-weight
                             (branch-structure (right-branch mobile))))))
         ))))

(define (example-binary-mobile)
  (make-mobile
    (make-branch
      2
      (make-mobile
        (make-branch
          999
          (make-mobile
            (make-branch 1 2)
            (make-branch 1 5)
            )
          )
        (make-branch 3 1)
        )
      )
    (make-branch
      4
      (make-mobile
        (make-branch 100 3)
        (make-branch 200 900)))))

(define (balanced? mobile)
  (define (torque branch)
    (*
      (branch-length branch)
      (total-weight (branch-structure branch))))

  (cond
    ((not (pair? mobile)) #t)
    (else
      (and
        (=
          (torque (left-branch mobile))
          (torque (right-branch mobile)))
        (balanced? (branch-structure (left-branch mobile)))
        (balanced? (branch-structure (right-branch mobile)))))))



;; Mapping over trees

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; 2.30
(define (square-tree1 tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (* tree tree))
    (else
      (cons
        (square-tree1 (car tree))
        (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree)
             )
         ) tree))

; 2.31

(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree))
         ) tree))

(define (square-tree tree)
  (tree-map square tree))

; 2.32
; set of all subset

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
       (append
         rest
         (map
           (lambda (x) ;; concatenate the first element with every other element of the rest
             (cons (car s) x))
           rest)))))

;; Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else
          (+
            (sum-odd-squares (car tree))
            (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond
    ((null? sequence) '())
    ((predicate (car sequence))
     (cons (car sequence) (filter predicate (cdr sequence))))
    (else
      (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else
      (append
        (enumerate-tree (car tree))
        (enumerate-tree (cdr tree))))))

(define (sum-odd-square1 tree)
  (accumulate + 0
              (map square
                   (filter odd? (enumerate-tree)))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fibs-squares n)
  (accumulate cons
              '()
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (alternative-map p sequence)
  (accumulate
    (lambda (x y) 
      (append y (list (p x))))
    '()
    sequence
    ))

(define (alternative-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (alternative-length sequence)
  (accumulate 
    (lambda (x y) (+ 1 y))
    0 sequence))
