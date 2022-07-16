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

;; Sets as ordered lists of numbers

(define (element-of-set-ord? x set)
  (cond
   ((null? set) #f)
   ((= x (car set)) #t)
   ((< x (car set)) #f)
   (else
    (element-of-set-ord? x (cdr set)))))

(define (intersection-set-ord set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((= (car set1) (car set2))
    (cons (car set1)
          (intersection-set-ord (cdr set1) (cdr set2))))
   ((< (car set1) (car set2))
    (intersection-set (cdr set1) set2))
   ((> (car set1) (car set2))
    (intersection-set set1 (cdr set2)))
   ))

;; ex 2.61
(define (adjoin-set-ord x set)
  (cond
   ((null? set) (list x))
   ((= x (car set)) set)
   ((> x (car set)) (cons (car set) (adjoin-set-ord x (cdr set))))
   ((< x (car set)) (cons x set))))

;;ex 2.62
(define (union-set-ord set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   ((> (car set1) (car set2))
    (cons (car set2) (union-set-ord set1 (cdr set2))))
   ((> (car set2) (car set1))
    (cons (car set1) (union-set-ord (cdr set1) set2)))
   (else
    (cons (car set1)(union-set-ord (cdr set1) (cdr set2))))))


;; Sets as binary trees

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-bin? x set)
  (cond
   ((null? set) #f)
   ((= x (entry set)) #t)
   ((< x (entry set)) (element-of-set-bin? x (left-branch set)))
   ((> x (entry set)) (element-of-set-bin? x (right-branch set)))))


(define (adjoin-bin x set)
  (cond
     ((null? set) (make-tree x '() '()))
     ((= x (entry set)) set)
     ((< x (entry set)) (make-tree
                         (entry set)
                         (adjoin-bin x (left-branch set))
                         (right-branch set)))
     ((> x (entry set)) (make-tree
                         (entry set)
                         (left-branch set)
                         (adjoin-bin x (right-branch set))))))


;; ex 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; Sets and information retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else
         (lookup given-key (cdr set-of-records)))))

