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


;; ex 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
       (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;; Sets and information retrieval
(define (make-record key info)
  (list key info))

(define (key record)
  (car record))

(define r1 (make-record 1 'ciao))
(define r2 (make-record 2 'record2))
(define r3 (make-record 3 'record3))
(define r4 (make-record 4 'record4))

(define record-tree
  (make-tree r3
             (make-tree r2 (make-tree r1 '() '()) '())
             (make-tree r4 '() '())))


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else
          (lookup given-key (cdr set-of-records)))))

;; ex 2.66
(define (lookup-tree given-key set-of-records)
  (cond
   ((null? set-of-records) #f)
   ((equal? given-key (key (entry set-of-records)))
    (entry set-of-records))
   ((> given-key (key (entry set-of-records)))
    (lookup-tree given-key (right-branch set-of-records)))
   ((< given-key (key (entry set-of-records)))
    (lookup-tree given-key (left-branch set-of-records)))
   (else
    #f)))
