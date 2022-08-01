;; 2.3.4

(define example1 '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else
         (print "bad bit " bit))))

;;ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-result (decode sample-message sample-tree))
;; '(A D A B B C A)

;; ex 2.68
(define (contain-symbol? sym l)
  (cond ((null? l) #f)
        ((eq? (car l) sym) #t)
        (else
         (contain-symbol? sym (cdr l)))))
(define (get-symbols branch)
  (cond
   ((null? branch) '())
   ((leaf? branch) '())
   (else
    (caddr branch))))

(define (encode-symbol sym tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond ((null? tree) '())
          ((and (leaf? left)
                (eq? sym (symbol-leaf left))) '(0))
          ((and (leaf? right)
                (eq? sym (symbol-leaf right))) '(1))
          ((and (not (leaf? left))
                (contain-symbol? sym (get-symbols left)))
           (cons 0 (encode-symbol sym left)))
          ((and (not (leaf? right))
                (contain-symbol? sym (get-symbols right)))
           (cons 1 (encode-symbol sym right)))
          (else
           (print "Error Symbol not in the tree")
           '()))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; ex 2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; make-code-tree and make-leaf
(define (successive-merge sets)
  (define (iter sets tree)
    (cond ((null? sets) tree)
          ((null? (cdr sets))
           (make-code-tree (car sets)
                           tree))
          ((= (weight-leaf (car sets))
              (weight-leaf (cadr sets)))
           (iter (cddr sets)
                 (if (not (null? tree))
                  (make-code-tree tree
                                  (make-code-tree (car sets)
                                                  (cadr sets)))
                  (make-code-tree (car sets)
                                  (cadr sets)))))
          (else
           (iter (cdr sets)
                 (make-code-tree (car sets) tree)))))
  (iter sets '()))

(define (successive-merge sets)
  (if (null? (cdr sets))
      (car sets)
      (successive-merge (adjoin-set (make-code-tree (car sets)
                                                    (cadr sets))
                                    (cddr sets)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-pairs '((D 1) (C 1) (B 2) (A 4)))
(define sample-pairs2 '((G 1) (H 1) (E 1) (F 1) (D 1) (C 1) (B 3) (A 8)))

;; ex 2.70
(define rock-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-pairs))
(define song
  '(GET A JOB SHA NA NA NA NA NA NA NA NA
        GET A JOB SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
