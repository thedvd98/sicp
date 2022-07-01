(load "utils.scm")

(define empty-board '())

(define (safe? k positions)
  '())

(define (adjoin-position new-row k rest-of-queen)
  (cons (list new-row k) rest-of-queen))

(define (get-row position)
  (car position))
(define (get-col position)
  (cadr position))

(define (collision-row pos1 pos2)
  (if (= (get-row pos1) (get-row pos2))
      #t
      #f))

(define (collision-oblique pos1 pos2)
  (let ((diff-row (- (get-row pos1) (get-row pos2)))
	(diff-col (- (get-col pos1) (get-col pos2))))
    (if (= (abs diff-row) (abs diff-col))
	#t
	#f)))

(define (safe? k positions)
  (define (safe k rest-positions new-pos)
    (cond
    ((null? rest-positions) #t)
    ((collision-row new-pos (car rest-positions)) #f)
    ((collision-oblique new-pos (car rest-positions)) #f)
    (else
     (safe k (cdr rest-positions) new-pos))))
  (if (null? positions)
      #t
      (safe k (cdr positions) (car positions))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (contain-position? pos positions)
  (cond
   ((null? positions) #f)
   ((equal? pos (car positions)))
   (else
    (contain-position? pos (cdr positions)))))

(define (fact n)
  (if (= 1 n)
      1
      (* n (fact (- n 1)))))
