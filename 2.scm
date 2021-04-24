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
