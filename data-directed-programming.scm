(define (square x)
  (* x x))
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; a "table"
(define table '())
;; implemented in section 3.3.3
(define (put op type item)
  (set! table (cons (list op type item) table)))
(define (get op type)
  (define (search op type li)
    (let ((hd (car li)))
      (cond ((and
              (eq? op (car hd))
              (eq? type (cadr hd)))
             (caddr hd))
            (else
             (search op type (cdr li))))))
  (search op type table))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part 'rectangular real-part)
  (put 'imag-part 'rectangular imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part 'polar real-part)
  (put 'imag-part 'polar imag-part)
  (put 'magnitude 'polar magnitude)
  (put 'angle 'polar angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



;; ex 2.73

(define table '())

(define (install-sum-package)
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  (put 'deriv '+ (lambda (exp var) (make-sum (deriv (addend exp) var)
                                             (deriv (augend exp) var)))))

(define (install-product-package)
  (define (multiplier x) (car x))
  (define (multiplicand x) (cadr x))
  (define (make-product a1 a2) (list '* a1 a2))
  (put 'deriv '* (lambda (exp var) (make-sum
                                    (make-product (multiplier exp)
                                                  (deriv (multiplicand exp) var))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp))))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))
#|
ex 2.74
Insatiable Inc

Differents data structures...
division1: (('pippo '(salary 10) '(address 1998)) ('pluto '(salary 10) '(address 1998)))
division2: (('hello '(address 844) '(salary 844)) ('hi '(address 99999) '(salary 10189)))


tagged with division id

|#

(define table '())
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (put op type item)
  (set! table (cons (list op type item) table)))

(define (get op type)
  (define (searchtable op type li)
    (if (null? li)
        '()
        (let ((hd (car li)))
          (cond ((and
                  (eq? op (car hd))
                  (eq? type (cadr hd)))
                 (caddr hd))
                (else
                 (searchtable op type (cdr li)))))))
  (searchtable op type table))

(define (install-division1)
  (define (get-record employee file)
    (define (search employee file)
      (let ((record (if (null? file) '() (car file))))
        (cond
         ((null? record) '())
         ((eq? employee (car record)) (attach-tag 'division1 record))
         (else (search employee (cdr file))))))
    (search employee file))

  (define (get-salary employee file)
    'TODO
    )
  (put 'get-record 'division1 get-record)

  'done)

(define (install-division2)
  (define (get-record employee file)
    (define (search employee file)
      (let ((record (if (null? file) '() (car file))))
        (cond
         ((null? record) '())
         ((eq? employee (car record)) (attach-tag 'division2 record))
         (else (search employee (cdr file))))))
    (search employee file))

  (put 'get-record 'division2 get-record)

  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
;; a.
(define (get-record employee-name file)
  (let ((type (type-tag file))
        (record ((get 'get-record (type-tag file)) employee-name (contents file))))
    (if (null? record)
        '()
        record)))

;; b.
(define (get-salary employee-name file)
  (let ((record (get-record employee-name file)))
    ((get 'get-salary (type-tag record)) employee-name record)))

;; c.
(define (find-employee-record employee-name file-list)
  (define (search-record name file)
    (get-record name file))
  (define (find-in-files name file-list)
    (if (null? file-list)
        '()
        (let ((record (search-record name (car file-list))))
          (if (null? record )
              (find-in-files name (cdr file-list))
              record))))
  (find-in-files employee-name file-list))

;; d. ?

(define (test-ex274)
  (define file1 (list 'division1 '(pippo (salary 10) (address 1998)) '(pluto (salary 10) (address 1998)) '(alfonso (salary 8888) (address 9))))
  (define file2 (list 'division2 '(hello (address 844) (salary 844)) '(hi (address 99999) (salary 10189))))

  (set! table '())
  (install-division1)
  (install-division2)
  (print (get-record 'pippo file1))
  (print (get-record 'pluto file1))
  (print (get-record 'notexist file1))
  (print (get-record 'notexist file2))
  (print (get-record 'pluto file2))
  (print (get-record 'hello file2))

  (print (find-employee-record 'pippo (list file1 file2)))
  (print (find-employee-record 'notexist (list file1 file2)))

  )

