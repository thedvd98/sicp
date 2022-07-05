(import scheme)
(import (srfi 203))

(define image rogers)

(define (paint painter)
  (let ((a (painter (canvas-frame))))
    (print (canvas-refresh))))

(define (make-vect x y)
  (list x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cadr vect))

(define (add-vect v1 v2)
  (list
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (list
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v1)
  (list
   (* s (xcor-vect v1))
   (* s (ycor-vect v1))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (X)
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define (outline-frame)
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define (diamond-shape)
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

;; Example (paint X)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)    ; new origin at the top
   (make-vect 1.0 1.0)    ; new end of edge1 at top right
   (make-vect 0.0 0.0)))  ; new end of edge2 at bottom left


(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1 0.5)
		     (make-vect 0.5 1)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

