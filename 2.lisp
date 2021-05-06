
(defun square-list (items)
  (if (null items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
