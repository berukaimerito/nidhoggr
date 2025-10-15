(defun list-substract (list)
  (apply #'mapcar (lambda (a b) (- a b)) list)
  )
