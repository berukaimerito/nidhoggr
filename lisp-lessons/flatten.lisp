
(defun flatten (list)
  (nreverse (%flatten list '())))

(defun %flatten (list accumulator)
  ;; base case if the list is empty, return the accumulator
  (if (null list)
      accumulator
      ;; handle first element
      (if (atom list)
          ;; If it is atomic push it to the accumulator and pass the xdr of the list
          (cons list accumulator)
          ;; if a cons cell, flatten it and process rest
          (%flatten (cdr list) (%flatten (car list) accumulator))))
  )
