(defun apply-predicates (predicates elements)
  (let (result)  ; Initialize result as an empty plist
    (loop for predicate in predicates
          do (let ((temp-list '()))  ; Temporary list for current predicate
               (loop for element in elements
                     when (funcall predicate element)  ; Apply the predicate
                       do (push element temp-list))
                 (push (reverse temp-list) result)))  ; Preserve order
    (apply #'values (nreverse result) ) ))  ; Reverse result to maintain predicate order
