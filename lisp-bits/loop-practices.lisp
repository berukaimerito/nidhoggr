(defun print-loop (list)
  (loop for x on list
        do (format t "what was this ~a~%" x)
         (loop for point in x
                 do (format t "For antenna location on: ~A calculate the distances for locations in -> ~%~a~%" point x))))

;; i need to get calculations for the first collection in the loop for x on list -> x
