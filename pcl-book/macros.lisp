(defmacro do-primes (condition &rest body)
  `(if ,condition
       (progn ,@body)
       nil))
