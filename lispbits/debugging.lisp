
(defun fib (n)
  ;; (break)
  (if (<= 0 n 1)
      ;; DIVISON-BY-ZERO here
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


(defun sum (xs &optional (acc 0))
  (if (null xs)
      acc
      (sum (cdr xs) (+ (car xs) acc))))

(defstruct resource )
(defvar *malformed-limit-value* nil)

(defun read-malformed-limit-value (limit)
  (alexandria:if-let ))
