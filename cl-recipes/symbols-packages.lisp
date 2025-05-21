
;; If you tried to use this value using a variable named temp you will face with an issue.
(defmacro swap (var-1 var-2)
  '(let ((temp var-1))
    (setf ,var-1 ,var-2
      ,var-2 temp))
  (values))
