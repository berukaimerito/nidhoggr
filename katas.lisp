(defun is-square? (n)
  (if (< 0 n)
      nil
      (= n (* (isqrt n) (isqrt n)))))
