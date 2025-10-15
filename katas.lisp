(defun is-square? (n)
  (if (< 0 n)
      nil
      (= n (* (isqrt n) (isqrt n)))))

(defun cantor (nested-list)
  (loop :for i :below (length nested-list)
        :for diagonal-element = (nth i (nth i nested-list))
        :collect (if (zerop diagonal-element) 1 0)))

(defun potatoes (p0 w0 p1)
  (let* ((dry-matter-kg (/ (* w0 (- 100 p0)) 100))
         (dry-matter-percentage (- 100 p1))
         (final-weight (/ (* dry-matter-kg 100) dry-matter-percentage)))
    final-weight))

(defun potatoes (p0 w0 p1)
  (truncate (/ (* w0 (- 100 p0)) (- 100 p1))))

(defun f (x y z)
    (let ((x-axis (* x (+ 1 y) (+ 1 z)))
          (y-axis (* y (+ 1 x) (+ 1 z)))
          (z-axis (* z (+ 1 x) (+ 1 y))))
      (+ x-axis y-axis z-axis)))

(setq *read-default-float-format* 'double-float)
(defun max-ball (v0)
  (flet ((convert-to-m/s (v)
           (/ (* v 1000.0) 3600.0)))
    (let* ((v-m/s (convert-to-m/s v0))
           (g 9.81)
           (t-max (/ v-m/s g)))
      (round (* t-max 10)))))

(d)

(defun sum-powered-digits (digits start-power)
  "Sum each digit raised to an incrementing power starting from START-POWER."
  (loop :for digit :in digits
        :for power :from start-power
        :sum (expt digit power)))


(defun dig-pow (n p)
  (let* ((digits (map 'list #'digit-char-p (write-to-string n)))
         (sum (sum-powered-digits digits p))
         (remainder (mod sum n)))
    (if (zerop remainder)
        (/ sum n)
        -1)))

(defun indivisible-by-p (n divisors)
  "Returns T if N is not divisible by any number in DIVISORS."
  (notany (lambda (d) (zerop (mod n d))) divisors))

(defun real-numbers (n)
  ;; times out
  (loop :for i :from 1 :to n
        :count (indivisible-by-p i '(2 3 5))))

(defun expressions-matter (a b c)
  "Return the highest achievable result following the rules."
  ;; We check all valid combinations without reordering a, b, and c.
  (max (+ a b c)       ; a + b + c
       (* a b c)       ; a * b * c
       (+ (* a b) c)   ; (a * b) + c
       (+ a (* b c))   ; a + (b * c)
       (* (+ a b) c)   ; (a + b) * c
       (* a (+ b c)))) ; a * (b + c)
