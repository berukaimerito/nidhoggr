(defpackage :utils
  (:use :cl :alexandria :split-sequence))

(in-package :utils)

(defun str->digit-list (n)
  (map 'list #'digit-char-p (int->str n)))

(defun int->str (n)
  (write-to-string n))

(defun divisible? (n d)
  (zerop (mod n d)))

(defun obj->2d-vector (&optional file-path)
  )

(defun sum-powered-digits (digits start-power)
  "Sum each digit raised to an incrementing power starting from START-POWER."
  (loop :for digit :in digits
        :for power :from start-power
        :sum (expt digit power)))

(defun indivisible-by-p (n divisors)
  "Returns T if N is not divisible by any number in DIVISORS."
  (notany (lambda (d) (divisible? n d)) divisors))

(defun split-file-by-newline (filename)
  (let* ((raw (alexandria:read-file-into-string filename))
         (lines (ppcre:split "\\n\\n" raw)))
    lines))

(defun str->str-array (input))

(defun filter-whitespace (str)
  (remove-if #'(lambda (c) (member c '(#\space #\tab))) str))

(defun split-by-newline (str)
  (split-sequence:split-sequence #\Newline str :remove-empty-subseqs t))

(defun empty-string? (str)
  (zerop (length str)))

(defun load-file (file-path)
  "Returns non-empty lines from a fine in a list."
  (when (probe-file file-path)
    (with-open-file (stream file-path)
      (loop :for line = (read-line stream nil nil)
            :while line
            :when (not (empty-string? line))
            :collect line))))

(defun build-2D-grid (filename)
  (let* ((raw-chars (alexandria:read-file-into-string filename))
         (clean-input (split-by-newline raw-chars))
         (string-array (mapcar (lambda (x) (coerce x 'list)) clean-input))
         (2d-grid (make-array (list (length (car string-array)) (length string-array))
                              :initial-contents string-array)))
    2d-grid))

(defun in-bounds? (grid i j)
  (array-in-bounds-p grid i j))
