;;
(defvar *alist* '((1) (2) (3)))
(defvar *another-alist* '((1 . 2) (3 . 4) (4 . 5)))

(defun print-data-structure (ds)
  (format nil "Data Structure: ~a~%Type of: ~a~%" ds (type-of ds)))

(defun alist-p (lst)
  "Check if LST is an association list (a list of conses)."
  (and (listp lst)
       (every #'consp lst)))
