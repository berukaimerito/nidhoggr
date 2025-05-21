
(defun verify-my-eggz ()
  (let ((fns (list
              #'egg-1 #'egg-2))))
  mapcar #'verify)

(defun egg-1 ()
  *egg-hunt*)

(defun egg-2 ()
  *package*)

(defun postorder (tree)
 (reverse (format nil "~{~A~}" (funcall #'flatten tree)) ))

(defun flatten (list)
  (nreverse (%flatten list '())))

(defun %flatten (list accumulator)
  ;; base case if the list is empty, return the accumulator
  (if (null list)
      accumulator
      ;; handle first element
      (if (atom list)
          ;; if it is atomic push it to the accumulator and pass the xdr of the list
          (cons list accumulator)
          ;; if a cons cell, flatten it and process rest
          (%flatten (cdr list) (%flatten (car list) accumulator)))))

(defun egg-3 ()
  (postorder *i-postordered-a-whole-box-of-eggs*))

(defun egg-4 ()
  (macroexpand-1 '*poof-and-its-gone*))

(defun )
