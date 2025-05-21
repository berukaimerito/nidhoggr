

(defun zip (&rest args)

  (if (or (null args)
          (not (every #'listp args))
          (not (apply #'= (mapcar #'length args))))
      (format nil "The arguments should not be nil or any other type than list"))

  )
