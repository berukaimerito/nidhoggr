;; unix wc

(defun whitespacep (character)
  (member character '(#\Space #\Tab #\Return #\Newline)))

(defun wc (filename)
  (with-open-file (stream filename :if-does-not-exist nil)
    (if (null stream)
        (format *error-output* "~S: file does not exist~%" filename)
        (loop with lines = 0
              with words = 0
              with chars = 0
              with reading-whitespace-p = t
              for character = (read-char stream nil)
              while (not (null character))
              do (incf chars)
                 (when (char= character #\Newline)
                   (incf lines))
                 (when (and (null reading-whitespace-p)
                            (whitespacep character))
                   (setf reading-whitespace-p t))
                 (when (and reading-whitespace-p
                            (not (whitespacep character)))
                   (incf words)
                   (setf reading-whitespace-p nil))
              finally (return (values lines words chars))))))

;; unix cat

(defun cat (direction &rest files)
  (flet ((perform ()
           (with-output-to-string (output)
             (dolist (file files)
               (with-open-file (stream file :if-does-not-exist nil)
                 (if (null stream)
                     (format *error-output* "~S: file does not exist~%"
                             file)
                     (loop for line = (read-line stream nil)
                           while (not (null line))
                           with direction = (or direction output)
                           do (format direction "~A~%" line))))))))
    (let ((result (perform)))
      (if direction nil result))))

;; partition

(defun partition (predicates values)
  (flet ((select (predicate) (remove-if-not predicate values)))
    (let ((result (mapcar #'select predicates)))
      (values-list result))))

;; flatten

(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (nconc (flatten (car x))
                  (flatten (cdr x))))))

;; zip

(defun zip (&rest lists)
  (loop for i below (length (first lists))
        for row = (mapcar (lambda (list) (nth i list))
                          lists)
        collect row))

(defun zip-2 (list-1 list-2)
  (mapcar #'list list-1 list-2))

(defun zip-3 (list-1 list-2 list-3)
  (mapcar #'list list-1 list-2 list-3))

(defun zip-4 (list-1 list-2 list-3 list-4)
  (mapcar #'list list-1 list-2 list-3 list-4))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

;; braid and nbraid

(defparameter *foo* (list 1 2 3 4))
(defparameter *bar* (list :a :b :c :d))
(defparameter *baz* (list "q" "w" "e" "r"))

(defun braid (&rest lists)
  (loop for i below (length (first lists))
        for row = (mapcar (lambda (list) (nth i list))
                          lists)
        nconc row))

(defun braid (&rest lists)
  (apply #'mapcan #'list lists))

(defun nbraid (&rest lists)
  ;; do not allocate ANY new memory
  (loop repeat (* (length lists) (length (first lists)))
          initially (nconc lists lists)
        with result = (first lists)
        for sublist on lists
        do (let ((value-1 (cdar sublist))
                 (value-2 (cadr sublist)))
             (setf (car sublist) value-1
                   (cdar sublist) value-2))
        finally (return result)))
