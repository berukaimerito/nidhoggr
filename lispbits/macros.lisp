(defun eval-rules ()
  (let ((x 10)
        (y 20)
        (numbers '(5 6 8))
        (list1 '(a b c))
        (list2 '(d e f)))
    ;; ` -> Backquote. It allows slective evaluation within the quoted expression.
    ;; , -> Comma. Within the backquoted expression the comma evaluates the following expression.
    ;; Regular quoted expressions ('expr) prevent evaluation
    ;; Backquoted expressions (`expr) quote the expression but allow selective evaluation
    ;; ,expr within backquote evaluates expr
    ;; ,@expr within backquote evaluates expr (which must be a list) and splices its elements   ;;
    (values
     `(x y)
     `(,x ,y)
     `(start ,@numbers end)
     `(combined ,@list1 ,@list2))
    ;; (x y) This will trigger an error.
    ))

(defmacro with-multiple-values (vars form &body body)
  `(multiple-value-bind ,vars ,form ,@body))

(defun build-function-call (function-name args)
  `(,function-name ,@args))
