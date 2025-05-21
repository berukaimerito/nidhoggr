(defmacro my-when (condition &body body)
  `(if ,condition
       (progn ,@body))) ;;
