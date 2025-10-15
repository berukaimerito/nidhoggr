(defclass account ()
  ((owner :initarg :owner)
   balance)
  (:default-initargs :owner (alexandria:required-argument :owner)))

(defmethod initialize-instance ((object account) &key)
  )

;; (with-slots)
;; method blongs to function not classes.

