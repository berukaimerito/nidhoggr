;; String -> String ; not sure about this
;; Write a function that will mimic the functionality of unix `cat` command.
;; Function reads the content of the files and outputs the content in a provided path(s)
;; (defun cat (flag arg1...argN) (...s))


  ;;  Code should decide if flag is a type of a Nil or T first thing first.
  ;; First read the files
  ;; Open a path. Check if it exist.
  ;; If there is a file in that path. Open the file
  ;; If you open the file read it's content.
  ;; If an error raised through reading such as the permissions did not allow return the error directly with a message
  ;; If you were able to read the file(s) store it in a variable.
  ;; Print the variable if flag is nil.
  ;; Return the variable if flag is T.


;;  is it possible to not load memory if flag is nil?

(defun cat (flag path &rest paths)
  (unless (member flag '(nil t))
    (error "Invalid flag value: ~a. Expected T or NIL." flag))
  ;; Loop through each
  ;; bind the variable with let

  (let ((collected-content
          ;;  create a list with cons to ensure path is included in a loop
          (loop for current-path in (cons path paths)
                if (probe-file current-path)
                  collect (handler-case
                              (with-open-file (stream current-path) ;;  stream represent variable opened inside. it binds the stream to the STREAM
                                (let ((content (make-string (file-length stream)))) ;; ALlocate buffer
                                  (read-sequence content stream) ;; reads data from the stream to preallocated sequence (like string or vector)
                                  content))
                            (file-error (e)
                              (format *error-output* "Error reading file ~a: ~a" current-path e)))
                else do (format *error-output* "File not found: ~a" current-path)))) ;; handle missing files
    (if flag
        (dolist (content collected-content)
          (when (stringp content) (format t "~a" content)))
        collected-content)))
