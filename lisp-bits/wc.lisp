;; String -> Cons
;; Read a file from path and return metadata about the file
;;

(defun count-words (line)
  (let ((in-word nil)
        (word-count 0))
    (loop for char across line
          do (cond
               ((and (not in-word) (not (char= char #\Space)))
                (setf in-word t)
                (incf word-count))
               ((and in-word (char= char #\Space))
                (setf in-word nil))))
    word-count))

(defun wc (file-path)
  (if (probe-file file-path)
      (with-open-file (stream file-path)
        (let ((rows 0)
              (chars 0)
              (words 0))
          (do ((line (read-line stream nil)
                     (read-line stream nil)))
              ((null line)
               (format t "File metadata ~A~%~%Rows: ~D~%Words: ~D~%Characters: ~D~%" file-path rows words chars)
               (values rows chars words))
            (incf rows)
            (incf chars (length line))
            (incf words (count-words line)))))
      (format t "File ~A not found.%" file-path)))
