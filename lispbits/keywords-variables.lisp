(defvar *symbols-cache* (make-hash-table))

(setf (gethash :foo *symbols-cache*) :bar)
(str:remove-punctuation "")

(defun scan-file-for-keywords (filename)
  "Scan a Lisp file for keywords and report them."
  (with-open-file (stream filename :direction :input)
    (let ((eof-value (gensym "EOF"))
          (found-keywords nil))
      (loop for form = (read stream nil eof-value)
            until (eq form eof-value)
            do (when (scan-keywords-in-form form)
                 (push form found-keywords)))
      found-keywords)))

(defun scan-keywords-in-form (form)
  "Scan a form for keywords, return list of found keywords."
  (let ((keywords nil))
    (labels ((traverse (f)
               (cond
                 ((keywordp f)
                  (push f keywords))
                 ((consp f)
                  (traverse (car f))
                  (traverse (cdr f))))))
      (traverse form)
      keywords)))

(defun scan-asdf-system-for-keywords (system-name)
  "Scan an ASDF system for keywords."
  (let ((system (asdf:find-system system-name)))
    (when system
      (let ((files (mapcar #'namestring (asdf:system-source-files system))))
        (loop for file in files
              when (member (pathname-type file) '("lisp" "cl") :test #'string=)
              append (scan-file-for-keywords file))))))

(defun scan-project-for-keywords (directory &key (extensions '("lisp" "cl" "asd")))
  "Scan all Lisp files in a directory for keywords."
  (let ((keyword-usage (make-hash-table :test #'equal)))
    (labels ((scan-dir (dir)
               (dolist (entry (directory (merge-pathnames "*.*" dir)))
                 (let ((name (namestring entry)))
                   (cond
                     ((and (pathname-name entry)
                           (member (pathname-type entry) extensions :test #'string=))
                      (let ((keywords (scan-file-for-keywords name)))
                        (when keywords
                          (setf (gethash name keyword-usage) keywords))))

                     ((probe-file (merge-pathnames #P"*.*" entry))
                      (scan-dir entry)))))))
      (scan-dir directory)
      keyword-usage)))

(defun normalize-all-spaces-trim (string)
  "replace any whitespace with hyphens and trim leading/trailing whitespace.
the pattern ^\\s+|\\s+$ in the regex is responsible for trimming leading and trailing whitespaces."
  (cl-ppcre:regex-replace-all "^\\s+|\\s+$"
                              (cl-ppcre:regex-replace-all "\\s+" string "-")
                              ""))
