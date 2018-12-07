(defun parse-step (line)
  (mapcar #'read-from-string
          (list (subseq line 5 6)
                (subseq line 36 37))))

(defun read-steps (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-step line))))

(defvar *steps* (read-steps "input"))

(defun sort-symbol-as-string (a b)
  (string< (string a) (string b)))

(defun find-starts (steps)
  (sort
   (remove-duplicates
    (set-difference (mapcar #'first steps)
                    (mapcar #'second steps)))
   #'sort-symbol-as-string))

(defun partition (p lst &key key)
  (loop for l in lst
        if (eql p (if key (funcall key l) l)) collect l into true-items
        else collect l into false-items
        finally (return (list true-items false-items))))

(defun process-graph-help (steps process-list)
  (and process-list
       (destructuring-bind (next . rest) process-list
         (destructuring-bind (children non-children)
             (partition next steps :key #'first)
           (let ((children-to-process (remove-if #'(lambda (c)
                                                     (member c non-children :key #'second))
                                                 (mapcar #'second children))))
             (cons next
                   (process-graph-help non-children
                                       (sort (append children-to-process rest)
                                             #'sort-symbol-as-string))))))))

(defun process-graph (&optional (steps *steps*))
  (format nil "~{~a~}" (process-graph-help steps (find-starts steps))))
