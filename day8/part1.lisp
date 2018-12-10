(defun read-nodes (path)
  (with-open-file (in path)
    (loop for n = (read in nil nil)
          while n
          collect n)))

(defvar *nodes* (read-nodes "input"))

(defun make-sum-list (sum num-metadata rest)
  (list (apply #'+ (cons sum (subseq rest 0 num-metadata)))
        (subseq rest num-metadata)))

(defun add-metadata-entries-help (heap sum)
  (destructuring-bind (num-children num-metadata . rest) heap
    (cond ((zerop num-children) (make-sum-list sum num-metadata rest))
          (t (let ((child-sums (loop repeat num-children
                                     with running-rest = rest
                                     with running-sum = sum
                                     for child = (add-metadata-entries-help running-rest running-sum)
                                     do (setf running-sum (first child)
                                              running-rest (second child))
                                     finally (return (list running-sum running-rest)))))
               (make-sum-list (first child-sums) num-metadata (second child-sums)))))))

(defun add-metadata-entries (&optional (heap *nodes*))
  (first (add-metadata-entries-help heap 0)))
