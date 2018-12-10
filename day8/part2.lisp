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
          (t (let ((child-sums (loop for index from 1 to num-children
                                     with running-rest = rest
                                     for child = (add-metadata-entries-help running-rest sum)
                                     do (setf running-rest (second child))
                                     collect (first child) into sums
                                     finally (return (list sums running-rest)))))
               (destructuring-bind (sums new-rest) child-sums
                 (list (apply #'+ (cons sum (mapcar #'(lambda (x)
                                                        (or (nth (1- x) sums) 0))
                                                    (subseq new-rest 0 num-metadata))))
                       (subseq new-rest num-metadata))))))))

(defun add-metadata-entries (&optional (heap *nodes*))
  (first (add-metadata-entries-help heap 0)))
