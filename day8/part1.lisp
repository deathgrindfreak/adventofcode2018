(defun read-nodes (path)
  (with-open-file (in path)
    (loop for n = (read in nil nil)
          while n
          collect n)))

(defvar *nodes* (read-nodes "input"))

(defun add-metadata-entries-help (heap sum)
  (destructuring-bind (num-children num-metadata . rest) heap
    (cond ((zerop num-children) (list (apply #'+ (cons sum (subseq rest 0 num-metadata)))
                                      (subseq rest num-metadata)))
          (t (let ((child-sums (loop repeat num-children
                                     with running-rest = rest
                                     with running-sum = sum
                                     for child = (add-metadata-entries-help running-rest running-sum)
                                     do (setf running-sum (first child)
                                              running-rest (second child))
                                     finally (return (list running-sum running-rest)))))
               (list (apply #'+ (cons (first child-sums)
                                      (subseq (second child-sums) 0 num-metadata)))
                     (subseq (second child-sums) num-metadata)))))))

(defun add-metadata-entries (&optional (heap *nodes*))
  (first (add-metadata-entries-help heap 0)))
