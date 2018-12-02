(defun string-to-symbol-list (str)
  (map 'list #'(lambda (x) (read-from-string (string x))) str))

(defun read-box-ids (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (string-to-symbol-list line))))

(defvar *ids* (read-box-ids "input"))

(defun process-id (id)
  (and id
       (destructuring-bind (head . rest) id
        (let* ((filtered-list (remove head rest))
               (number-of-chars (- (length id) (length filtered-list))))
          (if (> number-of-chars 1)
              (cons (list head number-of-chars) (process-id filtered-list))
              (process-id filtered-list))))))

(defun process-ids (&optional (ids *ids*))
  (labels ((remove-by-n (n lst)
             (length
              (remove-if #'(lambda (x) (not (= n (second x)))) lst))))
    (let ((total-groupings
            (reduce #'(lambda (groupings id)
                        (let* ((group-by-length (process-id id))
                               (number-of-twos (remove-by-n 2 group-by-length))
                               (number-of-threes (remove-by-n 3 group-by-length)))
                          (list (+ (first groupings) (if (zerop number-of-twos) 0 1))
                                (+ (second groupings) (if (zerop number-of-threes) 0 1)))))
                    ids :initial-value '(0 0))))
      (apply #'* total-groupings))))
