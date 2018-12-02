(defun string-to-symbol-list (str)
  (map 'list #'(lambda (x) (read-from-string (string x))) str))

(defun read-box-ids (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (string-to-symbol-list line))))

(defvar *ids* (read-box-ids "input"))

(defun number-of-differences (id1 id2)
  (reduce #'(lambda (a x)
              (if x a (1+ a)))
          (mapcar #'eq id1 id2) :initial-value 0))

(defun common-elements (id1 id2)
  (remove nil (mapcar #'(lambda (a b)
                          (and (eq a b) a))
                      id1 id2)))

(defun right-keyp (test-id id-list)
  (find-if #'(lambda (id)
               (= 1 (number-of-differences id test-id)))
           id-list))

(defun find-right-ids (ids)
  (destructuring-bind (head . rest) ids
    (let ((mem-id (right-keyp head rest)))
      (if mem-id
          (list head mem-id)
          (find-right-ids rest)))))

(defun common-elements-right-ids (&optional (ids *ids*))
  (string-downcase
   (format nil "~{~a~}"
           (apply #'common-elements (find-right-ids ids)))))
