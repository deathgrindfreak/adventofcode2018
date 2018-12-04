(defun split-on (line ch)
  (let ((sp-pos (position ch line)))
    (if sp-pos
        (cons (subseq line 0 sp-pos)
              (split-on (subseq line (1+ sp-pos)) ch))
        (list line))))

(defun parse-claim (claim)
  (destructuring-bind (id _ position dimensions) (split-on claim #\space)
    (list (list 'id (read-from-string (subseq id 1)))
          (list 'coordinates
                (mapcar #'parse-integer
                        (split-on (subseq position 0 (1- (length position)))
                                  #\,))
                (mapcar #'parse-integer
                        (split-on dimensions #\x))))))

(defun read-claims (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-claim line))))

(defvar *claims* (read-claims "input"))

(defun intersectsp (rec1 rec2)
  (labels ((get-coords (lst) (cdr (assoc 'coordinates lst))))
    (destructuring-bind (((x1 y1) (w1 h1)) ((x2 y2) (w2 h2)))
        (list (get-coords rec1) (get-coords rec2))
      (not (or (>= x1 (+ x2 w2)) (>= x2 (+ x1 w1))
               (>= y1 (+ y2 h2)) (>= y2 (+ y1 h1)))))))

(defun find-intersections (&optional (claims *claims*))
  (loop for head in claims
        for inters = (reduce #'(lambda (inters claim)
                                 (if (and (not (equal head claim))
                                          (intersectsp head claim))
                                     (1+ inters)
                                     inters))
                             claims :initial-value 0)
        when (zerop inters)
          return (second (assoc 'id head))))
