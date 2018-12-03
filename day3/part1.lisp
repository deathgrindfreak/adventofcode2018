(defun split-on (line ch)
  (let ((sp-pos (position ch line)))
    (if sp-pos
        (cons (subseq line 0 sp-pos)
              (split-on (subseq line (1+ sp-pos)) ch))
        (list line))))

(defun parse-claim (claim)
  (destructuring-bind (id _ position dimensions) (split-on claim #\space)
    (list (list 'id (subseq id 1))
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

(defun intersectsp (x1 y1 w1 h1 x2 y2 w2 h2)
  (not (or (>= x1 (+ x2 w2)) (>= x2 (+ x1 w1))
           (>= y1 (+ y2 h2)) (>= y2 (+ y1 h1)))))

(defun get-intersection-coords (rec1 rec2)
  (labels ((get-coords (lst) (cdr (assoc 'coordinates lst))))
    (destructuring-bind (((x1 y1) (w1 h1)) ((x2 y2) (w2 h2)))
        (list (get-coords rec1) (get-coords rec2))
      (and (intersectsp x1 y1 w1 h1 x2 y2 w2 h2)
           (let ((lx (max x1 x2))
                 (rx (min (+ x1 w1) (+ x2 w2)))
                 (by (min (+ y1 h1) (+ y2 h2)))
                 (ty (max y1 y2)))
             (list (list lx ty) (list (- rx lx) (- by ty))))))))

(defun enumerate-rectangle (rec)
  (destructuring-bind ((x y) (w h)) rec
      (loop for i from x below (+ x w)
            append (loop for j from y below (+ y h)
                     collect (list i j)))))

(defun find-intersections (&optional (claims *claims*))
  (let ((intersect-coords
          (loop for (head . rest) on claims
                append (loop for claim in rest
                             for inter = (get-intersection-coords head claim)
                             when inter
                               append (enumerate-rectangle inter)))))
    (length (remove-duplicates intersect-coords :test #'equal))))
