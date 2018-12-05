(defun read-polymer (path)
  (with-open-file (in path)
    (read-line in nil nil)))

(defvar *polymer* (read-polymer "input"))

(defun oppositePolarityp (a b)
  (let ((inta (char-int a))
        (intb (char-int b)))
    (= (+ (min inta intb) 32) (max inta intb))))

(defun simplify-polymer-helper (polymer processed)
  (cond ((null polymer) processed)
        ((and (consp processed)
              (oppositepolarityp (first polymer) (first processed)))
         (simplify-polymer-helper (cdr polymer) (cdr processed)))
        ((and (consp polymer)
              (consp (cdr polymer))
              (oppositepolarityp (first polymer) (second polymer)))
         (simplify-polymer-helper (cddr polymer) processed))
        (t (destructuring-bind (head . rest) polymer
             (simplify-polymer-helper rest (cons head processed))))))

(defun simplify-polymer (&optional (polymer *polymer*))
  (coerce (reverse (simplify-polymer-helper (coerce polymer 'list) nil)) 'string))

(defun remove-unit (c p)
  (remove-if #'(lambda (ch)
                 (or (char= c ch)
                     (char= (char-upcase c) ch)))
             p))

(defun find-simplest-polymer (&optional (polymer *polymer*))
  (let ((simplest-polymers
          (loop for c across "abcdefghijklmnopqrstuvwxyz"
                collect (length (simplify-polymer (remove-unit c polymer))))))
    (apply #'min simplest-polymers)))
