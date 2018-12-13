(ql:quickload :cl-ppcre) ; Need some regex

(defun parse-line (line)
  (cl-ppcre:register-groups-bind (year month day hour minute text)
      ("\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] (.*)" line)
    (list (mapcar #'parse-integer
                  (list year month day hour minute))
          text)))

(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (list< (rest a) (rest b)))
        (t (< (first a) (first b))) ))

(defun read-lines (path)
  (with-open-file (in path)
    (sort
     (loop for line = (read-line in nil nil)
           while line
           collect (parse-line line))
     #'list<
     :key #'cdar)))

(defvar *records* (read-lines "input"))

(defun get-status (text)
  (let ((start (subseq (second text) 0 5)))
    (cond ((equal start "Guard") 'start)
          ((equal start "falls") 'sleep)
          ((equal start "wakes") 'wake))))

(defun parse-guard-id (text)
  (cl-ppcre::register-groups-bind (id)
      ("Guard #(\\d+) begins shift" (second text))
    (parse-integer id)))

(defun interval-slept (a b)
  (let ((interv (mapcar #'- (first b) (first a))))
    (+ (* 60 (fourth interv)) (fifth interv))))

(defun get-minute (int)
  (car (last (car int))))

(defun sleep-interval (sleep wake)
  (destructuring-bind (s w) (mapcar #'get-minute (list sleep wake))
    (list s (1- w))))

(defun histogram (lst)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (itm lst)
      (incf (gethash itm hash 0)))
    (loop for k being the hash-keys of hash
          collect (list (gethash k hash) k))))

(defun calculate-intervals (rec sleep-wake-cycles)
  (destructuring-bind (minutes-rested minute-intervals) rec
    (loop for (sleep wake) on sleep-wake-cycles by #'cddr
          for total-slept = (+ minutes-rested (interval-slept sleep wake))
            then (+ total-slept (interval-slept sleep wake))
          for interval = (cons (sleep-interval sleep wake) minute-intervals)
            then (cons (sleep-interval sleep wake) interval)
          finally (return (list total-slept interval)))))

(defun find-best-guard (&optional (records *records*) (guards (make-hash-table :test #'equal)))
  (destructuring-bind (head . rest) records
    (let* ((key (parse-guard-id head))
           (rec (gethash key guards (list 0 nil)))
           (next-record (position-if #'(lambda (r) (eq 'start (get-status r)))
                                     rest)))
      (setf (gethash key guards)
            (calculate-intervals rec (subseq rest 0
                                             (or next-record (length rest)))))
      (if next-record
          (find-best-guard (subseq rest next-record) guards)
          (let* ((guards (loop for k being the hash-keys of guards
                               collect (cons k (gethash k guards))))
                 (max-sleep (find (apply #'max (mapcar #'(lambda (x)
                                                           (or (cadr x) 0))
                                                       guards))
                                  guards :key #'cadr))
                 (hist (histogram
                        (apply #'append
                               (mapcar #'(lambda (x)
                                           (loop for i from (first x) to (second x) collect i))
                                       (third max-sleep))))))
            (* (first max-sleep)
               (second
                (find (apply #'max (mapcar #'car hist)) hist :key #'car))))))))
