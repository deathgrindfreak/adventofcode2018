(defun read-config (path)
  "Reads in configuration"
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defvar *config* (read-config "input"))

(defun calculate-resulting-freq (&optional (config *config*))
  (apply #'+ config))

(calculate-resulting-freq)
