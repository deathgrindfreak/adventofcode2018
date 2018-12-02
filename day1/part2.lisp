(defun read-config (path)
  "Reads in configuration"
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defvar *config* (read-config "input"))

(defun find-repeated-freq (&optional (config *config*))
  (let ((config-len (length config)))
    (loop for ind = 0 then (mod (+ ind 1) config-len)
          summing (nth ind config) into current-total
          when (member current-total totals)
            return current-total
          collect current-total into totals)))
