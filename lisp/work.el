;;; Some work related stuff
;;; Code:

(defun work/clock-toggle (&optional dir)
  "Find the current log file in DIR and clocks in or out depending on current state."
  (interactive)
  (let* ((logdir (or dir "~/org/tundra/log"))
         (file (-last-item (sort (f-entries logdir
                                            (lambda (p) (s-starts-with-p "20" (f-base p)))) 'string<))))
    file))

;(work/clock-toggle)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'work)
