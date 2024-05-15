;;; work.el -- Some work related stuff
;;; Commentary:
;;; Code:


(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; (defun work/clock-toggle (&optional dir)
;;   "Find the current log file in DIR and clocks in or out depending on current state."
;;   (interactive)
;;   (let* ((logdir (or dir "~/org/sdsc/log"))
;;          (file (-last-item (sort (f-entries logdir
;;                                             (lambda (p) (s-starts-with-p "20" (f-base p)))) 'string<))))
;;     file))


;;; disable the audio bell
(setq visible-bell 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'work)
;;; work.el ends here
