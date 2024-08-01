;;; scala-play.el -- Some helpers running scala-cli scripts
;;; Commentary:
;;; Code:

(require 'vterm)
(require 'vterm-toggle)

(defun scala-play--run-in-vterm-kill (process event)
  "A process sentinel. Kill PROCESS's buffer if it is live, ignoring EVENT."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun scala-play/run ()
  "Evaluate current buffer as a scala-cli script in a vterm window."
  (interactive)
  (let* ((file (buffer-file-name))
         (filename (shell-quote-argument (file-relative-name file)))
         (cur-buf (current-buffer))
         (vterm-buffer-name "*scala-play-run*"))
    (unless file
      (user-error "You need to save the file first"))
    (save-buffer)
    (with-current-buffer (vterm-other-window)
      (unless scala-play-minor-mode
        (scala-play-minor-mode t))
      (setq-local scala-play--scala-file file)
      (setq-local scala-play--source-buffer cur-buf)
      (set-process-sentinel vterm--process 'scala-play--run-in-vterm-kill)
      (vterm-send-string "clear")
      (vterm-send-return)
      (vterm-send-string (format "scala-cli ."))
      (vterm-send-return))))

(defun scala-play/toggle-execute ()
  "Run the scala script when on scala buffer or switch back to it when on vterm."
  (interactive)
  (cond ((eq major-mode 'scala-ts-mode) (scala-play/run))
        ((eq major-mode 'vterm-mode) (switch-to-buffer-other-window scala-play--source-buffer))
        (t (user-error "Invalid context"))))

(define-minor-mode scala-play-minor-mode
  "Toggle scala-play minor mode."
  :init-value nil
  :lighter " 🐹"
  :keymap
  `((,(kbd "<f9>") . scala-play/toggle-execute)
    (,(kbd "C-c C-c") . scala-play/toggle-execute)))

;;; scala-play.el ends here
