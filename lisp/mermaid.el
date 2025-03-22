;;; mermaid.el --- helper functions for mermaid -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;; Partly copied from here:
;;; https://github.com/abrochard/mermaid-mode/
;;;
;;; Code:

(defvar my/mermaid-tmp-dir "/tmp/")
(defvar my/mermaid-output-format ".png")
(defvar my/mermaid-mmdc-location "mmdc")
(defvar my/mermaid-flags "-b transparent -t dark --scale 3")

(defun my/mermaid-compile-markdown-code ()
  "Compile the current markdown code fence."
  (interactive)
  (require 'markdown-mode)
  (let* ((bounds (markdown-code-block-at-point))
         (begin (and bounds (nth 0 bounds)))
         (end (and bounds (nth 1 bounds)))
         (tmp-file-name (concat my/mermaid-tmp-dir "markdown-code.mmd")))
    (when (and begin end)
      (write-region (+ begin 11) (- end 3) tmp-file-name)
      (my/mermaid-compile-file tmp-file-name))))

(defun my/mermaid-compile ()
  "Compile the current mermaid file using mmdc."
  (interactive)
  (my/mermaid-compile-file (buffer-file-name)))

(defun my/mermaid-compile-buffer ()
  "Compile the current mermaid buffer using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat my/mermaid-tmp-dir "current-buffer.mmd")))
    (write-region (point-min) (point-max) tmp-file-name)
    (my/mermaid-compile-file tmp-file-name)))

(defun my/mermaid-compile-region ()
  "Compile the current mermaid region using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat my/mermaid-tmp-dir "current-region.mmd")))
    (when (use-region-p)
      (write-region (region-beginning) (region-end) tmp-file-name)
      (my/mermaid-compile-file tmp-file-name))))

(defun my/mermaid-compile-file (file-name)
  "Compile the given mermaid FILE-NAME using mmdc."
  (interactive "fFilename: ")
  (let* ((input file-name)
         (output (concat (file-name-sans-extension input) my/mermaid-output-format))
         (exit-code (apply #'call-process my/mermaid-mmdc-location nil "*mmdc*" nil (append (split-string my/mermaid-flags " ") (list "-i" input "-o" output)))))
    (if (zerop exit-code)
        (let ((buffer (find-file-noselect output t)))
          (display-buffer buffer)
          (with-current-buffer buffer
            (auto-revert-mode)))
      (pop-to-buffer "*mmdc*"))))

(provide 'mermaid)
;;; mermaid.el ends here
