;; untitled buffer for quickly taking notes
(defun org-scratch/make-buffer (&optional name)
  (interactive)
  (let* ((name (or name "*scratchpad-1*"))
         (buf (get-buffer name)))
    (switch-to-buffer name)
    (when (not (equal "org-mode" major-mode))
      (org-mode))
    (when (not buf)
      (insert (concat "#+TITLE: " name))
      (newline 2))
    name))

(defun org-scratch/switch-to-new-untitled (n)
  "Switches to buffer *scratchpad-n* if it does not exist."
  (let* ((name (concat "*scratchpad-" (number-to-string n) "*"))
         (buf (get-buffer name)))
    (when (not buf)
      (org-scratch/make-buffer name))))

(defun org-scratch/new-buffer (arg)
  "Creates a new buffer immediately without visiting a
  file. Something like *scratch* but set to org-mode. Takes you
  always to the same scratch buffer, but with prefix arg it creates
  a new one."
  (interactive "P")
  (if (not arg)
      (org-scratch/make-buffer)
    (let ((n 1))
      (while n
        (when (> n 100)
          (error "Too many open buffers"))
        (let ((name (org-scratch/switch-to-new-untitled n)))
          (setq n (if (not name) (1+ n) nil))))
      n)))

(provide 'org-scratch)
