;;; presentation.el --- doing presentations in emacs

;; most things are copied
;; e.g. http://bzg.fr/emacs-strip-tease.html http://bzg.fr/emacs-hide-mode-line.html

(use-package org-tree-slide
  :commands org-tree-slide-mode
  :defer t
  :config
  (bind-key "<next>" 'org-tree-slide-move-next-tree org-tree-slide-mode-map)
  (bind-key "<prior>" 'org-tree-slide-move-previous-tree org-tree-slide-mode-map)
  (bind-key "C-t" 'org-tree-slide-content org-tree-slide-mode-map)
  (bind-key "<right>" nil org-tree-slide-mode-map)
  (bind-key "<left>" nil org-tree-slide-mode-map))

(defvar-local my/hide-mode-line nil)

(defun my/set-fringe-background ()
  "Sets the fringe background to the current background."
  (interactive)
  (custom-set-faces
   `(fringe ((t (:background ,(face-attribute 'default :background)))))))

(define-minor-mode my/hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if my/hidden-mode-line-mode
      (setq my/hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format my/hide-mode-line
          my/hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             my/hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defun my/presentation-enable ()
  ;; set fringe
  ;; (set-fringe-mode
  ;;  (/ (- (frame-pixel-width)
  ;;        (* 170 (frame-char-width)))
  ;;     2))
  (my/set-fringe-background)

  (text-scale-adjust 3)
  (delete-other-windows)
  (my/hidden-mode-line-mode)
  (org-display-inline-images)
  (org-tree-slide-mode))

(defun my/presentation-disable ()
  (text-scale-adjust 0)
  (set-fringe-style nil)
  (org-toggle-inline-images)
  (org-tree-slide-mode 0)
  (my/hidden-mode-line-mode 0))

(define-minor-mode my/presentation-mode
  "Minor mode combining org-tree-slide with some ui tweaks for
  doing presentations."
  :init-value nil
  :global nil
  (if my/presentation-mode
      (my/presentation-enable)
    (my/presentation-disable)))

(provide 'presentation)
