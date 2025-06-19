;;; init.el -- emacs magic
;;;
;;; Commentary:
;;;   My Emacs config file.
;;;
;;; Code:

(defconst my/emacs-start-time (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prepare use-package


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(eval-and-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some core packages

(use-package dash)
(use-package s)
(use-package f)
(use-package hydra)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configure emacs look&feel

;; backup stuff
(use-package files
  :config
  (let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
    (make-directory backup-dir t)
    (setq
     backup-by-copying t
     backup-directory-alist `((".*" . ,backup-dir))
     auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t)))

(use-package window
  :bind* (("C-–" . other-window)
          ("C-•" . other-window)))

;; some emacs gui tweaks
(setq use-file-dialog nil)
;; draw a red frame around images when cursor is on
;; see [[mu4e:msgid:87vb53cswe.fsf@gnus.org][Re: Change appearance of selected image]]
(setq default-frame-alist
       (nconc (list '(mouse-color . "red")
                    '(cursor-color . "red"))
              default-frame-alist))

(setq scroll-preserve-screen-position t)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(blink-cursor-mode 0)

;; deactivate suspend keys in graphic mode
(when (display-graphic-p)
  (unbind-key "C-x C-z")
  (unbind-key "C-z"))

(setq gc-cons-threshold 20000000)

;; set emacs window title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
                 "%b"))))

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq vc-follow-symlinks t)
(setq sentence-end-double-space nil)
(global-auto-revert-mode t)
(display-time-mode t)
(setq-default tab-width 2
              indent-tabs-mode nil)

(add-to-list 'exec-path "~/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lisp/mylib

(use-package mylib
  :load-path "lisp"
;  :commands (my/host-p my/host-starts-with-p my/copy-to-terminal)
  :demand t
  :bind (("M-j" . my/join-line)
         ("C-x C-y" . my/copy-to-terminal)
         ("C-h C-f" . find-function)
         ("<f1>" . my/visit-now)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show parens
(use-package paren
  :config
  (show-paren-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; consult / embark / marginalia / vertico

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
;;         ("C-c h" . consult-history)
;;         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
 ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ;;         ("M-#" . consult-register-load)
         ;;         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-flycheck)
;;         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s zL" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package vertico
  :config
  (vertico-mode))

(use-package embark
  :bind
  (("M-o" . embark-act)
   ("C-h B" . embark-bindings)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package wgrep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy

(use-package avy
  :bind (("M-g g" . avy-goto-line)
         ("C-S-s" . avy-goto-char-timer)
         ("C-s-s" . avy-goto-char-timer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eyebrowse – a window config manager

(use-package eyebrowse
  :diminish eyebrowse-mode
  :commands (eyebrowse-switch-to-window-config-2 eyebrowse-prev-window-config eyebrowse-next-window-config)
  :bind* (("C-<" . eyebrowse-prev-window-config)
          ("C->" . eyebrowse-next-window-config))
  :config
  (eyebrowse-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; windmove

(use-package windmove
  :bind (("s-C-<right>" . windmove-right)
         ("s-C-<left>" . windmove-left)
         ("s-C-<up>" . windmove-up)
         ("s-C-<down>" . windmove-down)
         ("S-C-<up>" . enlarge-window)
         ("S-C-<down>" . shrink-window)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<left>" . shrink-window-horizontally)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; golden-ratio-mode

(use-package golden-ratio-mode
  :diminish golden-ratio-mode
  :commands (golden-ratio-mode golden-ratio)
  :config
  (golden-ratio-mode 1)
  (remove-hook 'window-configuration-change-hook 'golden-ratio)
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-char-timer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rainbow-delimiters

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company

(use-package pos-tip
  :commands (pos-tip-show))

(use-package company
  :diminish (company-mode . " ∀")
  :demand t
  :init
  (defun my/company-complete-next-cycle ()
    (interactive)
    (company-complete-common-or-cycle 1))
  (defun my/company-complete-prev-cycle ()
    (interactive)
    (company-complete-common-or-cycle -1))
  :bind (:map company-active-map
              ("C-n" . my/company-complete-next-cycle)
              ("C-p" . my/company-complete-prev-cycle))
  :config
  (use-package company-quickhelp
    :demand t
    :bind (:map company-active-map
           ("M-h" . company-quickhelp-manual-begin))
    :config
    (setq company-quickhelp-delay nil))
  (global-company-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; woman

(use-package woman
  :config
  (when (file-exists-p "/run/current-system/sw/share/man")
    (setq woman-manpath '("/run/current-system/sw/share/man")))
  (when (file-exists-p "~/.nix-profile/share/man")
    (add-to-list 'woman-manpath "~/.nix-profile/share/man")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prettify symbols


(global-prettify-symbols-mode)
(-each '(scala-mode-hook scala-ts-mode-hook java-ts-mode-hook js2-mode-hook)
  (lambda (hook)
    (add-hook hook (lambda ()
                     (add-to-list 'prettify-symbols-alist '("!=" . ?≠))
                     (add-to-list 'prettify-symbols-alist '(">=" . ?≥))
                     (add-to-list 'prettify-symbols-alist '("<=" . ?≤))))))
(-each '(scheme-mode-hook)
  (lambda (hook)
    (add-hook hook (lambda ()
                     (add-to-list 'prettify-symbols-alist '("lambda" . ?λ))))))
(-each '(js2-mode-hook)
  (lambda (hook)
    (add-hook hook (lambda ()
                     (add-to-list 'prettify-symbols-alist '("function" . ?ƒ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line numbers

(use-package display-line-numbers
  :commands (display-line-numbers-mode)
  :init
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key (replaces guide-key)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-scratch

(use-package org-scratch
  :load-path "lisp"
  :bind ("C-x C-n" . org-scratch/new-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whitespace-cleanup-mode

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delsel

(use-package delsel
  :config
  (pending-delete-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move-text

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet setup

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode yas-reload-all)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (:map yas-minor-mode-map
         ("C-<tab>" . yas-expand)
;         ("C-i" . yas-expand)
         )
  :config
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple cursors

(use-package multiple-cursors
  :commands (mc/mark-next-like-this
             mc/unmark-next-like-this
             mc/mark-previous-like-this
             mc/unmark-previous-like-this)
  :init
  (defhydra hydra-mc (global-map "C-c m")
    "multiple cursor: "
    ("E" mc/edit-lines "Edit Lines")
    ("n" mc/mark-next-like-this "Mark next")
    ("N" mc/unmark-next-like-this "Unmark next")
    ("p" mc/mark-previous-like-this "Mark previous")
    ("P" mc/unmark-previous-like-this "Unmark previous")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region

(use-package expand-region
  :bind (("M-2" . er/expand-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editorconfig

(use-package editorconfig
  :disabled t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ds4e

(use-package ds4e-search
  :commands (ds4e-search)
  :config
  ;(setq ds4e-dsc-executable "/home/eike/workspace/projects/dsc/target/debug/dsc")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-d" . dired-jump)
	 ("C-x d" . dired-jump))
  :config
  (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  ;;(setq dired-kill-when-opening-new-dired-buffer t) Emacs 28
  (use-package dired-filter
    :config
    (bind-key "f" dired-filter-mark-map dired-mode-map)))

(use-package dired-rainbow
  :after dired)

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	      ("i" . dired-subtree-cycle)))

(use-package all-the-icons-dired
  :after dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar
  :disabled t
  :bind (("C-x C-g" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-close-sidebar-on-file-open t)
  ;;(setq dired-sidebar-subtree-line-prefix "··")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-theme 'icons))

(use-package ds4e-dired
  :after dired
  :bind (:map dired-mode-map
              ("C-d u" . ds4e-dired-upload)
              ("C-d o" . ds4e-dired-open-browser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dirvish

(use-package dirvish
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"    "Home")
     ("n" "/mnt/nas"  "NAS")))
  :config
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  :bind
  ((("C-z C-z" . dirvish-side)
    :map dirvish-mode-map
         ("a" . dirvish-quick-access)
         ("f" . dirvish-file-info-menu)
         ("y" . dirvish-yank-menu)
         ("N" . dirvish-narrow)
         ;;("^" . dirvish-history-last)
         ("h"   . dirvish-history-jump)
         ("TAB" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-m" . dirvish-mark-menu)
         ("M-t" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump))))

(use-package dirvish-yank)
(use-package dirvish-vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit

(use-package magit
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-popup-show-common-commands nil)
  (setq magit-popup-manpage-package 'woman))

(use-package forge
  :after magit)

(use-package with-editor
  :commands (with-editor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-timemachine

(use-package git-timemachine
  :commands git-timemachine
  :config
  (defun my/git-timemachine-show-commit ()
    "Show the current commit with magit."
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  (bind-key "s" 'my/git-timemachine-show-commit git-timemachine-mode-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile

(use-package projectile
;;  :bind (("C-c pp" . consult-projectile-switch-project))
  :bind ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("p" . consult-projectile-switch-project)
              ("b" . consult-projectile-switch-to-buffer)
              ("d" . consult-projectile-find-dir)
              ("f" . consult-projectile-find-file)
              ("s s" . consult-ripgrep))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'auto)
  (setq projectile-remember-window-configs t)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-mode-line-prefix " Ƥ"))

;; (use-package counsel-projectile
;;   :config
;;   (counsel-projectile-mode)
;;   (setq counsel-projectile-switch-project-action 'magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rest cient

(use-package restclient
  :commands (restclient-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solr client
(use-package solr
  :load-path "lisp"
  :commands (solr-client-mode))

(use-package ob-solr
  :after solr
  :load-path "lisp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit

(use-package paredit
  :diminish paredit-mode
  :commands enable-paredit-mode
  :init
  (-each '(emacs-lisp-mode-hook
           lisp-mode-hook
           lisp-interaction-mode-hook
           scheme-mode-hook
           clojure-mode-hook)
    (lambda (mode)
      (add-hook mode 'enable-paredit-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stumpwm

(use-package stumpwm-mode
  :commands (stumpwm-mode)
  :mode (("\\.stumpwmrc" . stumpwm-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nyan-mode (the cat… ;-))

(use-package nyan-mode
  :disabled t
  :config
  (nyan-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logview mode

(use-package logview
  :commands (logview-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password-store

(use-package password-store
  :demand t
  :commands (password-store-get)
  :bind (("<f2>" . password-store-copy))
  :init
  (defun my/password-store-get-entry (entry)
    (s-lines (password-store--run-show entry)))
  (defun my/password-store-get-user (entry)
    (car (cdr (my/password-store-get-entry entry))))
  (defun my/password-store-get-key (entry key)
    (let ((line (-find (lambda (s) (s-starts-with-p key s))
                       (my/password-store-get-entry entry))))
      (when line
        (s-trim (substring line (1+ (length key))))))))

(use-package pass
  :commands pass)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browse-url

(use-package browse-url
  :bind (("C-c b" . browse-url))
  :config
  (if (eq system-type 'darwin)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "open"
            browse-url-generic-args '("-a" "/Applications/Firefox.app"))
    (setq browse-url-browser-function 'browse-url-firefox
          browse-url-generic-program "firefox")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keycast

(use-package keycast
  :commands (keycast-mode)
  :config
  (setq keycast-insert-after 'moody-mode-line-buffer-identification))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beacon
(use-package beacon
  :commands (beacon-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vterm

(use-package vterm
  :commands (vterm))

(use-package vterm-toggle
  :bind (("<f6>" . vterm-toggle)
         ("<f7>" . vterm-toggle)
         ("C-<f6>" . vterm-toggle-cd)
         (:map vterm-mode-map
               ("C-c n" . vterm-toggle-forward)
               ("<f6>" . vterm-toggle)
               ("<f7>" . vterm-toggle)))
  :init
  (setq vterm-toggle-fullscreen-p nil)
  (defun run-in-vterm-kill (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b)
           (kill-buffer b))))

  (defun run-in-vterm (command)
    "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
    (interactive
     (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
             (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
        (read-shell-command "Terminal command: "
                            (cons filename 0)
                            (cons 'shell-command-history 1)
                            (list filename)))))
    (with-current-buffer (vterm (concat "*" command "*"))
      (set-process-sentinel vterm--process #'run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu-list

;; https://github.com/bmag/imenu-list
(use-package imenu-list
  :bind (("C-ß" . imenu-list-smart-toggle))
  :init
  (setq imenu-list-focus-after-activation t)
  :config
  (imenu-list-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashboard

(use-package dashboard
  :config
  (use-package ds4e-dashboard
    :config
    (setq ds4e-dashboard-section-name "Offene Aufgaben")
    (setq ds4e-dashboard-query '(:bookmark "Offene Aufgaben"))
    (ds4e-dashboard-register))
  (add-to-list 'dashboard-item-shortcuts '(docspell . "d"))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 5)
                          (docspell . 5)
                          (recents  . 5)
                          (agenda . 5)
                          (bookmarks . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-footer "Have a nice day!")
  (setq show-week-agenda-p t)
  (setq dashboard-set-file-icons t)

  (dashboard-insert-startupify-lists)

  (defun my/dashboard-activate ()
    (interactive)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (dashboard-mode)
    (redisplay))
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fill-column helper

(use-package fill-column-indicator)
(use-package visual-fill-column)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org mode
(use-package org
  :mode  (("\\.org" . org-mode)
          ("\\.org.gpg" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-todo-keywords
        '((sequence "WAIT(w@/!)" "BUG(b)" "TODO(t)" "WORKING(k!)" "|"
                    "DONE(d!)" "WONTFIX(n@)" "CANCELLED(c@)")))
  (setq org-deadline-warning-days 10)
  (setq org-log-into-drawer t)
  (setq org-src-fontify-natively t)
  (setq org-loop-over-headlines-in-active-region t)
  ;(setq org-ellipsis " ⤵");; ⤵ ≫
  (setq org-startup-folded t)
  (use-package ob-restclient
    :commands (org-babel-execute:restclient))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (java . t)
     (js . t)
     (shell . t)
     (clojure . t)
     (scheme . t)
     (scala . t)
     (python . t)
     (sql . t)
     (jdbcsh . t)
     (sqlite . t)
     (gnuplot . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (elvish . t)
     (rust . t)
     (sparql . t)
     (C . t)
     (R . t)))
  ;; running scala code with ammonite
  (setq org-babel-scala-command "amm")
  (setq org-babel-scala-wrapper-method "%s")
  (setq org-babel-clojure-backend 'cider)

  (let ((ditaa-path (s-trim
                     (shell-command-to-string
                      "nix-shell -p ditaa --run 'realpath -e \"$(dirname $(which ditaa))/../lib/ditaa.jar\"'"))))
    (setq org-ditaa-jar-path ditaa-path))

  (defvar my-org-project-files nil)
  (defun my/org-project-files ()
     my-org-project-files)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))
                             (my/org-project-files . (:maxlevel . 2))
                             (nil . (:maxlevel . 2)))
        org-reverse-note-order t)

  (setq org-capture-templates
        '(("e" "Ausgaben")
          ("e1" "Ausgabe Sonstiges CHF" entry (file+headline "~/org/expenses/2020.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e2" "Ausgabe Sonstiges EUR" entry (file+headline "~/org/expenses/2020.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e3" "Ausgabe Teilen CHF" entry (file+headline "~/org/expenses/2020.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e4" "Ausgabe Teilen EUR" entry (file+headline "~/org/expenses/2020.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e5" "Mittag BC" entry (file+headline "~/org/expenses/2020.org" "Sonstiges")
           "** Mittag BC   :lebensbedarf:\n %^{date}p %^{chf}p" :prepend t :empty-lines 1))))

(use-package org-bullets
  :disabled t
  :if window-system
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-modern
  :config
  (setq
   org-ellipsis "…"
   org-pretty-entities t
   org-hide-emphasis-markers t)
  ;; workaround for table formatting with dates
  ;; https://github.com/minad/org-modern/issues/5#issuecomment-1704023036
  :custom
  (org-modern-block-fringe 10)
  :custom-face
  (org-modern-label
   ((t :height 1.0 :weight semi-bold
       :underline nil :inherit default)))
  :init
  (global-org-modern-mode))

(use-package org-indent
  :if window-system
  :diminish org-indent-mode
  :commands org-indent-mode
  :init (setq org-startup-indented t))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-files "~/org/agenda")
  (setq org-agenda-filter-preset '("-later"))
  (setq org-agenda-log-mode-items '(closed state clock))
  ;(use-package org-contacts)
  )

(use-package org-crypt
  :commands (org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry)
  :config
  (org-crypt-use-before-save-magic)
  (add-to-list 'org-tags-exclude-from-inheritance org-crypt-tag-matcher)
  (setq org-crypt-key "AD7AC35E"))

(use-package org-journal
  :commands org-journal-new-entry
  :config
  (setq org-journal-dir "~/org/journal/")
  (setq org-journal-file-format "%Y%m%d.org"))

(use-package org-clock
  :commands (my/org-clock-admin my/org-clock-bc-org)
  :bind (("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-x" . org-clock-in-last)
         ("C-c C-x C-q" . org-clock-cancel))
  :config
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist 'history)
  (setq org-clock-history-length 25)
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (defun my/org-clock-id (id)
    (save-excursion
      (org-id-goto id)
      (org-clock-in)
      (org-clock-mark-default-task)))

  (defun my/org-clock-admin ()
    (interactive)
    (my/org-clock-id "18346830-07db-4840-9e2d-4bdfda4683bd"))

  (defun my/org-clock-bc-org ()
    (interactive)
    (my/org-clock-id "e51a4e26-302f-4353-8e6e-504cb383bfd0")))

(use-package svg-tag-mode
  :disabled t
  :config
  (setq svg-tag-tags
        '(("\\(:[A-Za-z0-9]+:\\)" . ((lambda (tag)
                               (svg-tag-make tag :beg 1 :end -1))))
          ("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                                (svg-tag-make tag :beg 1 :inverse t
                                                              :margin 0 :crop-right t))))
          (":[A-Za-z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                                (svg-tag-make tag :beg 1 :end -1
                                                              :margin 0 :crop-left t)))))))

(use-package ob-mongo
  :disabled t)

(use-package ox-koma-letter
  :config (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t) t)
  (progn
    (add-to-list 'org-latex-classes
                 '("my-letter"
                   "\\documentclass\[parskip=half,fontsize=12pt,subject=titled]\{scrlttr2\}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"))

    ;; moderncv
    (add-to-list 'org-latex-classes
                 '("mymoderncv"
                   "\\documentclass\{moderncv\}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[ngerman]{babel}
\\name{Eike}{Kettner}
\\social[github]{eikek}
\\email{eike.kettner@posteo.de}
\\phone[mobile]{+41~(76)~278~4160}
\\address{Arbergstrasse 7c}{8405 Winterthur}{Schweiz}
\[NO-DEFAULT-PACKAGES]
\[NO-PACKAGES]
\[EXTRA]"
                   ("\\section{%s}" . "\\section{%s}")
                   ("\\subsection{%s}" . "\\subsection{%s}")))


    (setq org-koma-letter-default-class "my-letter")))

(use-package ox-reveal)

(use-package presentation
  :load-path "lisp"
  :commands (my/presentation-enable my/presentation-disable my/presentation-mode))

(use-package org-expenses
  :bind (("C-c e" . org-expenses/expense-view))
  :config
  (when (file-exists-p "/run/current-system/sw/bin/sqlite3")
    (setq org-expenses/sqlite-cmd "/run/current-system/sw/bin/sqlite3"))

  (setq org-expenses/sqlite-db-file "~/.exp.db")
  (setq org-expenses/files "~/org/expenses/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ekg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ekg
  :bind (([f11] . ekg-capture)
         (:map ekg-edit-mode-map
               ("C-c C-t" . ek/ekg-edit-insert-tag)))
  :commands (ekg-show-notes-latest-captured ekg-show-notes-for-today ekg-show-notes-with-tag)
  :custom-face
  (ekg-tag
   ((t :height 1.0 :weight semi-bold
       :box (:line-width (1 . 1) :color "DeepSkyBlue1")
       :foreground "DeepSkyBlue1"
       :underline nil :inherit default)))
  :config
  (setq ekg-db-file "~/org/triples.db")
  (require 'ekg-auto-save)
  (add-hook 'ekg-capture-mode-hook #'ekg-auto-save-mode)
  (add-hook 'ekg-edit-mode-hook #'ekg-auto-save-mode)
  (add-to-list 'ekg-command-regex-for-narrowing "org-element-at-point" t)
  (defun ek/ekg-edit-insert-tag ()
    "Insert a TAG at point."
    (interactive)
    (let ((tag (completing-read "Tag: " (ekg-tags) nil t)))
      (goto-char (pos-eol))
      (re-search-backward "[^ \t\r]")
      (if (looking-at-p ",")
          (forward-char)
        (progn
          (forward-char)
          (insert ",")))
      (insert " " tag))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; E-Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mu4e

;; from emacs wiki: http://www.emacswiki.org/emacs/mu4e
;;; message view action
(defun my/mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message MSG in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(eval-and-compile
  (defvar my/mu4e-find-load-path
    (let ((cand '("~/.nix-profile/share/emacs/site-lisp/mu4e"
                  "/run/current-system/sw/share/emacs/site-lisp/mu4e")))
      (-find 'file-exists-p cand))))

(defun my/mu4e-make-contexts ()
  "Make my email contexts."
  `(,(make-mu4e-context
      :name "posteo.de"
      :enter-func (lambda () (mu4e-message "Switch to posteo.de context"))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg
                                                          '(:to :cc :bcc)
                                                          "@posteo.de$")))
      :vars `((user-full-name . "Eike Kettner")
              (mu4e-maildir . "~/Mail")
              (mu4e-sent-folder . "/posteo/Sent")
              (mu4e-trash-folder . "/posteo/Trash")
              (mu4e-drafts-folder . "/posteo/Drafts")
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://docspell.org"))
              (smtpmail-smtp-server . ,(my/password-store-get-key "email/posteo.de" "mailhost"))
              (smtpmail-smtp-user . ,(my/password-store-get-key "email/posteo.de" "user"))
              (smtpmail-stream-type . starttls)
              (smtpmail-smtp-service . 587)))
    ,(make-mu4e-context
      :name "gmx.de"
      :enter-func (lambda () (mu4e-message "Switch to gmx.de context"))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg
                                                          '(:to :cc :bcc)
                                                          "@gmx.de$")))
      :vars `((user-full-name . "Eike Kettner")
              (mu4e-maildir . "~/Mail")
              (mu4e-sent-folder . "/gmx/Gesendet")
              (mu4e-trash-folder . "/gmx/Trash")
              (mu4e-drafts-folder . "/gmx/Entw&APw-rfe")
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://docspell.org"))
              (smtpmail-smtp-server . ,(my/password-store-get-key "email/gmx.de" "smtphost"))
              (smtpmail-smtp-user . ,(my/password-store-get-key "email/gmx.de" "user"))
              (smtpmail-smtp-service . 587)))))
(use-package mu4e
  :load-path my/mu4e-find-load-path
  :bind (("<f5>" . mu4e))
  :config

  (use-package ds4e-mu4e
    :config
    (ds4e-mu4e-register))

  
  (setq mu4e-context-policy 'pick-first)
  ;;(setq mu4e-compose-context-policy nil)

  (setq mu4e-contexts (my/mu4e-make-contexts))
  (set-face-foreground 'mu4e-unread-face "deep sky blue")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-stream-type 'starttls)
  (setq mu4e-maildir "~/Mail")
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mml2015-use 'epg) ;; enable gpg/mime over epg
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval nil)
  (setq message-citation-line-format "On %a %d.%m.%Y at %R, %f writes:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;;  (setq mu4e-html2text-command 'mu4e-shr2text)  later in mu4e…
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (add-to-list 'mu4e-view-actions
               '("View in browser" . my/mu4e-msgv-action-view-in-browser) t)

  ;; see https://www.djcbsoftware.nl/code/mu/mu4e/Queries.html#Queries
  (add-to-list 'mu4e-bookmarks
               `(,(concat "date:2w..now AND NOT m:/eknet/spam AND NOT "
                          "m:/eknet/LearnSpam AND NOT "
                          "m:/eknet/lists* ")
                 "No mailinglists, no spam" ?c))

  (setq
   mu4e-view-show-addresses t
   mu4e-use-fancy-chars t
   mu4e-view-show-images t
   mu4e-view-image-max-width 800
   mu4e-compose-crypto-reply-plain-policy 'sign
   mu4e-compose-dont-reply-to-self t)
  ;; (-each '("eike.kettner@eknet.org" "eike@eknet.org")
  ;;   (lambda (addr)
  ;; (add-to-list 'mu4e-user-mail-address-list addr)))
  )

(use-package org-mu4e
  :commands (org-store-link org-mail org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-convert-to-html t)
  (defalias 'org-mail 'org-mu4e-compose-org-mode)

  ;; redefine org~mu4e-mime-convert-to-html-maybe to use custom
  ;; function when exporting to HTML; this is to add more export
  ;; settings, i.e. disable the TOC.
  (defun my/org-mu4e-mime-convert-to-html ()
    (let ((org-export-with-author nil)
          (org-export-with-date nil)
          (org-export-with-email nil)
          (org-export-with-section-numbers nil)
          (org-export-with-title nil)
          (org-export-with-toc nil))
      (org~mu4e-mime-convert-to-html)))

  ;; this is copy&paste from org-mu4e.el; replacing the last function
  ;; call.
  (defun org~mu4e-mime-convert-to-html-maybe ()
    "Convert to html if `org-mu4e-convert-to-html' is non-nil.
This function is called when sending a message (from
`message-send-hook') and, if non-nil, sends the message as the
rich-text version of what is assumed to be an org mode body."
    (when org-mu4e-convert-to-html
      (mu4e-message "Converting to html")
      (my/org-mu4e-mime-convert-to-html))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :hook
  (emacs-lisp-mode . flycheck-mode))

(use-package scala-mode
  :disabled t
  :config
  (setq
    scala-indent:align-forms t
    scala-indent:align-parameters t
    scala-indent:indent-value-expression t
    scala-indent:default-run-on-strategy 0))

(use-package scala-ts-mode
  :mode ("\\.s\\(cala\\|bt\\|c\\)$"
         "\\.mill"))

(use-package scala-play
  :load-path "lisp"
  :commands scala-play-minor-mode)

(use-package java-ts-mode
  :mode ("\\.java" . java-mode))

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package python-ts-mode
  :mode ("\\.py" . python-ts-mode))

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))

(use-package python-pytest
  :config
  (transient-append-suffix 'python-pytest-dispatch "-s"
    '("-N" "No coverage" "--no-cov"))
  (transient-append-suffix 'python-pytest-dispatch "-N"
    '("-w" "No warnings" "-p no:warnings")))

(use-package elpy)
(use-package ruff-format)

(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (scala "https://github.com/tree-sitter/tree-sitter-scala")))
  :config
  ;; see https://github.com/KaranAhlawat/scala-ts-mode/issues/12
  (setq treesit-font-lock-level 4))


;;; https://scalameta.org/metals/docs/editors/emacs.html
(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-z")
  :hook
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . flycheck-mode)
  (scala-ts-mode . lsp-deferred)

  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers t)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]target\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bsp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bloop\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elm-stuff\\'")
  (setq lsp-file-watch-threshold nil)
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 300000000)           ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-keep-workspace-alive nil)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)
  )

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol
;;   :bind (:map lsp-mode-map
;;               ("M-s f" . lsp-ivy-workspace-symbol)))

(use-package lsp-metals
  :bind (:map lsp-mode-map
              ("C-z i" . lsp-java-add-import)))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ("C-q" . lsp-ui-doc-show)
              ("M-e" . lsp-ui-imenu)
              ("M-RET" . lsp-ui-sideline-apply-code-actions)
              ("M-g f" . consult-lsp-symbols)
              :map lsp-ui-imenu-mode-map
              ("C-<return>" . my/lsp-ui-imenu-visit-kill))
  :init
  (defun my/lsp-ui-imenu-visit-kill ()
    (interactive)
    (lsp-ui-imenu--visit)
    (with-current-buffer "*lsp-ui-imenu*"
      (lsp-ui-imenu--kill)))
  :config
  (lsp-ui-doc-mode nil)
  (setq lsp-ui-doc-use-childframe (display-graphic-p)))

(use-package flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treemacs

(use-package treemacs
  :disabled t
  :commands (treemacs)
  :bind (("<f8>" . treemacs)
         ("C-z C-z" . treemacs)
         (:map treemacs-mode-map
               ("M" . treemacs-mark-or-unmark-path-at-point)
               ("<f8>" . treemacs-quit))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; purescript

(use-package purescript-mode
  :commands (purescript-mode)
  :mode (("\\.purs$" . purescript-mode))
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (turn-on-purescript-indentation))))

(use-package psc-ide
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)))
  (bind-key "M-SPC" 'company-complete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rust

(use-package rustic
  :mode (("\\.rs$" . rustic-mode))
  :config
  (setq rustic-lsp-client 'lsp-mode)
  (setq rustic-format-on-save t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emmet mode

;; use C-j to expand css like syntax in html
(use-package emmet-mode
  :commands (emmet-mode)
  :config
  (add-hook 'nxml-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web mode

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.scala.html\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sass-mode

(use-package sass-mode
  :mode ("\\.scss\\'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adoc-mode

(use-package adoc-mode
  :mode "\\.adoc")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yaml mode

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config (setq yaml-indent-offset 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; goto-chg

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; geiser (scheme)

(use-package geiser-guile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mermaid support

(use-package mermaid
  :commands (my/mermaid-compile-file my/mermaid-compile-markdown-code)
  :load-path "lisp"
  :bind (:map markdown-mode-map
              ("C-c C-c r" . my/mermaid-compile-markdown-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; impatient-mode
(use-package impatient-mode
  :commands (impatient-mode)
  :config
;;; Start the server with M-x httpd-start
;;; Tell impatient mode to use it: M-x imp-set-user-filter RET
;;; my/imp-markdown-html RET.
  (defun my/imp-markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rec-mode

(use-package rec-mode
  :mode "\\.rec$")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plantuml mode

(use-package plantuml-mode
  :commands (plantuml-mode)
  :mode "\\.puml"
  :init
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path
        (-find 'f-exists?
               '("/run/current-system/sw/lib/plantuml.jar"
                 "~/.nix-profile/lib/plantuml.jar")))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (setenv "GRAPHVIZ_DOT"
          (-find 'f-exists?
                 '("/run/current-system/sw/bin/dot"
                   "~/.nix-profile/bin/dot"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; groovy

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode))
  :interpreter "groovy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elm

(use-package elm-mode
  :commands (elm-mode)
  :mode "\\.elm"
  :config
;  (add-to-list 'company-backends 'company-elm)
  (setq elm-mode-hook '(elm-indent-simple-mode))
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode)
  (add-hook 'elm-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)))
  (defun my/elm-compile-buffer ()
    (interactive)
    (let ((elm-compile-arguments '("--output=/dev/null")))
      (elm-compile-buffer)))
  (defun my/elm-compile-buffer-html ()
    (interactive)
    (let ((elm-compile-arguments '("--output=index.html")))
      (elm-compile-buffer)))
  (bind-key "C-c C-c" 'my/elm-compile-buffer elm-mode-map)
  (bind-key "C-c h" 'my/elm-compile-buffer-html elm-mode-map)
  (setq elm-tags-on-save t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nix

(use-package nix-ts-mode
  :mode "\\.nix")

(use-package nix-mode
  :config
  (use-package company-nixos-options
    :disabled t
    :commands (company-nixos-options)
    :init
    (eval-after-load 'company
      (add-to-list 'company-backends 'company-nixos-options))))

(defun my/nix-prefetch-url (url)
  (interactive (list (browse-url-url-at-point)))
  (unless url
    (user-error "No url found at point"))
  (let ((sha (shell-command-to-string
              (format "nix-prefetch-url '%s' 2>/dev/null | tail -n1" url))))
    (kill-new (s-trim sha))
    (message "sha256: %s" (s-trim sha))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scad-mode (for OpenSCAD)
(use-package scad-mode
  :mode (("\\.scad" . scad-mode))
  :config
  (add-hook 'scad-mode-hook (lambda () (setq c-basic-offset 2)))
  (defun my/increment-number-at-point-and-save (arg)
    (interactive "p")
    (my/increment-number-at-point arg)
    (save-buffer))
  (defun my/decrement-number-at-point-and-save (arg)
    (interactive "p")
    (my/decrement-number-at-point arg)
    (save-buffer))
  (bind-key "M-<up>" 'my/increment-number-at-point-and-save scad-mode-map)
  (bind-key "M-<down>" 'my/decrement-number-at-point-and-save scad-mode-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sparql-mode
(use-package sparql-mode
  :mode (("\\.sparql" . sparql-mode)
         ("\\.rq" . sparql-mode))
  :config
  (add-hook 'sparql-mode-hook 'company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fish-mode / shell things
(use-package fish-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(use-package fish-completion
  :config
  (global-fish-completion-mode))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'git-radar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solaire-mode

(use-package solaire-mode
  :disabled t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clipetty

(use-package clipetty
  :if (not (display-graphic-p))
  :config
  (global-clipetty-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solr helpers

(use-package solr
  :load-path "lisp"
  :commands (solr-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ansible

(use-package ansible
  :disabled t
  :commands (ansible))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess

(use-package ess-site
  :mode (("\\.R" . ess-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for work

(use-package work
  :load-path "lisp"
  :if (or (my/host-starts-with-p "poros")
          (string-equal user-login-name "sdsc"))
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; envrc
(use-package envrc
  :config
  (envrc-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme

;; needs a compositing wm, e.g.
;; compton  --backend glx --paint-on-overlay --glx-no-stencil  -b
(defun transparency (value)
  "Set the transparency value of the frame window.

The `VALUE' may be 0=transparent to 100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun set-transparency ()
  "Set transparent background."
  (when (display-graphic-p)
    (transparency 90))
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'set-transparency)

(use-package moody
  :disabled t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (minions-mode 1)
  (setq minions-direct '(projectile-mode)))

(let ((size (if (display-graphic-p)
                135
              120))
      (fontname "source code pro medium"))
  (set-face-attribute 'default nil
                          :font fontname
                          :height size))

(use-package doom-themes)

(use-package kaolin-themes)

(defun my/theme-setup ()
  "Setup Emacs theme, including some tweaks."
  (load-theme 'doom-dracula t)
  (setq rainbow-delimiters-max-face-count 4)
  ;; change dirvish highlighted line color for theme doom-1337
  ;;(set-face-attribute 'dirvish-hl-line nil :background "DodgerBlue2")
  (set-transparency)
  (set-background-color "gray8")
  (set-cursor-color "orange"))

(my/theme-setup)

;; must run: `M-x' `all-the-icons-install-fonts'
(use-package all-the-icons)
;; must run: `M-x' `nerd-icons-install-fonts'
(use-package nerd-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs application framework (eaf) - browser, pdf-viewer
(use-package eaf
  :disabled t
  :config
  (setq eaf-browser-default-search-engine "duckduckgo")
  (use-package eaf-browser
    ;; :bind (:map eaf-mode-map
    ;;             ("f" . eaf-py-proxy-open_link)
    ;;             ("M-<left>" . eaf-py-proxy-history_backward))
    :config
    (defun my/open-browser (url &optional args)
      (interactive "p")
      (if args (browse-url-firefox url)
        (eaf-open-browser url)))
    (setq browse-url-browser-function 'my/open-browser))
  (use-package eaf-pdf-viewer)
  (use-package eaf-markdown-previewer)
  (use-package eaf-org-previewer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MacOS tweaks
(when (eq system-type 'darwin)
  (setq ns-right-alternate-modifier 'none)
  (setq ns-right-command-modifier 'none)
  (setq ns-command-modifier 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit-server

;; https://github.com/stsquad/emacs_chrome (not only for chrome)
(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server
  :commands (my/start-server-once)
  :init  (add-hook 'after-init-hook 'my/start-server-once t)
  :config
  (defun my/start-server-once ()
    (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
           (server-file (expand-file-name server-name server-dir)))
      (unless (f-exists? server-file)
        (server-start)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customize stuff by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     default))
 '(mml-secure-openpgp-sign-with-sender t)
 '(send-mail-function 'smtpmail-send-it))


(let ((elapsed (float-time (time-subtract (current-time)
                                          my/emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       my/emacs-start-time))))
               (message "Auto save org buffers every 10 minutes")
               (run-at-time "5 minutes" 600 'org-save-all-org-buffers)
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:slant normal :weight normal :height 120 :width semi-expanded :foundry "SRC" :family "Hack")))))

(provide 'init)
;;; init.el ends here


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
