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

;;(define-key global-map (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme

;; needs a compositing wm, e.g.
;; compton  --backend glx --paint-on-overlay --glx-no-stencil  -b
(defun transparency (value)
   "Set the transparency value of the frame window. `VALUE' may be
0=transparent to 100=opaque."
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (minions-mode 1)
  (setq minions-direct '(projectile-mode)))

(use-package gruvbox-dark-hard-theme
  :config
  (transparency 90)
  (setq rainbow-delimiters-max-face-count 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show parens
(use-package paren
  :config
  (show-paren-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy / counsel / swiper

(use-package ivy
  :demand t
  :diminish (ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-height 15)
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)))

(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . counsel-ibuffer)
         ("M-x" . counsel-M-x)
         ("C-x l" . counsel-locate)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         (:map counsel-find-file-map
               ("C-." . counsel-up-directory))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lisp/mylib

(use-package mylib
  :load-path "lisp"
  :commands (my/host-p my/host-starts-with-p my/copy-to-terminal)
  :demand t
  :bind (("M-j" . my/join-line)
         ("C-x C-y" . my/copy-to-terminal)
         ("C-h C-f" . find-function)
         ("<f1>" . my/visit-now))
  :config
  (transparency 90))

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
  :commands (golden-ratio-mode golden-ratio))


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
(-each '(scala-mode-hook java-mode-hook js2-mode-hook)
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
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-d" . dired-jump)
	 ("C-x d" . dired-jump))
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (use-package dired-filter
    :config
    (bind-key "f" dired-filter-mark-map dired-mode-map)))

(use-package dired-rainbow
  :after dired)

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	      ("i" . dired-subtree-cycle)))

(use-package stripe-buffer
  :disabled t
  :commands (turn-on-stripe-buffer-mode stripe-listify-buffer)
  :init
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  (add-hook 'dired-mode-hook 'stripe-listify-buffer))


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
  :bind (("C-c pp" . ivy-projectile-switch-project))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-remember-window-configs t)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-mode-line-prefix " Ƥ"))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
;  (setq counsel-projectile-switch-project-action 'magit-status)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rest cient

(use-package restclient
  :commands (restclient-mode))


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
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "qutebrowser")))


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
         ("C-<f6>" . vterm-toggle-cd)
         (:map vterm-mode-map
               ("s-n" . vterm-toggle-forward)
               ("s-p" . vterm-toggle-backward)
               ("<f6>" . vterm-toggle-cd)))
  :init
  (setq vterm-toggle-fullscreen-p nil))


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
  (setq org-todo-keyword-faces
        '(("TODO" . "#8b0000")
          ("BUG" . "#8b0000")
          ("DONE" . "#6b8e23")))
  (setq org-deadline-warning-days 10)
  (setq org-log-into-drawer t)
  (setq org-src-fontify-natively t)
  (setq org-loop-over-headlines-in-active-region t)
  (setq org-ellipsis " ⤵");; ⤵ ≫
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
     (sqlite . t)
     (gnuplot . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (elvish . t)
     (C . t)
     (R . t)))
  ;; running scala code with ammonite
  (setq org-babel-scala-command "amm")
  (setq org-babel-scala-wrapper-method "%s")
  (load-file (concat user-emacs-directory "lisp/ob-scala.el"))

  (let ((ditaa-path (s-trim
                     (shell-command-to-string
                      "nix-shell -p ditaa --run 'realpath -e \"$(dirname $(which ditaa))/../lib/ditaa.jar\"'"))))
    (setq org-ditaa-jar-path ditaa-path))

  ;; show all habits in the agenda, press K to hide/show habit items
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-refile-targets '((nil . (:maxlevel . 3)))
        org-reverse-note-order t)
  (setq org-capture-templates
        '(("e" "Ausgaben")
          ("e1" "Ausgabe Sonstiges CHF" entry (file+headline "~/org/expenses/2019.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e2" "Ausgabe Sonstiges EUR" entry (file+headline "~/org/expenses/2019.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e3" "Ausgabe Teilen CHF" entry (file+headline "~/org/expenses/2019.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e4" "Ausgabe Teilen EUR" entry (file+headline "~/org/expenses/2019.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e5" "Mittag BC" entry (file+headline "~/org/expenses/2019.org" "Sonstiges")
           "** Mittag BC   :lebensbedarf:\n %^{date}p %^{chf}p" :prepend t :empty-lines 1))))

(use-package org-bullets
  :if window-system
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

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

(use-package ob-mongo)

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
\\homepage{https://eknet.org}
\\phone[mobile]{+41~(76)~278~4160}
\\address{Arbergstrasse 7c}{8405 Winterthur}{Schweiz}
\[NO-DEFAULT-PACKAGES]
\[NO-PACKAGES]
\[EXTRA]"
                   ("\\section{%s}" . "\\section{%s}")
                   ("\\subsection{%s}" . "\\subsection{%s}")))


    (setq org-koma-letter-default-class "my-letter")))

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
;;; E-Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mu4e

;; from emacs wiki: http://www.emacswiki.org/emacs/mu4e
 ;;; message view action
(defun my/mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
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
  `(,(make-mu4e-context
      :name "posteo.de"
      :enter-func (lambda () (mu4e-message "Switch to posteo.de context"))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg
                                                          '(:to :cc :bcc)
                                                          "@posteo.de$")))
      :vars `((user-mail-address . "eike.kettner@posteo.de")
              (user-full-name . "Eike Kettner")
              (mu4e-maildir . "~/Mail")
              (mu4e-sent-folder . "/posteo/Sent")
              (mu4e-trash-folder . "/posteo/Trash")
              (mu4e-drafts-folder . "/posteo/Drafts")
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://eikek.github.io/mpc4s"))
              (smtpmail-smtp-server . ,(my/password-store-get-key "email/posteo.de" "mailhost"))
              (smtpmail-smtp-user . ,(my/password-store-get-user "email/posteo.de"))
              (smtpmail-smtp-service . 587)
              (smtpmail-auth-credentials . '(,(my/password-store-get-key "email/posteo.de" "mailhost")
                                             587
                                             ,(my/password-store-get-user "email/posteo.de")
                                             ,(password-store-get "email/posteo.de")))))))
(use-package mu4e
  :load-path my/mu4e-find-load-path
  :bind (("<f5>" . mu4e))
  :config
  (setq mu4e-context-policy 'pick-first)
  ;;(setq mu4e-compose-context-policy nil)

  (setq mu4e-contexts (my/mu4e-make-contexts))
  (set-face-foreground 'mu4e-unread-face "deep sky blue")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-maildir "~/Mail")
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mml2015-use 'epg) ;; enable gpg/mime over epg
  (setq org-mu4e-convert-to-html t)
  (eval-after-load 'org
    '(require 'org-mu4e))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval nil)

  (defun my/mu4e-render-html-message ()
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (shr-insert-document dom)
      (goto-char (point-min))))

  (setq mu4e-html2text-command 'my/mu4e-render-html-message)
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
  (-each '("eike.kettner@eknet.org" "eike@eknet.org")
    (lambda (addr)
      (add-to-list 'mu4e-user-mail-address-list addr))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :commands (flycheck-mode))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package cc-mode
  :mode ("\\.java" . java-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))


(use-package lsp-mode
  :hook ((scala-mode . lsp)
         (yaml-mode . lsp)
         (elm-mode . lsp))
  :commands (lsp)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-peek-list-width 80)
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 40)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0)

  (defvar-local my/lsp-ui-doc-toggle-var nil)
  (defun my/lsp-ui-doc-toggle ()
    (interactive)
    (if (not my/lsp-ui-doc-toggle-var)
        (progn
          (lsp-ui-doc-show)
          (setq my/lsp-ui-doc-toggle-var t))
      (progn
        (lsp-ui-doc-hide)
        (setq my/lsp-ui-doc-toggle-var nil))))
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)  
  (bind-key "C-q" 'my/lsp-ui-doc-toggle lsp-ui-mode-map)
  (bind-key "M-." 'lsp-ui-peek-find-definitions lsp-ui-mode-map)
  (bind-key "M-?" 'lsp-ui-peek-find-references lsp-ui-mode-map)
  (bind-key "M-@" 'lsp-ui-peek-find-implementation lsp-ui-mode-map))

;; Add company-lsp backend for metals
(use-package company-lsp
  :commands (company-lsp)
  :init (push 'company-lsp company-backends))

(use-package lsp-java
  :after lsp  
  :config
  (add-hook 'java-mode-hook 'lsp)
  (bind-key "C-M-e" 'lsp-treemacs-errors-list java-mode-map)
  (bind-key "C-M-o" 'lsp-java-organize-imports java-mode-map)
  (bind-key "C-M-a" 'lsp-java-add-import java-mode-map)
  (bind-key "C-M-v" 'lsp-java-extract-to-local-variable java-mode-map)
  (bind-key "C-M-r" 'lsp-rename java-mode-map)
  (bind-key "M-n" 'flycheck-next-error java-mode-map)
  (bind-key "M-p" 'flycheck-previous-error java-mode-map))

(use-package lsp-elm)

(use-package treemacs
  :commands (treemacs)
  :bind (("<f8>" . treemacs)
         (:map treemacs-mode-map
               ("<f8>" . treemacs-quit))))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols lsp-treemacs-java-deps-list))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t))

(use-package dap-java
  :after (lsp-java))



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
  :mode "\\.yml"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geiser (scheme)

(use-package geiser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flymd
  :commands (flymd-flyit)
  :config
  (setq
   flymd-close-buffer-delete-temp-files t
   flymd-output-directory temporary-file-directory))

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
  (add-to-list 'company-backends 'company-elm)
  (setq elm-tags-on-save nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nix

(use-package nix-mode
  :mode "\\.nix"
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
    (user-error "No url found at point."))
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
;;; Ansible

(use-package ansible
  :commands (ansible))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess

(use-package ess-site
  :mode (("\\.R" . ess-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for work

(use-package bluecare
  :load-path "lisp"
  :if (my/host-starts-with-p "n76")
  :demand t
  :init
  (add-hook 'org-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-<return>") 'bc/org-browse-jira))))



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
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "DejaVu Sans Mono")))))


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
