;;; init.el -- emacs magic

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
  (require 'bind-key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some core packages

(use-package dash)
(use-package s)
(use-package f)
(use-package hydra)

(use-package mylib
  :load-path "lisp"
  :commands (my/host-p my/host-starts-with-p transparency)
  :bind (("M-j" . my/join-line)
         ("C-h C-f" . find-function)))

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

;; some emacs gui tweaks
(setq use-file-dialog nil)
;; draw a red frame around images when cursor is on
;; see [[mu4e:msgid:87vb53cswe.fsf@gnus.org][Re: Change appearance of selected image]]
(setq default-frame-alist
       (nconc (list '(mouse-color . "red")
                    '(cursor-color . "red"))
              default-frame-alist))
(setq c-basic-indent 2)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
 (setq inhibit-startup-screen t)

(setq scroll-preserve-screen-position t)

(fset 'yes-or-no-p 'y-or-n-p)

(line-number-mode t)
(column-number-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(blink-cursor-mode 0)

;; deactivate suspend keys in graphic mode
(when (display-graphic-p)
  (unbind-key "C-x C-z")
  (unbind-key "C-z"))

;; set emacs window title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Emacs: " (abbreviate-file-name (buffer-file-name)))
                 "%b"))))

;; no opaque background in terminals
(defun no-background-terminal (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(defun no-background-frame ()
  (no-background-terminal (selected-frame)))

(add-hook 'window-setup-hook 'no-background-frame)
(add-hook 'after-make-frame-functions 'no-background-terminal)


;; ;; see https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
;; ;; set the fall-back font
;; ;; this is critical for displaying various unicode symbols, such as those used in my init-org.el settings
;; ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))


(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)



;; show parens
(use-package paren
  :config
  (show-paren-mode t))

;; winner mode
(use-package winner
  :config
  (winner-mode t))

;; save-place
(use-package saveplace
  :config
  (setq-default save-place t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; windmove + move-buffers

(use-package windmove
  :bind (("s-C-<right>" . windmove-right)
         ("s-C-<left>" . windmove-left)
         ("s-C-<up>" . windmove-up)
         ("s-C-<down>" . windmove-down)
         ("S-C-<up>" . enlarge-window)
         ("S-C-<down>" . shrink-window)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<left>" . shrink-window-horizontally)))

(use-package buffer-move
  :bind (("s-<right>" . buf-move-right)
         ("s-<left>" . buf-move-left)
         ("s-<up>" . buf-move-up)
         ("s-<down>" . buf-move-down)))

(defhydra hydra-other-window (global-map "C-ö")
  "change window: "
  ("RET" (lambda () (interactive)) "Exit" :exit t)
  ("ö" other-window "Next window")
  ("C-ö" other-window "Next window")
  ("p" (other-window -1) "Previous window")
  ("<right>" windmove-right "Right window")
  ("<left>" windmove-left "Left window")
  ("<up>" windmove-up "Up window")
  ("<down>" windmove-down "Down window"))

(defhydra hydra-tweak-window (global-map "C-ä")
  ("RET" (lambda () (interactive)) "Exit" :exit t)
  ("C-<return>" golden-ratio "Golden Ratio" :exit t)
  ("<right>" buf-move-right "Buffer right")
  ("<left>" buf-move-left "Buffer left")
  ("<up>" buf-move-up "Buffer up")
  ("<down>" buf-move-down "Buffer down")
  ("C-<up>" enlarge-window "Enlarge Vert.")
  ("C-<down>" shrink-window "Shrink Vert.")
  ("C-<right>" enlarge-window-horizontally "Enlarge Horiz.")
  ("C-<left>" shrink-window-horizontally "Shrink Horiz."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eyebrowse – a window config manager

(use-package eyebrowse
  :diminish eyebrowse-mode
  :commands (eyebrowse-switch-to-window-config-2)
  :bind* (("C-<" . eyebrowse-prev-window-config)
          ("C->" . eyebrowse-next-window-config))
  :config
  (eyebrowse-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uniquify

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rainbow-delimiters

(use-package rainbow-delimiters
  :defer 2
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
;;; helm

(use-package helm
  :diminish helm-mode
  :commands helm-moccur
  :bind (("C-c h" . helm-command-prefix)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-find-files-map
         ("C-." . helm-find-files-up-one-level))
  :config

  (require 'helm-config)
  (use-package helm-ag
    :defer 2)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-in-side-p           nil
   ;; move to end or beginning of source when reaching top or bottom of source.
   helm-move-to-line-cycle-in-source     t
   ;; search for library in `require' and `declare-function' sexp.
   helm-ff-search-library-in-sexp        t
   ;; scroll 8 lines other window using M-<next>/M-<prior>
   helm-scroll-amount                    8
   helm-ff-file-name-history-use-recentf t
   helm-ff-skip-boring-files             t)

  (helm-autoresize-mode 1)
  (helm-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop

(use-package helm-swoop
  :defer 2
  ;; C-c SPC overrides stuff in orgmode
  :bind* (("C-c SPC" . helm-swoop)))

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
;;; linum

(use-package nlinum
  :commands (nlinum-mode)
  :defer 2
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key (replaces guide-key)

(use-package which-key
  :defer 2
  :diminish which-key-mode
  :commands which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; golden-ratio

(use-package golden-ratio
  :diminish golden-ratio-mode
  :commands (golden-ratio-mode golden-ratio))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anzu

(use-package anzu
  :defer 2
  :diminish anzu-mode
  :config (global-anzu-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; theme

(use-package eziam-dark-theme
  :disabled t
  :config
  (transparency 90)
  (setq rainbow-delimiters-max-face-count 3))

(use-package badger-theme
  :disabled t)

(use-package sexy-monochrome-theme
  :disabled t
  :config
  (transparency 85))

(use-package solarized-theme
  :disabled t
  :config
  (load-theme 'solarized-dark t))

(use-package reykjavik-theme
  :disabled t)

(use-package spike-theme
  :disabled t)

(use-package boron-theme
  :disabled t
;;  :if (display-graphic-p)
  :config
  (transparency 90))

(use-package minimal-theme
  :disabled t)

(use-package zweilight-theme
  :disabled t)

(use-package darktooth-theme
  :disabled t
  ;;  :if (not (display-graphic-p))
  :config
  (transparency 90))

(use-package soft-stone-theme
;;  :disabled t
  :if (display-graphic-p))

(use-package soft-charcoal-theme
  :disabled t
  :config
  (transparency 95))

(use-package colonoscopy-theme
  :disabled t
  :config
  (transparency 95))

(use-package deep-thought-theme
  :disabled t
  :if (display-graphic-p))

(use-package planet-theme
  :disabled t
  :if (display-graphic-p))

(use-package leuven-theme
  :disabled t
  :if (display-graphic-p))

(use-package powerline
  :disabled t
  :config
  (powerline-default-theme))

(use-package smart-mode-line
  :demand t
  :init
  (use-package rich-minority
    :defer t)
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; htmlize

(use-package htmlize
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode

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
    :commands (org-babel-execute:restclient)
    :defer t)
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
     (C . t)
     (R . t)))
  ;; running scala code with ammonite
  (setq org-babel-scala-command "amm")
  (setq org-babel-scala-wrapper-method "%s")

  (let ((ditaa-path (s-trim (shell-command-to-string "nix-shell -p ditaa --run 'realpath -e \"$(dirname $(which ditaa))/../lib/ditaa.jar\"'"))))
    (setq org-ditaa-jar-path ditaa-path))

  ;; org-habit
  (add-to-list 'org-modules 'org-habit t)
  ;; show all habits in the agenda, press K to hide/show habit items
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-refile-targets '((nil . (:maxlevel . 3)))
        org-reverse-note-order t)
  (setq org-capture-templates
        '(("e" "Ausgaben")
          ("e1" "Ausgabe Sonstiges CHF" entry (file+headline "~/org/expenses/2018.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e2" "Ausgabe Sonstiges EUR" entry (file+headline "~/org/expenses/2018.org" "Sonstiges")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e3" "Ausgabe Teilen CHF" entry (file+headline "~/org/expenses/2018.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{chf}p" :prepend t :empty-lines 1)
          ("e4" "Ausgabe Teilen EUR" entry (file+headline "~/org/expenses/2018.org.org" "Teilen")
           "** %^{what} %^g\n %^{date}p %^{eur}p" :prepend t :empty-lines 1)
          ("e5" "Mittag BC" entry (file+headline "~/org/expenses/2018.org" "Sonstiges")
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


(defun my/org-get-clock-string ()
  "A string for currently clocked task. Used from stumpwm."
  (when (fboundp 'org-clocking-p)
    (when (org-clocking-p)
      (let ((str (org-clock-get-clock-string)))
        (substring-no-properties str 1 (1- (length str)))))))

(use-package org-clock
  :commands (my/org-clock-admin my/org-clock-bc-org)
  :bind (("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-x" . org-clock-in-last)
         ("C-c C-x C-q" . org-clock-cancel)
         ("<f1>" . my/org-clock-admin)
         ("<f2>" . my/org-clock-bc-org))
  :config
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist 'history)
  (setq org-clock-history-length 25)
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (load "helm-org-clock.el")
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

(use-package ox-publish
  :config
  (setq org-publish-project-alist
        '(("eknet-notes"
           :base-directory "~/org/eknet-site/"
           :base-extension "org"
           :publishing-directory "~/public_html"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-head-extra "<meta charset=\"utf-8\" /> <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /> <style type=\"text/css\"> body { max-width: 950px; margin: auto; padding: 0 25px; } #postamble { font-size: small; color: gray; } #postamble a { color: black; } h1.title { padding-bottom: 10px; margin: 22px 0px 33px; border-bottom: 1px solid #EEE; }</style>"
           :auto-preamble t)
          ("eknet-static"
           :base-directory "~/org/eknet-site/"
           :base-extension "htm\\|html\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
           :publishing-directory "~/public_html"
           :recursive t
           :publishing-function org-publish-attachment)
          ("eknet-publish"
           :components ("eknet-notes" "eknet-static")))))

(use-package presentation
  :load-path "lisp"
  :defer t
  :commands (my/presentation-enable my/presentation-disable my/presentation-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notelog

(defun my/add-note-headers (meta)
  "Adds some html headers to each note  file."
  (let ((prefix (apply #'concat
                       (loop for i from 0 to (plist-get meta :subdir-level)
                             collect "../")))
        (date (plist-get meta :date))
        (headers '("#+html_head: <link href='%scss/styles/color-brewer.css' rel='stylesheet'/>"
                   "#+html_head: <script src='%sjs/jquery-2.1.1.min.js'></script>"
                   "#+html_head: <script src='%sjs/highlight.pack.js'></script>"
                   "#+html_head: <script src='%sjs/highlight-load-org.js'></script>"
                   "#+html_head: <link rel='stylesheet' href='%scss/kube.min.css'/>"
                   "#+html_head: <script src='%sjs/kube.min.js'></script>")))
    (goto-char (point-min))
    (dolist (head headers)
      (if (cl-find ?% head)
          (insert (format head prefix) "\n")
        (insert head "\n")))
    (if date
        (insert "#+date: " date "\n")
      (insert "#+date: " (format-time-string "%Y-%m-%d" (plist-get meta :lastmod)) "\n"))
    (insert "#+language: en\n")
    (insert "#+keywords: " (mapconcat #'identity (plist-get meta :tags) " ") "\n")
    (insert "#+options: <:nil ^:{} d:nil num:nil tags:t \n")))

(defun my/remove-time-from-journal-notes (meta)
  (let ((source (plist-get meta :source)))
    (when (string-match-p "[0-9]+" (file-name-base source))
      (goto-char (point-min))
      (if (search-forward "#+title: " nil t)
          (progn
            (delete-char 6)
            (plist-put meta :title (buffer-substring-no-properties (point) (point-at-eol))))
        meta))))

(defun my/generate-website (&optional arg)
  (interactive "P")
  (let ((org-html-htmlize-output-type nil)
        (org-publish-use-timestamps-flag (if arg nil org-publish-use-timestamps-flag)))
    (notelog-generate-default-notes)
    (org-publish "eknet-publish")))

(defun my/publish-website (&optional arg)
  (interactive "P")
  (when arg
    (my/generate-website))
  (shell-command "~/org/eknet-site/sync-website.sh &"))

(use-package notelog
  :commands (notelog-generate-default-notes)
  :defer t
  :config
  (setq notelog-default-marker-tag "pub")
  (setq notelog-default-output-directory "~/org/eknet-site/main")
  (setq notelog-default-subfolder-fn (notelog-subfolder-per-tag '("linux" "dev")))
  (setq notelog-default-input-files '("~/org/blog.org" "/home/eike/org/journal"))
  (add-to-list 'notelog-default-modify-fns 'my/add-note-headers)
  (add-to-list 'notelog-default-modify-fns 'my/remove-time-from-journal-notes)
  (setq notelog-default-index-template "~/org/eknet-site/index-template.org"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-scratch

(use-package org-scratch
  :load-path "lisp"
  :bind ("C-x C-n" . org-scratch/new-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; myshelf

(use-package myshelf
  :load-path "lisp"
  :commands (myshelf-insert-file myshelf-find-file myshelf-toc-synchronise)
  :config
  (setq myshelf-directory "~/shelf"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my ferien

(use-package my-ferien
  :load-path "lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whitespace-cleanup-mode

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :defer 2
  :config
  (global-whitespace-cleanup-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delsel

(use-package delsel
  :config
  (pending-delete-mode 1)
  :defer 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move-text

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet setup

(use-package yasnippet
  :defer 2
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode yas-reload-all)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (:map yas-minor-mode-map
         ("C-<tab>" . yas-expand)
;         ("C-i" . yas-expand)
         )
  :config
  (use-package helm-c-yasnippet
    :commands helm-yas-complete
    :defer t)
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region

(use-package expand-region
  :bind (("M-2" . er/expand-region)))


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
;;; global

;; run `gtags` command in project root, that creates three tag files
;; 1. M-x ggtags-mode
;; 2. M-. to find definitions/declarations
;; 3. C-u M-. to find references
;; 4. If multiple tags are found, use M-n and M-p to navigate between them.
;;
;; Handling multiple matches
;; +++++++++++++++++++++++++

;; When a search finds multiple matches, a buffer named
;; ``*ggtags-global*`` is popped up and ``ggtags-navigation-mode`` is
;; turned on to facilitate locating the right match.
;; ``ggtags-navigation-mode`` makes a few commands in the
;; ``*ggtags-global*`` buffer globally accessible:

;; `M-n`   Move to the next match.
;; `M-p`   Move to the previous match.
;; `M-}`   Move to next file.
;; `M-{`   Move to previous file.
;; `M-=`   Move to the file where navigation session starts.
;; `M-<`   Move to the first match.
;; `M->`   Move to the last match.
;; `C-M-s` or `M-s s`
;;         Use ``isearch`` to find the match.
;; `RET`   Found the right match so exit navigation mode. Resumable by
;;         `M-x tags-loop-continue`.
;; `M-,` (``M-*`` if Emacs < 25)
;;         Abort and go back to the location where the search was started.
(use-package ggtags
  :commands ggtags-mode
  :config
  (use-package helm-gtags :demand t)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))



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
;;; impatient mode

;; publish a buffer through local http server, allows live changes
;; start http server by `M-x httpd-start` and visit localhost:8080/imp
;; (use-package impatient-mode
;;   :commands (impatient-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rainbow-mode

(use-package rainbow-mode
  :config
  (add-hook 'nxml-mode-hook 'rainbow-mode)
  (add-hook 'sgml-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook  'rainbow-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; adoc-mode

(use-package adoc-mode
  :mode "\\.adoc")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yaml mode

(use-package yaml-mode
  :mode "\\.yml")


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
;;; java

(use-package cc-mode
  :mode ("\\.java" . java-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; groovy

(use-package groovy-mode
  :mode (("\\.groovy" . groovy-mode))
  :interpreter "groovy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js2 mode

(use-package js2-mode
  :mode (("\\.js" . js2-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scala

(defun my/scala-mode-setup ()
  (set-face-attribute 'scala-font-lock:var-face nil
                      :weight 'bold
                      :underline t
                      :foreground "#FF9090")

  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  ;; (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  (local-set-key (kbd "RET") '(lambda ()
                                (interactive)
                                (newline-and-indent)
                                (scala-indent:insert-asterisk-on-multiline-comment)))

  ;; Bind the 'join-line' command to M-j. This command is normally
  ;; bound to M-^ which is hard to access, especially on some European
  ;; keyboards. The 'join-line' command has the effect or joining the
  ;; current line with the previous while fixing whitespace at the
  ;; joint.
  ;;    (local-set-key (kbd "M-j") 'join-line)

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>")
                 'scala-indent:indent-with-reluctant-strategy))

(use-package scala-mode
  :mode ("\\.scala" "\\.sbt" "\\.sc")
  :interpreter ("scsh" "amm")
  :config
  (add-hook 'scala-mode-hook 'my/scala-mode-setup))

(use-package popup
  :demand t)

(use-package ensime
  :commands (ensime)
  :config
  (use-package sbt-mode
    :demand t)
  (require 'ensime-company)
  (setq ensime-startup-snapshot-notification nil
        ensime-prefer-noninteractive nil
        ensime-startup-notification nil)

  ;; there are some great Scala yasnippets, browse through:
  ;; https://github.com/AndreaCrotti/yasnippet-snippets/tree/master/scala-mode
  (add-hook 'scala-mode-hook #'yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elm

(use-package elm-mode
  :commands (elm-mode)
  :mode "\\.elm"
  :config
  (add-to-list 'company-backends 'company-elm)
  (setq elm-tags-on-save nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure

(use-package clojure-mode
  :mode (("\\.clj" . clojure-mode)))

(use-package monroe
  :commands (monroe)
  :config
  (add-hook 'clojure-mode-hook 'clojure-enable-monroe))

(use-package cider
  :commands (cider-jack-in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slime (common lisp)

(use-package slime
  :commands (slime-mode slime)
  :config
  (setq inferior-lisp-program "/run/current-system/sw/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nix

(use-package nix-mode
  :mode "\\.nix"
  :config
  (use-package helm-nixos-options
    :bind (:map nix-mode-map
                ("C-c C-n" . helm-nixos-options)))
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
;;; ess

(use-package ess-site
  :mode (("\\.R" . ess-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired

(use-package dired
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (use-package dired-filter
    :config
    (bind-key "f" dired-filter-mark-map dired-mode-map))
  (use-package dired-subtree
    :demand t
    :bind (:map dired-mode-map
           ("i" . dired-subtree-cycle)))
  (use-package dired-rainbow
    :demand t))

(use-package stripe-buffer
  :commands (turn-on-stripe-buffer-mode stripe-listify-buffer)
  :init
  (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
  (add-hook 'dired-mode-hook 'stripe-listify-buffer))


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
          browse-url-generic-program "conkeror")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit

(use-package magit
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-popup-show-common-commands nil)
  (setq magit-popup-manpage-package 'woman))

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
;;; git-gutter

(use-package fringe-helper
  :if window-system
  :demand t)

(use-package git-gutter
  :diminish git-gutter-mode
  :commands git-gutter-mode
  :init (add-hook 'prog-mode-hook 'git-gutter-mode)
  :config
  (use-package git-gutter-fringe
    :if window-system))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stumpwm

(use-package stumpwm-mode
  :commands (stumpwm-mode)
  :mode (("\\.stumpwmrc" . stumpwm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emms

(use-package emms
  :commands (emms emms-play-dired emms-add-dired)
  :bind (("<XF86AudioNext>" . emms-next)
         ("<XF86AudioPrev>" . emms-pause)
         ("<XF86AudioPlay>" . emms-pause))
  :config
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-info-mp3info)
  (require 'emms-info-ogginfo)
  (require 'emms-cache)
  (require 'emms-mode-line-icon)
  (require 'emms-stream-info)
  (setq emms-mode-line-format "ζ")
  (setq emms-player-list '(emms-player-mplayer))
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-track-description-function 'emms-track-simple-description))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-expense

(use-package org-expenses
  :bind (("C-c e" . org-expenses/expense-view))
  :config
  (when (file-exists-p "/run/current-system/sw/bin/sqlite3")
    (setq org-expenses/sqlite-cmd "/run/current-system/sw/bin/sqlite3"))

  (setq org-expenses/sqlite-db-file "~/.exp.db")
  (setq org-expenses/files "~/org/expenses/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile

(use-package projectile
  :defer 2
  :bind (("C-c pp" . helm-projectile-switch-project))
  :config
  (use-package helm-projectile
    :demand t
    :config
    (helm-projectile-on))

  (setq projectile-completion-system 'helm)
  (setq projectile-remember-window-configs t)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-mode-line '(:eval
                               (format " Ƥ[%s]"
                                       (projectile-project-name))))
  (projectile-global-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-lines

(use-package hide-lines
  :commands (hide-lines-show-all
             hide-lines
             hide-lines-matching
             hide-lines-not-matching))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password-store

(use-package password-store
  :commands (password-store-get
             my/password-store-get-entry
             my/password-store-get-key
             my/password-store-get-user)
  :defer 2
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magnatune

(use-package magnatune
  :bind (("C-<f11>" . magnatune-helm))
  :config
  (setq magnatune-username "eikek")
  (setq magnatune-password (password-store-get "internet/magnatune"))

  (when (file-exists-p "/run/current-system/sw/bin/sqlite3")
    (setq magnatune-sqlite-cmd "/run/current-system/sw/bin/sqlite3"))
  (add-hook 'magnatune-open-urls-hook 'magnatune-emms-url-hook)
  (require 'magnatune-helm))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gnuplot

(use-package gnuplot
  :mode ("\\*.gp". gnuplot-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pass

(use-package pass
  :commands pass
  :config
  (defun my/pass-swoop-tap ()
    (interactive)
    (let* ((entry (pass-entry-at-point))
           (word (if entry (car (reverse (s-split "/" entry t))))))
      (when word
        (helm-swoop :$query word))))
  (bind-key "S" 'my/pass-swoop-tap pass-mode-map)
  (bind-key "s" 'helm-swoop pass-mode-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chee

(use-package chee
  :load-path "~/workspace/projects/chee/emacs"
  :if (f-exists-p "~/workspace/projects/chee/emacs")
  :commands (chee-query-open)
  :bind (("C-c C-s" . chee-query-open))
  :config
  (chee-setup-default)
  (use-package chee-minor
    :load-path "~/workspace/projects/chee/emacs"
    :config
    (chee-minor-rec-setup)
    (let ((dir "/mnt/nas/safe/fotos"))
      (when (f-exists-p dir)
        (setq chee-default-repository-dir "/mnt/nas/safe/fotos")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dict.cc

(use-package dictcc
  :commands (dictcc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rest cient

(use-package restclient
  :commands (restclient-mode))

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
      :name "eknet.org"
      :enter-func (lambda () (mu4e-message "Switch to eknet.org context"))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg
                                                          '(:to :cc :bcc)
                                                          "@eknet.org$")))
      :vars `((user-mail-address . "eike@eknet.org")
              (user-full-name . "Eike")
              (mu4e-maildir . "~/Mail")
              (mu4e-sent-folder . "/eknet/Sent")
              (mu4e-trash-folder . "/eknet/Trash")
              (mu4e-drafts-folder . "/eknet/Drafts")
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://eikek.github.io/sharry"))
              (smtpmail-smtp-server . ,(my/password-store-get-key "email/eknet.org" "mailhost"))
              (smtpmail-smtp-user . ,(my/password-store-get-user "email/eknet.org"))
              (smtpmail-smtp-service . 25)
              (smtpmail-auth-credentials . '(,(my/password-store-get-key "email/eknet.org" "mailhost")
                                             25
                                             ,(my/password-store-get-user "email/eknet.org")
                                             ,(password-store-get "email/eknet.org")))))
    ,(make-mu4e-context
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
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://eikek.github.io/sharry"))
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
  (set-face-foreground 'mu4e-unread-face "#4169e1")
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

  (add-to-list 'mu4e-bookmarks
               `(,(concat "date:2w..now AND NOT m:/eknet/spam AND NOT "
                          "m:/eknet/LearnSpam AND NOT "
                          "m:/eknet/lists* ")
                 "No mailinglists, no spam" ?c))

  (setq
   mu4e-view-show-addresses t
   mu4e-use-fancy-chars t
   mu4e-view-show-images t
   mu4e-view-image-max-width 800)
  ;; setup helm to find email addresses from mu4e
  (setq my/mu4e-emails-helm-source
        '((name . "Emails")
          (candidates . mu4e~contacts-for-completion)
          (action . (lambda (candidate)
                      (insert candidate)
                      (message "%s" candidate)))))

  (defun my/mu4e-find-email ()
    (interactive)
    (helm :sources 'my/mu4e-emails-helm-source)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jabber

(use-package jabber
  :commands (jabber-connect-all)
  :bind-keymap (("C-c C-j" . jabber-global-keymap))
  :config
  (setq
   jabber-history-enabled t
   jabber-use-global-history nil
   jabber-history-dir "~/.emacs.d/jabber-history"
   jabber-history-enable-rotation t
   jabber-history-size-limit 1024)

  (setq jabber-account-list
        `(("eike@eknet.org"
           (:password . ,(password-store-get "eknet.org/www"))
           (:connection-type . starttls)
           (:network-server . "eknet.org"))))

  (unless (-find (lambda (theme)
                   (s-starts-with-p "solarized" (symbol-name theme)))
                 custom-enabled-themes)
    (set-face-attribute 'jabber-chat-prompt-local nil
                        :weight 'normal
                        :foreground "khaki")
    (set-face-attribute 'jabber-roster-user-online nil
                        :weight 'bold
                        :slant 'normal
                        :foreground "khaki"))

  (setq jabber-alert-presence-message-function
        'jabber-presence-only-chat-open-message)

  (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

;;  (define-jabber-alert ekalert "Show message to stumpwm" 'my/jabber-message)
;;  (add-hook 'jabber-alert-message-hooks 'jabber-message-ekalert)
  (jabber-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elfeed

(use-package elfeed
  :defer t
  :commands elfeed
  :config
  (setq elfeed-feeds
        (append
         (list (concat "https://bag.eknet.org/api/eike/entries/rss/"
                       (password-store-get "eknet.org/sitebag-token")
                       "?archived=false&complete=true"))
         '("http://planet.clojure.in/atom.xml"
           "http://feeds.feedburner.com/Clojure/coreBlog"
           "http://planet.emacsen.org/atom.xml"
           "http://whattheemacsd.com/atom.xml"
           "http://blog.magnatune.com/atom.xml"
           "http://endlessparentheses.com/atom.xml"
           "http://www.javaspecialists.eu/archive/tjsn.rss")
         elfeed-feeds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neotree

(use-package neotree
  :bind* ("<f8>" . neotree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hyperbole

(use-package hyperbole
  :disabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logview mode

(use-package logview
  :commands (logview-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for work

(use-package bluecare
  :load-path "lisp"
  :if (my/host-starts-with-p "bluecare")
  :demand t
  :init
  (add-hook 'org-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-<return>") 'bc/org-browse-jira))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server

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
 '(package-selected-packages (quote (oauth2 use-package)))
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
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

