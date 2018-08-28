;;; bluecare --- some fns regarding bc

(defun bc/pretty-print-mongo-json (&optional n)
  (interactive (list 2))
  (let ((objregexp "\\s-*\\({\\|\\[\\)")
        (beg (point))
        (end (make-marker)))
    (search-forward-regexp objregexp)
    (setq beg (point))
    (goto-char (1- (point)))
    (forward-sexp 1)
    (setq end (point-marker))
    (goto-char beg)
    (while (< (point) (marker-position end))
      (insert "\n")
      (-each (-repeat n " ") 'insert)
      (if (looking-at-p objregexp)
          (bc/pretty-print-mongo-json (+ 2 n))
        (search-forward "\" : ")
        (if (looking-at-p objregexp)
            (bc/pretty-print-mongo-json (+ 2 n))
          (if (search-forward ", \"" nil t)
              (goto-char (1- (point)))
            (goto-char (marker-position end))))))
    (set-marker end nil)
    (goto-char (1- (point)))
    (insert "\n")
    (-each (-repeat (- n 2) " ") 'insert)
    (when (eq 2 n)
      (goto-char beg))))



(defun bc/org-browse-jira ()
  "Browse the jira issue of current headline. The issue is taken
from category and tag. Category is the project name and each tag
in issue number."
  (interactive)
  (let ((baseurl "https://jira/browse/")
        (category (org-get-category nil t))
        (projects '(("I" . "LARNAGS")
                    ("L" . "LARNAGS")
                    ("B" . "BLUEC")
                    ("P" . "PATSTAMM")
                    ("W" . "BIW")
                    ("H" . "HINCLIENT")
                    ("R" . "ZRC")
                    ("Z" . "ZRLA")))
        (tag (org-get-tags-at nil t)))
    (-each tag
      (lambda (name)
        (when (string-match "[A-Z][0-9]+$" name)
          (let ((head (substring name 0 1))
                (issue (substring name 1)))
            (browse-url (concat baseurl
                                (or (rest (assoc head projects)) category)
                                "-"
                                issue))))))))


(defun bc/org-copy-subtree-as-confluence (arg)
  "Convert current subtree to confluence markup and put it in the
clipboard. Title and TOC is not included unless prefix arg is
given."
  (interactive "P")
  (require 'ox-confluence)
  (let ((outbuffer "*org CONFLUENCE Export*")
        (org-export-show-temporary-export-buffer nil))
    (save-excursion
      (save-restriction
        (org-show-subtree)
        (org-confluence-export-as-confluence nil
                                             t t (not arg))))
    (with-current-buffer outbuffer
      (kill-new (buffer-string)))))


(defun my/pdf-extract-text (file &optional method)
  "Extract text from a pdf (doing ocr)."
  (interactive (list (buffer-file-name) (completing-read "Via: " '(pdfbox jpedal) nil t)))
  (unless (f-exists? file)
    (user-error "file does not exist"))
  (let* ((pdfimage (executable-find "pdfimage.scsh"))
         (tesseract (executable-find "tesseract"))
         (via (or method "pdfbox"))
         (tempimagedir (make-temp-file "pdf-extract" t))
         (outimage (f-join tempimagedir (concat (file-name-base file)
                                                "-" via ".jpg")))
         (buf (get-buffer-create (format "*%s-%s*" (file-name-base file) via))))
    (message "convert first page of pdf to an image …")
    (shell-command (concat pdfimage " "
                           (if (s-equals-p via "pdfbox") "--no-jpedal " "--no-pdfbox ")
                           file " "
                           tempimagedir))
    (unless (f-exists? outimage)
      (error "output image file not found: %s" outimage))
    (with-current-buffer buf
      (erase-buffer)
      (message "Doing OCR with tesseract …")
      (shell-command (concat tesseract " "
                             outimage " "
                             "stdout "
                             "-l deu -psm 1 "
                             "--tessdata /run/current-system/sw/share/tessdata")
                     (current-buffer)
                     (current-buffer))
      (mark-whole-buffer)
      (fill-paragraph nil t))
    (switch-to-buffer buf nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings

(use-package excorporate
  :commands (excorporate)
  :init
  (setq excorporate-configuration
        '("eike.kettner@bluecare.ch" . "https://webmail/ews/exchange.asmx"))
  :config
  (require 'excorporate-org))


(defun my/mu4e-make-contexts ()
  `(,(make-mu4e-context
      :name "bluecare"
      :enter-func (lambda () (mu4e-message "Switch to bluecare context"))
      :match-func (lambda (msg)
                    (when msg
                      (mu4e-message-contact-field-matches msg
                                                          '(:to :cc :bcc)
                                                          "@bluecare.ch$")))
      :vars `((user-mail-address . "eike.kettner@bluecare.ch")
              (user-full-name . "Eike Kettner")
              (mu4e-maildir . "~/Mail")
              (mu4e-sent-folder . "/bluecare/Sent")
              (mu4e-trash-folder . "/bluecare/Trash")
              (mu4e-drafts-folder . "/bluecare/Drafts")
              (mu4e-compose-signature . ,(concat "http://www.bluecare.ch\n"))
              (smtpmail-smtp-server . ,(my/password-store-get-key "bluecare/login" "mailhost"))
              (smtpmail-smtp-user . ,(my/password-store-get-user "bluecare/login"))
              (smtpmail-smtp-service . 25)
              (smtpmail-auth-credentials . '(,(my/password-store-get-key "bluecare/login" "mailhost")
                                             25
                                             ,(my/password-store-get-user "bluecare/login")
                                             ,(password-store-get "bluecare/login")))))
    ,(make-mu4e-context
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
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://eikek.github.io/mpc4s"))
              (smtpmail-smtp-server . "localhost")
              (smtpmail-smtp-user . ,(my/password-store-get-user "email/eknet.org"))
              (smtpmail-smtp-service . 2587)
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
              (mu4e-compose-signature . ,(concat "GPG/PGP: AD7AC35E\nhttps://eikek.github.io/mpc4s"))
              (smtpmail-smtp-server . "localhost")
              (smtpmail-smtp-user . ,(my/password-store-get-user "email/posteo.de"))
              (smtpmail-smtp-service . 3587)
              (smtpmail-auth-credentials . '("localhost"
                                             3587
                                             ,(my/password-store-get-user "email/posteo.de")
                                             ,(password-store-get "email/posteo.de")))))))

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-bookmarks
               '("maildir:/bluecare/*" "BlueCare" ?b))
  (setq mu4e-get-mail-command "offlineimap -c ~/.offlineimap-bluecarerc"))


(use-package play-routes-mode
  :mode "routes")

(with-eval-after-load 'jabber
  (setq jabber-account-list
        `(("eike@eknet.org"
           (:password . ,(password-store-get "eknet.org/www"))
           (:connection-type . starttls)
           (:network-server . "localhost")))))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-prefer-current-team t)
  :config
  (setq slack-buffer-emojify t)
  (slack-register-team
   :name "bluecare"
   :default t
   :client-id (my/password-store-get-key "bluecare/slack" "clientid")
   :client-secret (my/password-store-get-key "bluecare/slack" "clientsecret")
   :token (my/password-store-get-key "bluecare/slack" "token")
   :subscribed-channels '(production random general)))


(use-package sql
  :config
  (setq sql-ms-program "~/bin/sqsh")
  (setq sql-sybase-program "~/bin/sqsh")
  (setq sql-ms-options '("-w" "300" "-n" "on")))

(defun sql-comint-ms (product options)
  "Create comint buffer and connect to Microsoft SQL Server."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options))
    (if (not (string= "" sql-server))
        (setq params (append (list "-S" sql-server) params)))
    (if (not (string= "" sql-database))
        (setq params (append (list "-D" sql-database) params)))
    (if (not (string= "" sql-user))
    (setq params (append (list "-U" sql-user) params)))
    (if (not (string= "" sql-password))
    (setq params (append (list "-P" sql-password) params))
      (if (string= "" sql-user)
      ;; if neither user nor password is provided, use system
      ;; credentials.
      (setq params (append (list "-E") params))
    ;; If -P is passed to ISQL as the last argument without a
    ;; password, it's considered null.
    (setq params (append params (list "-P")))))
    (sql-comint product params)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logview settings

(use-package logview
  :init
  (let ((sec (car (current-time-zone))))
    (if (= sec 7200) ;; summer time
        (setq logview-additional-timestamp-formats
              '(("Larnags Timestamp" (java-pattern . "yyyy-MM-dd HH:mm:ss.SSS+0200"))))
      (setq logview-additional-timestamp-formats
            '(("Larnags Timestamp" (java-pattern . "yyyy-MM-dd HH:mm:ss.SSS+0100"))))))
  (setq logview-additional-submodes
        '(("Larangs" (format . "TIMESTAMP [THREAD] [LEVEL] NAME -") (levels . "SLF4J")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'bluecare)
