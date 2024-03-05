;;; solr.el -- Some simple helpers to work with solr
;;; Commentary:
;;; Code:

(require 'url)
(require 'url-http)
(require 'thingatpt)
(require 'json-ts-mode)

(defvar solr/base-url "http://localhost:8983")
(defvar solr/core-name "test")
(defvar solr/query-fl "* score,[child]")
(defvar solr/commit-within 500)

(defvar solr/--http-ok "HTTP/1.1 200 OK")
(defvar solr/--result-buffer-name "*SOLR Result*")

(defun solr/update-documents (&optional data)
  "Send DATA or the current buffer to solr's update endpoint.
You need to wrap documents into a json array. The buffer as send
as-is."
  (interactive)
  (let* ((documents (or data (string-clean-whitespace (buffer-string))))
         (url (url-encode-url (format "%s/solr/%s/update?wt=json&overwrite=true&commitWithin=%d"
                                      solr/base-url solr/core-name solr/commit-within)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data documents)
         (result (url-retrieve-synchronously url nil t 30)))
    (save-excursion
      (with-current-buffer result
        (goto-char (point-min))
        (if (thing-at-point-looking-at solr/--http-ok)
            (message "Solr update successful.")
          (pop-to-buffer result))))))

(defun solr/delete-documents (&optional q)
  "Delete documents via Q or current buffer.
Q is just a solr query, it is wrapped into a command to delete
documents."
  (interactive)
  (let* ((query (or q (string-clean-whitespace (buffer-string))))
         (cmd (format "{'delete': {'query':'%s'}}" query)))
    (solr/update-documents cmd)))

(defun solr/query-string ()
  "Query solr for documents using buffer as query string."
  (interactive)
  (let* ((query (string-clean-whitespace (buffer-string)))
         (url (url-encode-url
               (format "%s/solr/%s/select?wt=json&indent=true&fl=%s&q=%s"
                       solr/base-url solr/core-name solr/query-fl query)))
         (url-request-method "GET")
         (result-buffer (url-retrieve-synchronously url nil t 30)))
    (with-current-buffer result-buffer
      (goto-char (point-min))
      ;; remove header, only show results. only if 200
      (when (thing-at-point-looking-at solr/--http-ok)
        (while (not (string-equal "\n" (thing-at-point 'line)))
          (delete-line))
        (delete-line)))
    (solr/--copy-to-result-buffer result-buffer)
    (save-excursion
      (delete-other-windows)
      (split-window-vertically)
      (pop-to-buffer (solr/--make-result-buffer)))))

(defun solr/query-json (&optional data)
  "Query solr for documents using DATA or buffer as json."
  (interactive)
  (let* ((query (or data (string-clean-whitespace (buffer-string))))
         (url (url-encode-url
               (format "%s/solr/%s/query?wt=json&indent=true"
                       solr/base-url solr/core-name)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data query)
         (result-buffer (url-retrieve-synchronously url nil t 30)))
    (with-current-buffer result-buffer
      (goto-char (point-min))
      ;; remove header, only show results. only if 200
      (when (thing-at-point-looking-at solr/--http-ok)
        (while (not (string-equal "\n" (thing-at-point 'line)))
          (delete-line))
        (delete-line)))
    (solr/--copy-to-result-buffer result-buffer)
    (save-excursion
      (delete-other-windows)
      (split-window-vertically)
      (pop-to-buffer (solr/--make-result-buffer)))))

(defun solr/--make-result-buffer ()
  "Get or create the result buffer."
  (let ((b (get-buffer-create solr/--result-buffer-name)))
    (with-current-buffer b
      (unless (eq major-mode 'json-ts-mode)
        (json-ts-mode)
        (define-key json-ts-mode-map (kbd "C-c C-q")
                    (lambda ()
                      (interactive)
                      (bury-buffer)
                      (delete-window)))))
    b))

(defun solr/--copy-to-result-buffer (response)
  "Copy contents from the buffer RESPONSE to the result buffer."
  (with-current-buffer (solr/--make-result-buffer)
    (erase-buffer)
    (insert-buffer-substring response)
    (goto-char (point-min))))

(defun solr/--paragraph-at-point ()
  "Return begin and end position of paragraph at point."
  (let* ((curr-point (point))
         (curly-close (search-forward-regexp "^\\]\\|^}"))
         (curly-open (and curly-close (search-backward-regexp "^\\[\\|^{")))
         (result (when curly-open `(:start ,curly-open :end ,curly-close))))
    (when curly-open
      (goto-char curr-point)
      result)))

(defun solr/--request-data-at-point ()
  "Return request data at point."
  (let* ((par (solr/--paragraph-at-point))
         (beg (plist-get par :start))
         (end (plist-get par :end))
         (str (buffer-substring-no-properties beg end))
         (type (if (string-prefix-p "[" str) :update :query))
         (comment-start-regexp
          (rx line-start (* white) (literal (or comment-start "//"))))
         (cnt (seq-filter
               (lambda (line) (not (string-match-p comment-start-regexp line)))
               (string-lines str t))))
    (message "SOLR request: %s" cnt)
    `(:type ,type :content ,(string-join cnt "\n"))))

(defun solr/execute-at-point ()
  "Run query or update of block at point."
  (interactive)
  (let* ((request (solr/--request-data-at-point))
         (type (plist-get request :type)))
    (cond ((eq type :update)
           (solr/update-documents (plist-get request :content)))
          ((eq type :query)
           (solr/query-json (plist-get request :content))))))

(define-minor-mode solr-minor-mode
  "Toggle solr minor mode."
  :init-value nil
  :lighter " SOLR"
  :keymap
  `((,(kbd "C-c C-d") . solr/delete-documents)
    (,(kbd "C-c C-c") . solr/execute-at-point)))

(define-derived-mode solr-client-mode
  json-ts-mode "SOLR client"
  "Major mode for SOLR client."
  (define-key solr-client-mode-map (kbd "C-c C-c") 'solr/execute-at-point)
  )

(provide 'solr)
;;; solr.el ends here
