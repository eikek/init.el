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

(defun solr/update-documents (&optional data inhibit-result-window)
  "Send DATA or the current buffer to solr's update endpoint.
You need to wrap documents into a json array. The buffer as send
as-is. If INHIBIT-RESULT-WINDOW is non-nil, the response is
returned as string, otherwise it is presented in a window."
  (interactive)
  (let* ((documents (or data (string-clean-whitespace (buffer-string))))
         (url (url-encode-url (format "%s/solr/%s/update?wt=json&versions=true&overwrite=true&commitWithin=%d"
                                      solr/base-url solr/core-name solr/commit-within)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data documents)
         (result (url-retrieve-synchronously url nil t 30)))
    (if inhibit-result-window
        (with-current-buffer result
          (buffer-substring-no-properties (point-min) (point-max)))
      (solr/--show-result result))))

(defun solr/delete-documents (&optional q inhibit-result-window)
  "Delete documents via Q or current buffer.
Q is just a solr query, it is wrapped into a command to delete
documents. With INHIBIT-RESULT-WINDOW non-nil, the result is
returned as a string and not shown in a window."
  (interactive)
  (let* ((query (or q (format "{'query': '%s'}" (string-clean-whitespace (buffer-string)))))
         (cmd (format "{'delete': %s}" query)))
    (solr/update-documents cmd inhibit-result-window)))

(defun solr/delete-document-by-id (id &optional inhibit-result-window)
  "Delete documents by ID or current buffer.
The ID will be wrapped into a delete command send to the update
endpoint. With INHIBIT-RESULT-WINDOW non-nil, the result is
returned as a string and not shown in a window."
  (interactive "MId: ")
  (let* ((cmd (format "{'delete': {'id':'%s'}}" id)))
    (solr/update-documents cmd inhibit-result-window)))

(defun solr/query-string (&optional inhibit-result-window)
  "Query solr for documents using buffer as query string.
With INHIBIT-RESULT-WINDOW non-nil, the result is returned as a
string and not shown in a window."
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
    (if inhibit-result-window
        (with-current-buffer result-buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      (solr/--show-result result-buffer))))

(defun solr/query-json (&optional data inhibit-result-window)
  "Query solr for documents using DATA or buffer as json.

When INHIBIT-RESULT-WINDOW is non-nil, the result is returned
otherwise it is presented in a window."
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
    (if inhibit-result-window
        (with-current-buffer result-buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      (solr/--show-result result-buffer))))

(defun solr/--show-result (result-buffer)
  "Copies the contents from the url RESULT-BUFFER into a new buffer.
The new buffer is better prepared to present the result to the user."
  (let ((buf (solr/--make-result-buffer)))
    (solr/--copy-to-result-buffer result-buffer)
    (save-excursion
      (delete-other-windows)
      (split-window-horizontally)
      (pop-to-buffer buf))))

(defun solr/--make-result-buffer ()
  "Get or create the result buffer."
  (let ((b (get-buffer-create solr/--result-buffer-name)))
    (with-current-buffer b
      (unless (eq major-mode 'solr-client-result-mode)
        (solr-client-result-mode)
        (read-only-mode)))
    b))

(defun solr/--copy-to-result-buffer (response)
  "Copy contents from the buffer RESPONSE to the result buffer."
  (with-current-buffer (solr/--make-result-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-buffer-substring response)
      (goto-char (point-min)))))

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

(defun solr/execute-at-point (&optional inhibit-result-window)
  "Run query or update of block at point.
With INHIBIT-RESULT-WINDOW non-nil, the result is returned as a
string and not shown in a window."
  (interactive)
  (let* ((request (solr/--request-data-at-point))
         (type (plist-get request :type)))
    (cond ((eq type :update)
           (solr/update-documents (plist-get request :content) inhibit-result-window))
          ((eq type :query)
           (solr/query-json (plist-get request :content) inhibit-result-window)))))

(defun solr/delete-at-point (&optional inhibit-result-window)
  "Delete documents using the query at point.
With INHIBIT-RESULT-WINDOW non-nil, the result is returned as a
string and not shown in a window."
  (interactive)
  (let* ((request (solr/--request-data-at-point))
         (type (plist-get request :type)))
    (when (eq type :update)
      (user-error "Thing at point is not a query, but seems to be an update block"))
    (solr/delete-documents (plist-get request :content) inhibit-result-window)))


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
  (define-key solr-client-mode-map (kbd "C-c C-d") 'solr/delete-at-point))

(define-derived-mode solr-client-result-mode
  json-ts-mode "SOLR Results"
  "Major mode for SOLR results."
  (define-key solr-client-result-mode-map (kbd "q")
              (lambda ()
                (interactive)
                (bury-buffer)
                (delete-window))))

(provide 'solr)
;;; solr.el ends here
