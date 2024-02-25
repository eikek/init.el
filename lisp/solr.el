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

(defun solr/update-documents (&optional q)
  "Send Q or the current buffer to solr's update endpoint.
You need to wrap documents into a json array. The buffer as send
as-is."
  (interactive)
  (let* ((query (or q (string-clean-whitespace (buffer-string))))
         (url (url-encode-url (format "%s/solr/%s/update?wt=json&overwrite=true&commitWithin=%d"
                                      solr/base-url solr/core-name solr/commit-within)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data query)
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

(defun solr/query ()
  "Query solr for documents."
  (interactive)
  (let* ((query (string-clean-whitespace (buffer-string)))
         (url (url-encode-url
               (format "%s/solr/%s/select?wt=json&indent=true&fl=%s&q=%s"
                       solr/base-url solr/core-name solr/query-fl query)))
         (url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
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


(define-minor-mode solr-minor-mode
  "Toggle solr minor mode."
  :init-value nil
  :lighter " SOLR"
  :keymap
  `((,(kbd "C-c C-u") . solr/update-documents)
    (,(kbd "C-c C-d") . solr/delete-documents)
    (,(kbd "C-c C-c") . solr/query)))

(provide 'solr)
;;; solr.el ends here
