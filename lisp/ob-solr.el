;;; ob-solr.el --- Babel Functions for solr        -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Code:

(require 'solr)

(defun org-babel-execute:solr-client (body params)
  "Execute BODY as solr request according to PARAMS."
  (let* ((pp (org-babel-process-params params))
         (solr/base-url (or (cdr (assq :solr-url pp)) "http://localhost:8983"))
         (solr/core-name (or (cdr (assq :solr-core pp)) "solr"))
         (action (or (cdr (assq :solr-action pp)) :query))
         (is-query (equal action "query"))
         (is-delete (equal action "delete")))
    (with-temp-buffer
      (solr-client-mode)
      (insert (org-babel-expand-body:generic body params))
      (goto-char (point-min))
      (cond (is-query (solr/query-json nil t))
            (is-delete (solr/delete-at-point t))
            (t (solr/execute-at-point t))))))

(provide 'ob-solr)
;;; ob-solr.el ends here
