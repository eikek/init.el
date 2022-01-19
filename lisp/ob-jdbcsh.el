;;; ob-jdbcsh.el --- support jdbcsh script
;;;
;;; Commentary:
;;;
;;; jdbcsh is a simple cli tool to run SQL queries against databases.
;;; It uses Java's JDBC interface and therefore supports multiple
;;; databases. This file adds support for orgmode source blocks.
;;;
;;; Code:

(require 'ob)
(require 'ob-sql)
(require 'org-table)

(defun org-babel-expand-body:jdbcsh (body params)
  "Bla BODY PARAMS."
  (org-babel-expand-body:sql body params))

(defun org-babel-edit-prep:jdbcsh (info)
  "Bla INFO."
  (org-babel-edit-prep:sql info))


(defun ob-jdbcsh-make-command (body params in-file out-file)
  "Make BODY PARAMS IN-FILE OUT-FILE."
  (let* ((cmdline (cdr (assq :cmdline params)))
         (dburl (org-babel-find-db-connection-param params :dburl))
         (dbuser (org-babel-find-db-connection-param params :dbuser))
         (dbpassword (org-babel-find-db-connection-param params :dbpassword))
         (colnames-p (not (equal "no" (cdr (assq :colnames params))))))
    (format "%s %s %s %s %s --out %s %s"
            (or cmdline "")
            (if colnames-p "--show-header" "")
            (if dburl (format "--jdbc-url '%s'" dburl) "")
            (if dbuser (format "--jdbc-user '%s'" dbuser) "")
            (if dbpassword (format "--jdbc-pass '%s'" dbpassword) "")
            (org-babel-process-file-name out-file)
            (org-babel-process-file-name in-file))))

(defun org-babel-execute:jdbcsh (body params)
  "Execute SQL BODY PARAMS."
  (let* ((result-params (cdr (assq :result-params params)))
         (colnames-p (not (equal "no" (cdr (assq :colnames params)))))
         (in-file (org-babel-temp-file "sql-in-"))
         (out-file (or (cdr (assq :out-file params))
                       (org-babel-temp-file "sql-out-")))
         (header-delim "")
         (command (ob-jdbcsh-make-command body params in-file out-file)))
    (with-temp-file in-file
      (insert
       (org-babel-expand-body:jdbcsh body params)))
    (org-babel-eval command "")

    (when colnames-p
      (with-temp-buffer
        (insert-file-contents out-file)
        (goto-char (point-min))
        (forward-line 1)
        (insert "-\n")
        (setq header-delim "-")
        (goto-char (point-min))
        (while (search-forward "|" nil t)
          (replace-match "¦"))
        (write-file out-file)))

    (with-temp-buffer
      (org-babel-result-cond result-params
        (with-temp-buffer
          (progn (insert-file-contents-literally out-file) (buffer-string)))
        (org-table-import out-file '(16))
        (org-babel-reassemble-table
         (mapcar (lambda (x)
                   (if (string= (car x) header-delim)
                       'hline
                     x))
                 (org-table-to-lisp))
         (org-babel-pick-name (cdr (assq :colname-names params))
                              (cdr (assq :colnames params)))
         (org-babel-pick-name (cdr (assq :rowname-names params))
                              (cdr (assq :rownames params))))))))

(defun org-babel-prep-session:jdbcsh (_session _params)
  "Raise an error because Sql sessions aren't implemented."
  (error "SQL sessions not yet implemented"))

(add-to-list 'org-src-lang-modes
             '("jdbcsh" . sql))

(provide 'ob-jdbcsh)

;;; ob-jdbcsh.el ends here
