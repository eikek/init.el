;;; ensime-ammonite --- use ensime with ammonite scripts

(require 's)
(require 'f)

(defvar enam-project-dir
  (expand-file-name "amm-ensime" user-emacs-directory))

(defvar enam-interpreters '("amm"))

(defvar enam-scala-version "2.11.8")

(defun enam-config-file ()
  (f-join enam-project-dir ensime-config-file-name))

(defun enam--first-line (file)
  ;; todo make it more efficient
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (search-forward "\n")
    (buffer-substring-no-properties 1 (1- (point)))))

(defun enam--find-interpreter (line)
  (-find (lambda (interp)
           (s-ends-with-p (concat "/usr/bin/env " interp) line))
         enam-interpreters))

(defun enam-script-file-p (source-file)
  "Return whether SOURCE-FILE is a scala script."
  (and (f-file-p source-file)
       (or (s-ends-with-p ".sc" source-file)
           (enam--find-interpreter (enam--first-line source-file)))))

(defun enam-find-connection ()
  (let ((project-root (file-truename enam-project-dir)))
    (-find (lambda (proc)
             (and (ensime-conn-if-alive proc)
                  (s-equals-p (plist-get (ensime-config proc) :root-dir)
                              project-root)))
           ensime-net-processes)))

;; (defun ensime-config-includes-source-file (conf file &optional no-ref-sources)
(defun enam-config-includes-file (orig-fn &rest args)
  (let ((res (apply orig-fn args))
        (conn (enam-find-connection)))
    (or res
        (and conn
             (enam-script-file-p (second args))))))

(advice-add 'ensime-config-includes-source-file
            :around #'enam-config-includes-file)

;; must wrap the script into an object to satisfy scala compiler
;; this is done by advicing the ensime functions and adding the
;; desired code.


;; (defun ensime-get-buffer-as-string ()
(defun enam-wrap-script-string (orig-fn &rest args)
  (let ((res (apply orig-fn args)))
    (format "object foo {  %s  \n }"
            (if (s-starts-with-p "#!" res)
                (substring res (s-index-of "\n" res))
              res))))

(advice-add 'ensime-get-buffer-as-string
            :around #'enam-wrap-script-string)

;; (defun ensime-src-info-with-contents-in-temp ()
(defun enam-wrap-scrip-tmp (orig-fn &rest args)
  (let*((res (apply orig-fn args))
        (file (plist-get res :contents-in)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (looking-at-p "#!")
        (search-forward "\n")
        (delete-region (point-min) (point)))
      (insert "object foo { ")
      (goto-char (point-max))
      (insert "\n}")
      (ensime-write-buffer file nil nil))
    res))

(advice-add 'ensime-src-info-with-contents-in-temp
            :around #'enam-wrap-scrip-tmp)

(defun enam-project-deps ()
  (let ((amm-version (shell-command-to-string "amm --code \"print(ammonite.Constants.version)\"")))
    `(,(format "\"com.lihaoyi\" %% \"ammonite_%s\" %% \"%s\"" enam-scala-version amm-version))))

(defun enam-create-project ()
  (when (f-exists-p enam-project-dir)
    (f-delete enam-project-dir t))
  (f-mkdir enam-project-dir "project")
  (with-temp-buffer
    (insert (mapconcat 'identity
                       `("name := \"amm-ensime\""
                         "version := \"1.0\""
                         ,(format "scalaVersion := \"%s\"" enam-scala-version)
                         "libraryDependencies ++= Seq("
                         ,(mapconcat 'identity (enam-project-deps) ",\n")
                         ")")
                       "\n"))
    (write-file (f-join enam-project-dir "build.sbt")))
  (with-temp-buffer
    (insert "sbt.version=0.13.12")
    (write-file (f-join enam-project-dir "project" "build.properties")))
  (with-temp-buffer
    (insert "addSbtPlugin(\"org.ensime\" % \"sbt-ensime\" % \"1.10.0\")\n"
            "addSbtPlugin(\"io.get-coursier\" % \"sbt-coursier\" % \"1.0.0-M14\")")
    (write-file (f-join enam-project-dir "project" "plugins.sbt"))))

(defun enam-generate-config ()
  (let ((default-directory (file-name-as-directory enam-project-dir)))
    (when (or (not (f-exists-p (enam-config-file)))
              ;; todo get rid of this call:
              (ensime--config-sbt-needs-refresh-p (f-join enam-project-dir ".ensime")))
      (when (not (f-exists-p enam-project-dir))
        (enam-create-project))
      (ensime-refresh-config))))


(defun ensime-ammonite ()
  (interactive)
  (enam-generate-config)
  (let ((default-directory (file-name-as-directory enam-project-dir)))
    (ensime)))


(provide 'ensime-ammonite)
