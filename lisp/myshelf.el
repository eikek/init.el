;;; myshelf.el ---

;; * Introduction

;; This is my simple approach to have a shelf for files and keep a
;; little track of them. For example, bills, certificates etc --
;; things you like to keep, but you don't need often (if at
;; all). Therefore the focus is on adding them quickly and finding
;; them just quick enough.

;; This is supposed to be used in combination with org mode. It only
;; defines a helper function that takes a local or remote file and
;; copies it in your defied /shelf directory/. It then collects some
;; properties about it and inserts them -- together with a link to
;; that file -- in the current buffer. You can decide whether the
;; filename should be taken from the original file or if it should be
;; derived from the current heading.

;; So the idea is that you create a headline and "insert a file"
;; there. The file itself is put in a directory, but its properties
;; are added to the current headline. Thus, there is a always just one
;; file per headline.

;; Then there is a function that takes a local or remote file (it is
;; downloaded if remote) and checks whether it is in your shelf. All
;; other things are supposed to be delegated to org, by searching for
;; the file properties, for example.

(require 'f)

(defvar myshelf-directory nil
  "The only required setting is the /shelf directory/. It is not
  set by default.")

(defvar myshelf-derive-file-name-p nil
  "It is possible to use a filename derived from the current
heading. This may be useful for tools like =find=. I'm not sure
what a good default value is, for now it is set to ~nil~, so the
original file name is preserved.")

(defvar myshelf-insert-file-functions nil
  "If a file is inserted, hooks are invoked to allow further things.
For example, I usually like to =git add= the file. The functions are
applied to two arguments: the filename (full path into the shelf dir)
and the list of its properties (=0= based):

1. sha1
2. full file path
3. simple file name (base + ext)
4. size in bytes
5. size human readable")


;; Implementation

(defun myshelf-file-name-append-number (arg)
  "Appends a number to the given filename or adds 1 to an existing number."
  (let* ((base (file-name-sans-extension arg))
         (ext  (file-name-extension arg))
         (mr   (string-match "\\(.*\\)-\\([0-9]+\\)$" base))
         (name (match-string 1 base))
         (n    (match-string 2 base)))
    (if name
        (concat name "-" (int-to-string (+ (string-to-int n) 1)) "." ext)
      (concat base "-1." ext))))

(defun myshelf-file-name-make-path (fn &optional n)
  "Creates the full path to the file into the shelf directory for a given file name"
  (let* ((count (or n 0))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m"))
         (fpath (f-expand (f-join myshelf-directory year month fn))))
    (cond ((not (file-exists-p fpath))
           fpath)
          ((< count 100)
           (myshelf-file-name-make-path (myshelf-file-name-append-number fn) (+ count 1)))
          (error "file '%s' name already taken a few times" name))))

  (defun myshelf-file-name-from-heading ()
    "Uses the current heading to create a filename by removing weird chars and whitespace."
    (if orgstruct-mode
        (let ((hname (nth 4 (org-heading-components))))
          (replace-regexp-in-string "[^a-zA-Z0-9_\\-]|\s+" "-" hname))
      (error "No heading available. Are you looking at an org file?")))

  (defun myshelf-file-name-make-target (source)
    "Creates the target filename for the given source file"
    (let* ((name (file-name-nondirectory source))
           (target (if myshelf-derive-file-name-p
                       (myshelf-file-name-from-heading)
                     name))
           (fpath (myshelf-file-name-make-path target))
           (dirs (file-name-directory fpath)))
      (make-directory dirs t)
      fpath))


;; *** File Properties

;; When a file has been copied to the shelf, some properties should be
;; extracted.

(defun myshelf-file-checksum (file)
  "Asks a shell command to create a checksum of ~file~."
  (let* ((output (shell-command-to-string (concat "sha1sum " file)))
         (hash (substring output 0 40)))
    hash))

(defun myshelf-file-get-properties (file)
  "Retrieve file properties in a list:
  0. sha1
  1. full file path
  2. simple file name (base + ext)
  3. size in bytes
  4. size human readable"
  (let* ((hash (myshelf-file-checksum file))
         (attributes (file-attributes file))
         (basename (concat (file-name-base file) "." (file-name-extension file)))
         (sizebytes (nth 7 attributes))
         (sizehr (file-size-human-readable sizebytes)))
    (list hash file basename sizebytes sizehr)))

(defun myshelf-insert-file-properties (props)
  "Inserts properties of the given file at point as org properties."
  (let* ((hash (nth 0 props))
         (basename (nth 2 props))
         (file (nth 1 props))
         (sizehr (nth 4 props))
         (timestamp (format-time-string "%Y-%m-%d %a")))
    (org-set-property "file_insertion" (concat "[" timestamp "]")) ;
    (org-set-property "file_location" (concat "[[" file "]]"))
    (org-set-property "file_name" basename)
    (org-set-property "file_size" sizehr)
    (org-set-property "file_sha1" hash)))

;; *** table of contents for fast lookup

;; A file is added to a toc file in the shelf directory. This is for
;; quick lookups via checksums. I've read that serializing hash tables is
;; available since Emacs 23. Evaluating ~(featurep
;; 'hashtable-print-readable)~ should give =t=.

(defun myshelf-toc-file ()
  (concat myshelf-directory "/toc"))

(defun myshelf-toc-clear ()
  "Clears the toc map and the toc file."
  (clrhash myshelf-toc)
  (myshelf-toc-write-file))

(defun myshelf-toc-read-file ()
  "Reads the toc file into memory."
  ;; maybe avoid holding it in memory?
  (if (file-exists-p (myshelf-toc-file))
      (with-temp-buffer
        (insert-file-contents (myshelf-toc-file))
        (read (current-buffer)))
    (make-hash-table :test 'equal)))

(setq myshelf-toc (myshelf-toc-read-file))

(defun myshelf-toc-write-file ()
  "Writes the toc into a file"
  (with-temp-file (myshelf-toc-file)
    (prin1 myshelf-toc (current-buffer))))

(defun myshelf-toc-add-file (file &optional writetoc)
  "Adds the file to the shelf toc. If WRITETOC is non-nil, the
  toc file is written after update."
  (let ((hash (myshelf-file-checksum file)))
    (puthash hash (abbreviate-file-name file) myshelf-toc)
    (if writetoc
        (myshelf-toc-write-file))
    hash))

;; *** Directory traversal

;; Then I occassionally need to synchronise the toc file to an existing
;; directory. For this, the directory is traversed. The following code is
;; for that. When traversing a directory, only files are visited and
;; those not of use for myshelf are skipped (like hidden files or the toc
;; file).

(defun myshelf-traverse-apply-item (item &optional f)
  (let* ((name (nth 0 item))
         (attr (nth 9 item))
         (is-file (string-prefix-p "-" attr))
         (is-dir (string-prefix-p "d" attr)))
    (if (and is-file f)
        (funcall f name))
    (if is-dir
        (myshelf-traverse-dir name f))))

(defun myshelf-traverse-name-filter-p (item)
  "A predicate for filtering files to traverse. It excludes
  hidden files (on linux)."
  (let* ((fullname (nth 0 item))
         (basename (file-name-base fullname)))
    (not (or
          ;; TODO use file attributes to filter out hidden files
          (string-prefix-p "." basename)
          (file-equal-p fullname (myshelf-toc-file))
          (equal basename "..")))))

(defun myshelf-traverse-dir (dir f)
  "Traverses the directory `dir' recursively and applies the
  function `f' to each regular file. The argument passed to `f' is
  the full path to the file."
  (let ((listing (directory-files-and-attributes dir t)))
    (dolist (item
             (delete-if-not 'myshelf-traverse-name-filter-p listing))
      (myshelf-traverse-apply-item item f))))

;; ** Api
;; These functions make up the public api.

(defun myshelf-insert-file (arg)
  "Asks for a local file and copies it into the shelf
  directory. File properties are extracted and inserted at current
  point.

  If the file already exists, it is not added anew, but the
  properties of the existing file are inserted."
  (interactive "fFile: ")
  (let* ((target (myshelf-file-name-make-target arg))
         (old (myshelf-find-file arg))
         (file (or old target)))
    (if (not old)
        (progn
          (copy-file arg target)
          (myshelf-toc-add-file target t)))
    (if (or (not old) (y-or-n-p "The file is already shelfed. Insert its properties?"))
        (let ((props (myshelf-file-get-properties file)))
          (myshelf-insert-file-properties props)
          (if (not old)
              (run-hook-with-args 'myshelf-insert-file-functions file props))
          (message "File inserted at %s" file))
      (message "Aborted."))))

(defun myshelf-find-file (file)
  "Checks the given file, if it exists in the shelf. Returns the
  path to the file or nil."
  (let ((hash (myshelf-file-checksum file)))
    (gethash hash myshelf-toc)))

(defun myshelf-file-exists (file)
  "Checks whether the given file exists in the shelf by comparing
  checksums."
  (interactive "fFile: ")
  (let ((target (myshelf-find-file file)))
    (if (and target (file-exists-p target))
        (message "%s is shelfed" target)
      (message "File is not in the shelf."))))

(defun myshelf-toc-synchronise ()
  "Goes through the shelf directory and builds up a new toc file."
  (interactive)
  (myshelf-toc-clear)
  (myshelf-traverse-dir myshelf-directory
                        (lambda (file)
                          (message "Processing %s ..." file)
                          (myshelf-toc-add-file file)))
  (myshelf-toc-write-file)
  (message "Shelf toc file synchronised"))

(provide 'myshelf)
