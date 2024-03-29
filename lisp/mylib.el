(defun my/get-hostname ()
  "The hostname of this machine."
  (s-trim (shell-command-to-string "hostname")))

(defvar my/hostname (my/get-hostname))

;;;###autoload
(defun my/host-p (name)
  "Return whether NAME is the current hostname."
  (string-equal name my/hostname))

;;;###autoload
(defun my/host-starts-with-p (prefix)
  (string-prefix-p prefix my/hostname))


(defun my/join-line ()
  (interactive)
  (join-line -1))

;;; see http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs#570049
(defun my/pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; see http://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode#23382008
(defun display-ansi-colors ()
  (interactive)
  (unless (fboundp 'ansi-color-apply-on-region)
    (require 'ansi-color))
  (let ((inhibit-read-only t))
    (when (yes-or-no-p "Really? It will modify the buffer!")
      (ansi-color-apply-on-region (point-min) (point-max)))))


(defun my/xml-escape (beg end)
  (interactive (list (region-beginning) (region-end)))
  (let ((endm (make-marker)))
    (set-marker endm end)
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "\\(<\\|>\\|&\\)" (marker-position endm) t)
        (goto-char (1- (point)))
        (let ((c (buffer-substring-no-properties (point) (1+ (point)))))
          (delete-char 1)
          (cond
           ((s-equals-p "<" c) (insert "&lt;"))
           ((s-equals-p ">" c) (insert "&gt;"))
           ((s-equals-p "&" c) (insert "&amp;"))
           ((s-equals-p "\"" c) (insert "&quot"))
           ((s-equals-p "'" c) (insert "&apos;"))))))
    (set-marker endm nil)))



;;; thread dump navigation
(defun my/td-find-forward ()
  "Find next thread dump in file"
  (interactive)
  (search-forward "Full thread dump Java HotSpot"))

(defun my/td-find-backward ()
  "find previous thread dump in file"
  (interactive)
  (search-backward "Full thread dump Java HotSpot"))

(defun my/td-next-thread ()
  (interactive)
  (search-forward-regexp "^\".*?\"")
  (recenter-top-bottom 'middle))

(defun my/td-search-tid (tid)
  (interactive "MTid: ")
  (swiper (format "%x" (string-to-number tid))))

(defun my/td-count-threads ()
  (let ((c 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^\".*?\"" nil t)
        (setq c (1+ c)))
      c)))

(defun my/td-threads-by-state ()
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (search-forward "java.lang.Thread.State: " nil t)
        (let* ((name (s-trim (buffer-substring-no-properties
                              (point)
                              (point-at-eol))))
               (cell (assoc name result)))
          (if cell
              (incf (cdr cell))
            (setq result (cons (cons name 1) result)))
          ))
      (setq result
            (cons (cons "All"
                        (-reduce '+ (-map 'cdr result)))
                  result))
      result)))

(defun my/td-make-threads-report (in-buffer out-buffer)
  (with-current-buffer in-buffer
    (let ((table (-map (lambda (cell)
                         (list (car cell) (cdr cell)))
                       (my/td-threads-by-state)))
          (nthreads (my/td-count-threads))
          (buf (get-buffer-create out-buffer)))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "* Thread count" "\n\n")
          (insert "Enthält Threads ohne State Informationen (z.B. GC)\n\n")
          (insert "Thread# "(number-to-string nthreads) "\n\n")
          (insert "* Threads by State" "\n\n")
          (insert (orgtbl-to-orgtbl table nil) "\n")
          (let ((kmap (make-sparse-keymap)))
            (define-key kmap (kbd "q") 'bury-buffer)
            (use-local-map kmap)))))))

(defun my/td-show-threads-report ()
  (interactive)
  (let ((buf (get-buffer-create "*thread-report*")))
    (my/td-make-threads-report (current-buffer) buf)
    (pop-to-buffer buf nil t)))

(defun my/duration-to-minutes (duration)
  "Converts a duration string in format `mm:ss' or `hh:mm:ss'
into minutes."
  (let ((pairs (-zip-fill 0 (-map 'string-to-number
                                  (reverse (s-split ":" duration)))
                          `(,(/ 1.0 60) 1 60))))
    (-sum (-map (lambda (p) (* (car p) (cdr p))) pairs))))


(defun my/duration-add1 (a b)
  "Add duration strings like `1:33' and `2:45' that are in
`minute:second' or `hour:minute:second' format."
  (let* ((parts (reverse (-zip-fill "0"
                                    (reverse (s-split ":" a t))
                                    (reverse (s-split ":" b t)))))
         (sums (-map (lambda (pair)
                       (+ (string-to-number (car pair))
                          (string-to-number (cdr pair))))
                     parts))
         (result (-non-nil (-reduce-r-from
                            (lambda (n l)
                              (if (null (car l))
                                  (cons n l)
                                (let ((last (car l)))
                                  (if (>= last 60)
                                      (list (+ n (/ last 60)) (% last 60) (cdr l))
                                    (cons n l)))))
                            nil
                            sums))))
    (cond
     ((= 1 (length result)) (format "%s" (car result)))
     ((= 2 (length result)) (format "%s:%02d" (car result) (second result)))
     ((= 3 (length result)) (format "%s:%02d:%02d" (car result) (second result) (third result))))))

(defun my/duration-add (&rest a)
  "Add duration strings like `1:33' and `2:45' (in min:sec or
hour:min:sec format)."
  (if (null a)
      nil
    (-reduce 'my/duration-add1 a)))

(defun my/duration-times (n duration)
  (if (<= n 1)
      duration
    (apply 'my/duration-add (-repeat n duration))))


(defun my/swim-pace (duration meters)
  "Calculate time per 100m given some duration string in
`min:sec' format and meters."
  (let ((seconds
         (-reduce-from
          (lambda (secs pair)
            (+ secs (* (car pair)
                       (string-to-number (cdr pair)))))
          0
          (-zip-fill "0" (list 1 60 3600) (reverse (s-split ":" duration))))))
    (my/duration-add "0:0" (format "0:%d"
                                   (/ (* 100 seconds) meters)))))

(defun my/swim-stroke-rate (duration strokes)
  "Calculate stroke rate give a duration string in `mm:ss' or
`hh:mm:ss' and the number of strokes."
  (/ strokes (my/duration-to-minutes duration)))

(defun my/swim-stroke-length (meter strokes)
  (/ (float meter) (float strokes)))

(defun my/gen-uuid ()
  "Generate random uuid and put it in the kill ring."
  (interactive)
  (kill-new (s-trim (shell-command-to-string "uuidgen"))))

(defun my/random-string (len)
  "Generates a random string of LEN bytes length."
  (let ((cmd (format "head --bytes %s /dev/urandom | base64 -w0" len)))
    (s-replace-all '(("/" . "-") ("+" . "~")) (s-trim (shell-command-to-string cmd)))))

(defun my/kill-random-string (len)
  "Generates a random string of LEN bytes length and puts it in
the kill ring."
  (interactive "nBytes: ")
  (kill-new (my/random-string len)))

(defun my/insert-random-string (len)
  "Generates a random string of LEN bytes length and inserts it
at point."
  (interactive "nBytes: ")
  (insert (my/random-string len)))

(defun my/surround (value start end)
  (goto-char end)
  (insert value)
  (goto-char start)
  (insert value)
  (goto-char (+ 2 end)))

(defun my/surround-region (value)
  (interactive "MValue: ")
  (when (region-active-p)
    (my/surround value (region-beginning) (region-end))))

(defun my/replace-region (start end func)
  "Run a function over the region and replace the content with
  the result (current buffer)."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun my/urlencode-region (start end)
  (interactive "r")
  (my/replace-region start end 'url-hexify-string))

(defun my/urldecode-region (start end)
  (interactive "r")
  (my/replace-region start end 'url-unhex-string))

(defun my/get-screen-sizes ()
  "Return the size of all connected screens in mm in a list of
  cons cells where car is x and cdr is y."
  (let* ((cells (shell-command-to-string
                 "xrandr |grep -w connected | grep mm | awk '{print \"( \" $(NF - 2) \" . \" $NF \")\"}' | sed 's/m//g'"))
         (plist (concat "(" cells ")")))
    (read plist)))

(defun my/get-screen-size ()
  "Return the size of the largest connected screen in mm in a
  cons cell where car is x and cdr is y."
  (let ((sizes (my/get-screen-sizes)))
    (-max-by (lambda (a b)
               (> (* (car a) (cdr a))
                  (* (car b) (cdr b))))
             sizes)))

(defun my/get-screen-size-inch ()
  (let* ((sz (my/get-screen-size))
         (x (/ (car sz) 10.0 2.54))
         (y (/ (cdr sz) 10.0 2.54)))
    `(,x . ,y)))

(defun my/get-screen-width ()
  "Return the diagonal of the connected screen in inches."
  (let* ((c (my/get-screen-size-inch))
         (x (car c))
         (y (cdr c)))
    (sqrt (+ (* x x) (* y y)))))

(defun my/get-screen-resolution ()
  "Return the screen resolution in dots as a cons cell where car
  is x and cdr is y."
  (read (shell-command-to-string "xrandr | grep -w connected|tail -n1|awk '{print $3}' | awk -F'[x+]' '{print \"(\" $1 \" . \" $2 \")\"}'")))

(defun my/get-optimal-dpi ()
  (let ((size (my/get-screen-size-inch))
        (reso (my/get-screen-resolution)))
    `(,(/ (car reso) (car size)) ,(/ (cdr reso) (cdr size)))))

(defun my/set-dpi (dpi)
  (let ((cmd (format "xrandr --dpi %s ; echo 'Xft.dpi: %s' | xrdb -merge" dpi dpi)))
    (shell-command-to-string cmd)))

(defun my/set-optimal-dpi ()
  (let ((dpi (floor (car (my/get-optimal-dpi)))))
    (my/set-dpi dpi)))

(defun my/eval-stumpwm (s)
  "Use stumpish to evaluate some sexp in stumpwm."
  (require 'stumpwm-mode)
  (message "%s"
           (with-temp-buffer
             (call-process stumpwm-shell-program nil (current-buffer) nil
                           "eval"
                           s)
             (delete-char -1)
             (buffer-string))))

(defun my/copy-to-terminal (start end)
  "Paste the current region into the terminal. Requires stumpwmrc
to contain the corresponding functions."
  (interactive "r")
  (copy-region-as-kill start end)
  (my/eval-stumpwm "(progn (in-package :stumpwm) (ror-terminal) (keys-to-terminal '(\"C-a\" \"C-y\")))"))

(defun my/change-number-at-point (change)
  (let ((n (thing-at-point 'number))
        (bounds (bounds-of-thing-at-point 'word)))
    (when (and n bounds)
      (delete-region (car bounds) (cdr bounds))
      (insert (number-to-string (funcall change n)))
      (message "%s" n))))

(defun my/increment-number-at-point (arg)
  (interactive "p")
  (my/change-number-at-point
   (lambda (n)
     (+ (or arg 1) n))))

(defun my/decrement-number-at-point (arg)
  (interactive "p")
  (my/change-number-at-point
   (lambda (n)
     (- n (or arg 1)))))

(defun my/visit-now ()
  (interactive)
  (find-file "~/org/now.org"))

(defun my/org-duration-from-minutes (num)
  "Format to duration string, retain a possible negative sign in NUM."
  (let* ((anum (abs num))
         (str (org-duration-from-minutes anum)))
    (if (< num 0)
        (format "-%s" str)
      str)))
(defun my/org-duration-string-to-minutes (str)
  "Convert duration string STR to minutes supporting negative values."
  (if (s-starts-with-p "-" str)
      (* -1 (org-duration-string-to-minutes (substring str 1)))
    (org-duration-string-to-minutes str)))

(defun my/find-last-diff-minutes (file &optional result-name col-name)
  "Return column COL-NAME in the result table of RESULT-NAME in FILE.

It is expected to be a 2-row table."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char 1)
    (org-babel-goto-named-result (or result-name (or col-name "week-total")))
    (forward-line 1)
    (let* ((table (org-table-to-lisp))
           (colidx (-find-index (lambda (name) (string-equal name "Running")) (-first-item table)))
           (entry (-last-item (-select-column colidx table))))
      (my/org-duration-string-to-minutes entry))))

(defun my/org-clock-table-timediff (clocktable goal &optional rowname lastfile)
  "Get a time from CLOCKTABLE using ROWNAME and substract GOAL from it.

If LASTFILE is present it uses it to find the last diff for
calculating the running diff."
  (let* ((entry  (-find
                  (lambda(sl)
                    (string-equal (-first-item sl) (or rowname "Timesheet")))
                  clocktable))
         (total (string-trim (-second-item entry)))
         (goald (org-duration-string-to-minutes goal))
         (totald (org-duration-string-to-minutes total))
         (diff (- totald goald))
         (last-diff   (if lastfile
                              (my/find-last-diff-minutes lastfile)
                            0))
         (running-diff (+ diff last-diff)))
    `(:total ,total
             :goal ,goal
             :diff ,(my/org-duration-from-minutes diff)
             :last-diff ,(my/org-duration-from-minutes last-diff)
             :running-diff ,(my/org-duration-from-minutes running-diff))))

(defun my/previous-file ()
  "Using current buffer filename, gets the file before this using alphanum. ordering."
  (let* ((thisname (buffer-file-name))
         (dir (f-parent thisname))
         (found)
         (all (sort (f-entries dir) 'string<)))
    (-last-item (-take-while (lambda (el)
                               (not (string-equal thisname el))) all))))

(provide 'mylib)
;;; mylib ends here
