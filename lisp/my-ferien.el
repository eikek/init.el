;;; my-ferien.el --- einfaches Berechnen von Ferien für ein Jahr

;; Beachtet Feiertage in Zürich

;;; usage:

;; Setze my-ferien/work-h-per-day und my-ferien/ferien-budget-days
;; mache eine Liste von Ferientagen

;;     (setq my-ferien/ferien
;;       '("01-05"
;;         "01-06"
;;         "03-17--03-20"
;;         "05-15"
;;         "08-01--08-15"
;;         "08-19--08-20"))

;; erlaubt ist das Format [month-day] oder [day.month]

;; erstelle die Ferien Tabelle in einer org Datei

;;  (my-ferien/saldo-table my-ferien/ferien 2015)


(defvar my-ferien/work-h-per-day (* 8.4 0.80)
  "Amount of hours for a full working day.")

(defvar my-ferien/ferien-budget-days 25
  "Amount of vacation days available this year. This is added to
  `my-ferien/ferien-budget-hours'.")

(defvar my-ferien/ferien-budget-hours 0
  "Amount of vacation hours available this year. This is added to
  `my-ferien/ferien-budget-days'.")

(defun my-ferien/ferien-hper-year (&optional offset-hours)
  (+ (or offset-hours 0)
     (* my-ferien/ferien-budget-days my-ferien/work-h-per-day)
     my-ferien/ferien-budget-hours))


(defun my-ferien/format-date (date &optional longp)
  (if longp
      (format "%s %4d-%02d-%02d"
              (calendar-day-name date t)
              (third date)
              (first date)
              (second date))
    (format "%02d-%02d" (car date) (cadr date))))

(defun my-ferien/string2date (str year)
  (if (string-match-p "[0-9]*\\.[0-9]*" str)
      `(,(string-to-int (cadr (s-split "\\." str t)))
        ,(string-to-int (car (s-split "\\." str t)))
        ,year)
    (if (string-match-p "[0-9]*-[0-9]*" str)
        `(,(string-to-int (car (s-split "-" str t)))
          ,(string-to-int (cadr (s-split "-" str t)))
          ,year)
      (error "Unknown date format: %s" str))))

(defun my-ferien/string->range (str year)
  (let* ((split (s-split "--" str t))
         (beg (first split))
         (end (if (cdr split)
                  (cadr split)
                beg)))
    (cons (my-ferien/string2date beg year)
          (my-ferien/string2date end year))))

(defun my-ferien/expand-range (range)
  (let ((beg (first range))
        (end (rest range))
        (result))
    ;; weird, the compare fn calls car on its arguments?!
    (while (or (equal beg end)
               (calendar-date-compare (list beg) (list end)))
      (setq result (cons beg result))
      (setq beg (my-ferien/next-date beg)))
    result))

(defun my-ferien/strings->range (lst year)
  (-mapcat 'my-ferien/expand-range
           (-map (lambda (str) (my-ferien/string->range str year)) lst)))

(defun my-ferien/format-date-str (date-str year)
  (my-ferien/format-date
   (my-ferien/string2date date-str year) t))

(defun my-ferien/format-range-str (range-str year)
  (let* ((range (my-ferien/string->range range-str year))
         (beg (first range))
         (end (rest range)))
    (if (equal beg end)
        (my-ferien/format-date beg t)
      (format "%s - %s"
              (my-ferien/format-date beg t)
              (my-ferien/format-date end t)))))

(defun my-ferien/easter-sunday (year)
  "See wikipedia"
  (let* ((a (mod year 19))
         (b (floor (/ year 100)))
         (c (mod year 100))
         (d (floor (/ b 4)))
         (e (mod b 4))
         (f (floor (/ (+ b 8) 25)))
         (g (floor (/ (1+ (- b f)) 3)))
         (h (mod (+ (* 19 a) b (* -1 d) (* -1 g) 15) 30))
         (i (floor (/ c 4)))
         (k (mod c 4))
         (L (mod (+ 32 (* 2 e) (* 2 i) (* -1 h) (* -1 k)) 7))
         (m (floor (/ (+ a (* 11 h) (* 22 L)) 451)))
         (mon (floor (/ (+ h L (* -7 m) 114) 31)))
         (day (1+ (mod (+ h L (* -7 m) 114) 31))))
    (list mon day year)))

(defun my-ferien/next-date (date &optional n)
  (let* ((y (third date))
         (m (first date))
         (d (second date))
         (lastday (calendar-last-day-of-month m y))
         (result
          (if (= d lastday)
              (list (1+ m) 1 y)
            (list m (1+ d) y))))
    (if (or (not n) (<= n 1))
        result
      (my-ferien/next-date result (1- n)))))

(defun my-ferien/prev-date (date &optional n)
  (let* ((y (third date))
         (m (first date))
         (d (second date))
         (result
          (if (= 1 d)
              (list (1- m) (calendar-last-day-of-month (1- m) y) y)
            (list m (1- d) y))))
    (if (or (not n) (<= n 1))
        result
      (my-ferien/prev-date result (1- n)))))

(setq my-ferien/fixed-holidays
      '((:date "01-01" :factor 0 :description "Neujahrstag")
        (:date "01-02" :factor 0 :description "Berchtholdstag")
        (:date "05-01" :factor 0 :description "Tag der Arbeit")
        (:date "08-01" :factor 0 :description "Nationaltag")
        (:date "12-24" :factor 0.5 :description "Heiligabend")
        (:date "12-25" :factor 0 :description "Weihnachtstag")
        (:date "12-26" :factor 0 :description "Stephanstag")))

(defun my-ferien/easter-related-holidays (year)
  (let* ((eastersun (my-ferien/easter-sunday year))
         (eastermon (my-ferien/next-date eastersun))
         (easterfri (my-ferien/prev-date eastersun 2))
         (pfingstmon (my-ferien/next-date eastermon 49))
         (auffahrt (my-ferien/next-date eastersun 39)))
    (list
     `(:date ,(my-ferien/format-date eastermon) :factor 0 :description "Ostermontag")
     `(:date ,(my-ferien/format-date easterfri) :factor 0 :description "Karfreitag")
     `(:date ,(my-ferien/format-date pfingstmon) :factor 0 :description "Pfingstmontag")
     `(:date ,(my-ferien/format-date auffahrt) :factor 0 :description "Auffahrt"))))

(defun my-ferien/special-holidays (year)
  "Knabenschiessen und Sechseleuten ist immer der zweite Montag
      im September bzw. April (ausser das wäre Ostern)."
  (let* ((mondays-april
          (-filter (lambda (date)
                     (= 1 (calendar-day-of-week date)))
                   (-iterate 'my-ferien/next-date (list 4 1 year) 29)))
         (mondays-sept
          (-filter (lambda (date)
                     (= 1 (calendar-day-of-week date)))
                   (-iterate 'my-ferien/next-date (list 9 1 year) 29)))
         (eastermon (my-ferien/next-date (my-ferien/easter-sunday year)))
         (sechse
          (if (equal eastermon (second mondays-april))
              (third mondays-april)
            (second mondays-april)))
         (knaben (second mondays-sept)))
    (list
     `(:date ,(my-ferien/format-date sechse) :factor 0.5 :description "Sechseleuten")
     `(:date ,(my-ferien/format-date knaben) :factor 0.5 :description "Knabenschiessen"))))

(defun my-ferien/all-holidays (year)
  (append
   my-ferien/fixed-holidays
   (my-ferien/easter-related-holidays year)
   (my-ferien/special-holidays year)))

(defun my-ferien/off-hours (date)
  (let* ((year (third date))
         (weekday (calendar-day-of-week date))
         (day (my-ferien/format-date date))
         (factor (cond
                  ((= weekday 0) 0)
                  ((= weekday 6) 0)
                  (t (or (cdr (assoc day (-map (lambda (pl)
                                                 (cons (plist-get pl :date)
                                                       (plist-get pl :factor)))
                                               (my-ferien/all-holidays 2015))))
                         1)))))
    (* factor my-ferien/work-h-per-day)))

(defun my-ferien/count-work-h (range-str year)
  (-sum (-map 'my-ferien/off-hours
              (my-ferien/expand-range
               (my-ferien/string->range range-str year)))))

(defun my-ferien/count-work-days (range-str year)
  (format "%.1f"
          (/
           (my-ferien/count-work-h range-str year)
           my-ferien/work-h-per-day)))


(defun my-ferien/strings->range (lst year)
  (-mapcat 'my-ferien/expand-range
           (-map (lambda (str) (my-ferien/string->range str year)) lst)))


  ;;; Jetzt müssen nur noch ein paar Differenzen gemacht werden.
(defun my-ferien/ferien-h-taken (lst year)
  (-sum
   (-map 'my-ferien/off-hours
         (my-ferien/strings->range lst year))))

(defun my-ferien/ferien-h-left (lst year)
  (- (my-ferien/ferien-hper-year)
     (my-ferien/ferien-h-taken lst year)))

;;;###autoload
(defun my-ferien/holiday-table (year)
  (append
   '(("Datum" "Beschreibung" "Gewicht") hline)
   (-map (lambda (pl)
           (list (my-ferien/format-date-str (plist-get pl :date) year)
                 (plist-get pl :description)
                 (plist-get pl :factor)))
         (my-ferien/all-holidays year))))

;;;###autoload
(defun my-ferien/saldo-table (lst year)
  (let* ((taken (my-ferien/ferien-h-taken lst year))
         (left (my-ferien/ferien-h-left lst year)))
    (append
     (list `("" "hours" "days") 'hline)
     (-map (lambda (range)
             (list
              (my-ferien/format-range-str range year)
              (format "%.2f" (my-ferien/count-work-h range year))
              (my-ferien/count-work-days range year)))
           lst)
     (list
      'hline
      `("Gesamt"
        ,(format "%.2f" (my-ferien/ferien-hper-year))
        ,(format "%.2f" (/ (my-ferien/ferien-hper-year)
                           my-ferien/work-h-per-day)))
      `("Eingetragen"
        ,(format "%.2f" taken)
        ,(format "%.2f" (/ taken my-ferien/work-h-per-day)))
      'hline
      `("Übrig"
        ,(format "%.2f" left)
        ,(format "%.2f" (/ left my-ferien/work-h-per-day)))))))


(provide 'my-ferien)
