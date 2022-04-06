(defun simsched/parse-time-range-entry ()
  "Return the start time, end time, and subsequent text listed on a given line, if they exist."
  (let* (
         (time-regex "\\([[:digit:]][[:digit:]]?:[[:digit:]][[:digit:]]\\|[[:digit:]][[:digit:]]?\\)")
         (range-regex (concat time-regex "-" time-regex))
         (whole-range nil)
         (start-time nil)
         (end-time nil))
    (condition-case nil
        (progn
          (beginning-of-line)
          (re-search-forward range-regex (point-at-eol))
          (setq whole-range (split-string (match-string 0) "-"))
          (setq start-time (first whole-range))
          (setq end-time (second whole-range))

          ;; reset the point to be at the end of the time range to get the event name
          (beginning-of-line)
          (re-search-forward "-" (point-at-eol))
          (re-search-forward " " (point-at-eol))
          (list start-time end-time (buffer-substring-no-properties (match-end 0) (point-at-eol))))
      (error nil))))

(defun simsched/parse-schedule ()
  "Expand region above and below current line to discover
   all lines parseable as time ranges"
  (let ((begin (point-at-bol))
        (ranges-here-to-end '())
        (ranges-beginning-to-here '())
        (current-range (simsched/parse-time-range-entry)))
    (if (not current-range)
        (message "Point not inside a valid simple schedule.")
      (save-excursion
        (while current-range
          (push current-range ranges-here-to-end)
          (forward-line 1)
          (setq current-range (simsched/parse-time-range-entry)))

        (goto-char begin)
        (forward-line -1)
        (setq current-range (simsched/parse-time-range-entry))

        (while current-range
          (push current-range ranges-beginning-to-here)
          (forward-line -1)
          (setq current-range (simsched/parse-time-range-entry)))
        (append ranges-beginning-to-here (reverse ranges-here-to-end))))))

(load! "./unicode-enbox.el")
(load! "./ucs-utils.el")
(load! "./string-utils.el")
(load! "./list-utils.el")
(load! "./obarray-fns.el")

;; Assign synonymous times (1 and 1:00) with the same values, 1 representing A.M., the other P.M.
(setq simsched/normalize-time
      #s(hash-table
         size 24
         test equal
         data (
               "1:00"  (1 25)
               "1"     (1 25)
               "1:30"  (2 26)

               "2:00"  (3 27)
               "2"     (3 27)
               "2:30"  (4 28)

               "3:00"  (5 29)
               "3"     (5 29)
               "3:30"  (6 30)

               "4:00"  (7 31)
               "4"     (7 31)
               "4:30"  (8 32)

               "5:00"  (9 33)
               "5"     (9 33)
               "5:30"  (10 34)

               "6:00"  (11 35)
               "6"     (11 35)
               "6:30"  (12 36)

               "7:00"  (13 37)
               "7"     (13 37)
               "7:30"  (14 38)

               "8:00"  (15 39)
               "8"     (15 39)
               "8:30"  (16 40)

               "9:00"  (17 41)
               "9"     (17 41)
               "9:30"  (18 42)

               "10:00" (19 43)
               "10"    (19 43)
               "10:30" (20 44)

               "11:00" (21 45)
               "11"    (21 45)
               "11:30" (22 46)

               "12:00" (23 47)
               "12"    (23 47)
               "12:30" (24 48))))

;; When denormalizing times, always use the long form (1:00)
(setq simsched/denormalize-time
      #s(hash-table
         size 48
         test equal
         data (
               1  "1:00"
               25 "1:00"
               2  "1:30"
               26 "1:30"

               3  "2:00"
               27 "2:00"
               4  "2:30"
               28 "2:30"

               5  "3:00"
               29 "3:00"
               6  "3:30"
               30 "3:30"

               7  "4:00"
               31 "4:00"
               8  "4:30"
               32 "4:30"

               9  "5:00"
               33 "5:00"
               10  "5:30"
               34 "5:30"

               11  "6:00"
               35 "6:00"
               12  "6:30"
               36 "6:30"

               13  "7:00"
               37 "7:00"
               14  "7:30"
               38 "7:30"

               15  "8:00"
               39 "8:00"
               16  "8:30"
               40 "8:30"

               17  "9:00"
               41 "9:00"
               18  "9:30"
               42 "9:30"

               19  "10:00"
               43 "10:00"
               20  "10:30"
               44 "10:30"

               21  "11:00"
               45 "11:00"
               22  "11:30"
               46 "11:30"

               23  "12:00"
               47 "12:00"
               24  "12:30"
               48 "12:30")))

(defun simsched/normalize-schedule (schedule)
  (mapcar (lambda (time-range-entry)
            (list
             (gethash (first time-range-entry) simsched/normalize-time)
             (gethash (second time-range-entry) simsched/normalize-time)
             (third time-range-entry)))
          schedule))

(defcustom simsched/start-time "7:00"
  "First hour of the 12-hour schedule"
  :type 'string
  :options '("1:00" "2:00" "3:00" "4:00" "5:00" "6:00" "7:00" "8:00" "9:00" "10:00" "11:00" "12:00"))

(defun simsched/normalize-schedule-to-start-time (schedule)
  "Normalize all time ranges and return the proper times given the user's customized start time."
  (let ((custom-start (first (gethash simsched/start-time simsched/normalize-time)))
        (result (list))
        (am? t))
    (dolist (range schedule (reverse result))
      (let* ((normalized-start (gethash (first range)  simsched/normalize-time))
            (normalized-end   (gethash (second range) simsched/normalize-time))
            (am-start (first normalized-start))
            (pm-start (second normalized-start))
            (am-end (first normalized-end))
            (pm-end (second normalized-end))
            (should-start-am? (<= custom-start am-start))
            (should-still-be-am? (<= (if should-start-am? am-start pm-start) am-end))
            (normalized-range (list (third range))))
        (progn
          (if should-still-be-am? (push am-end normalized-range) (push pm-end normalized-range))
          (if should-start-am? (push am-start normalized-range) (push pm-start normalized-range))
          (push normalized-range result))))))

;; Decide our schedule should have max 8 columns.
;; Any more, and why do you have so many conflicts during the day?
(defvar simsched/MAX-COLUMNS 8)

(defun simsched/create-schedule-grid (schedule)
  (let ((grid (let ((grid_ (make-vector 48 nil)))
                (dotimes (i 48) (aset grid_ i (make-vector simsched/MAX-COLUMNS nil)))
                grid_)))
    (-each (simsched/normalize-schedule-to-start-time schedule) (lambda (range) (simsched/add-time-range-to-grid range grid)))
    grid))

(defun simsched/add-time-range-to-grid (range grid)
  "put range into the grid, inserting something to represent it"
  (let* ((start-time (first range))
         (end-time  (second range))
         (column (let ((look-at start-time)
                       (max-column 0))
                   (while (< look-at end-time)
                     (when (aref (aref grid look-at) max-column)
                       (progn (setq look-at start-time) (cl-incf max-column)))
                     (cl-incf look-at))
                   max-column)))
    (while (< start-time end-time)
      (aset (aref grid start-time) column (third range))
      (cl-incf start-time))))

(setq simsched/test-schedule '(("9" "9:30" "first")("10:00" "11:00" "second")("10:30" "11:30" "conflict")("11:30" "1" "another thing until afternoon")("2:00" "4" "one more in afternoon")))
(setq simsched/test-schedule2 '(("7" "9:30" "first")("1:30" "8" "another thing until afternoon")("6:00" "8" "one more in afternoon")))

(defcustom simsched/schedule-buffer-name "*simsched schedule*"
  "The default name of the buffer where the rendered schedule is displayed"
  :type 'string)

(defcustom simsched/switch-buffer-function
  #'pop-to-buffer
  "Function called to display the schedule buffer."
  :type 'function)

(defun simsched/get-create-schedule-buffer ()
  (get-buffer-create simsched/schedule-buffer-name))

(defun simsched/switch-to-schedule-buffer ()
  (interactive)
  (funcall simsched/switch-buffer-function (simsched/get-create-schedule-buffer)))

(defun simsched/render-time-labels ()
  "Place time labels in the simsched buffer.
   Assumes the buffer is empty."
  (with-current-buffer (simsched/get-create-schedule-buffer)
    (dotimes (n 48)
      (let* ((time-string (gethash (1+ n) simsched/denormalize-time))
             ;; hacky, better to align w/ regexp but I can't find appropriate elisp function
             (aligned-time-string (if (< (length time-string) 5) (concat " " time-string) time-string)))
        (insert aligned-time-string)
        (insert "\n")
        (insert "\n")))))

(defun simsched/range-span (schedule-grid row column)
  "Return the span (number of matching rows) that the item at row,column occupies.

   e.g. An item that lasts 30 minutes will have a span of 1
        An item that lasts 90 minutes will have a span of 3"
  (let ((task-at-time (aref (aref schedule-grid row) column))
        (bottom-row row)
        (top-row row))
    (while (and task-at-time (equal (aref (aref schedule-grid bottom-row) column) task-at-time))
      (setq bottom-row (1+ bottom-row)))
    (while (and task-at-time (equal (aref (aref schedule-grid top-row) column) task-at-time))
      (setq top-row (1- top-row)))
    (if (not task-at-time) 1
      (+ (- row top-row 1)
         (- bottom-row row)))))

(defun simsched/justify-entry (text max-width span)
  "Try to split long entries so that they take the minimum horizontal space required.
   max-width is a suggestion that `fill-region` will try to match without breaking words.
   span is the total number of lines that the entry should fill after justification.

   e.g. if max-width is 7 but the word is antidisestablishmentarianism, fill-region will not
        break that word to 7 character chunks."
  (with-temp-buffer
    (insert text)
    (setq fill-column max-width)
    (fill-region (point-min) (point-max))
    (let* ((lines (count-lines (point-min) (point-max)))
           (newlines-above (floor (/ (- span lines) 2.0)))
           (newlines-below (ceiling (/ (- span lines) 2.0))))
      (goto-char (point-min))
      (dotimes (_ newlines-above) (insert " \n"))
      (goto-char (point-max))
      (dotimes (_ newlines-below) (insert "\n "))
      (buffer-string))))

(defun simsched/insert-time-at-junction (string-to-insert)
  "When two time ranges share end/start time, a new special character must be used
   to concatenate the two schedule boxes."
    (let ((line-replacement (replace-regexp-in-string "┘" "┤"
                                                      (replace-regexp-in-string "└" "├"
                                                                                (or (thing-at-point 'line t) "")))))
      (beginning-of-line)
      (unless (eobp) (kill-line))
      (insert line-replacement)
      (beginning-of-line)
      (insert-rectangle (cdr string-to-insert))))


(defun simsched/render-schedule (schedule-grid)
  "Display the given (normalized) schedule grid in the simsched schedule buffer"
  (let ((column 0))
    (with-current-buffer (simsched/get-create-schedule-buffer)
      (erase-buffer)
      (simsched/render-time-labels)
      (while (< column simsched/MAX-COLUMNS)
        (let* ((max-width (cl-reduce #'max
                                  (mapcar (lambda (n)
                                            (let* ((task-at-time (aref (aref schedule-grid n) column))
                                                   (length-of-entry (length task-at-time))
                                                   (span (simsched/range-span schedule-grid n column)))
                                              (/ length-of-entry (- (1+ (* span 2)) 2))))
                                          (number-sequence 0 47))))
               (enboxed-tasks (with-temp-buffer
                                (let ((i 0)
                                      (special-delimiter "s%!r!")) ; need a short special delimiter to not mess up the boxing width
                                  (while (< i 48)
                                    (let* ((span (simsched/range-span schedule-grid i column))
                                           (task-at-time (aref (aref schedule-grid i) column))
                                           (string-to-insert (if task-at-time (simsched/justify-entry task-at-time max-width (- (1+ (* span 2)) 2)) "\n")))
                                      (insert string-to-insert)
                                      (insert "\n")
                                      (insert special-delimiter)
                                      (insert "\n")
                                      (matt/pad-lines-to-max-length)
                                      (setq i (1+ i))))
                                  (mapcar (lambda (entry) (if (string-match "[^\s\n]" entry) (unicode-enbox entry nil 'append 'append)))
                                          (split-string (buffer-string) (concat "\n" special-delimiter "\s*" "\n"))))))
               (rendered-column (with-temp-buffer
                                  (let ((i 0))
                                    (dotimes (i 48) (insert "\n"))
                                    (while (< i 48)
                                      (let* ((span (simsched/range-span schedule-grid i column))
                                             (task-at-time (aref (aref schedule-grid i) column))
                                             (string-to-insert (if task-at-time (split-string (nth i enboxed-tasks) "\n") (list ""))))
                                        (goto-line (1- (* i 2)))
                                        (end-of-line)
                                        (if (and task-at-time (char-equal (or (char-before) ?a) ?┘))
                                            (simsched/insert-time-at-junction string-to-insert)
                                          (insert-rectangle string-to-insert))
                                        (setq i (+ i span))))
                                    (split-string (buffer-string) "\n")))))
          (goto-char (point-min))
          (end-of-line)
          (insert-rectangle rendered-column)
          (matt/pad-lines-to-max-length)
          (setq column (+ column 1))))
      (simsched/trim-schedule)))
  (call-interactively #'simsched/switch-to-schedule-buffer))


(setq simsched/test-grid (simsched/create-schedule-grid simsched/test-schedule))

(defun simsched/trim-schedule ()
  "Remove schedule lines that don't have any items rendered."
  (let ((beginning-found nil)
        (end-found nil))
    (goto-char (point-min))
    (while (not beginning-found)
      (if (not (string-match "\u250c" (or (thing-at-point 'line t) ""))) ; top-left corner char
          (kill-region (point-at-bol) (1+ (point-at-eol)))
        (setq beginning-found t)))
    (goto-char (point-max))
    (while (not end-found)
      (if (not (string-match "\u2514" (or (thing-at-point 'line t) ""))) ; bottom-left corner char
          (kill-region (1- (point-at-bol)) (point-at-eol))
        (setq end-found t)))))

(defun simsched-for-region-around-point ()
  "Parse and render a simple schedule view for the schedule the point is within.

  The schedule region is a series of newline-separated Entries, where an Entry conforms to:


  Entry | TimeRange Objective

  TimeRange | Time-Time

  Time | [0-9]
       | 1[0|1|2]
       | [0-9]:[0|3]0
       | 1[0|1|2]:[0|3]0

  Objective | .*
 "
  (interactive)
  (simsched/render-schedule (simsched/create-schedule-grid (simsched/parse-schedule))))
