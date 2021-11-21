;;; kisaragi-diary.el --- My own way of keeping a diary  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; My alternative to org-roam-dailies, org-journal, or diary.el
;;
;; This was previously part of my .emacs.d. It probably fits here better.
;;
;; Usage:
;;
;; - M-x `kisaragi-diary/visit-entry-date' to visit entries from today.
;; - M-x `kisaragi-diary/visit-entry-yesterday' to visit entries from
;;   yesterday.
;; - C-u M-x `kisaragi-diary/visit-entry-date' to select a day from
;;   the calendar, then visit entries from that day. Days with diary
;;   entries are highlighted in the calendar.
;;
;;; Code:

(require 'calendar)
(require 'dash)
(require 'diary-lib)
(require 'f)
(require 'org)
(require 's)

(require 'parse-time)

(require 'org-roam-db)
(require 'kisaragi-notes-utils)

(require 'kisaragi-notes-vars)

(require 'kisaragi-notes-templates)

(defun kisaragi-diary//read-date (prompt)
  "Like `org-read-date', but also highlight days with diary entries in calendar.

Highlighting is normally enabled by default when running
`calendar', and disabled in `org-read-date' using a pair of
advices. This then dances around that.

PROMPT is passed to `org-read-date'."
  ;; Adding them globally ensures that they are present even in the
  ;; calendar buffer.
  ;;
  ;; These hooks are run when the view changes.
  (add-hook 'calendar-today-visible-hook #'kisaragi-diary//mark-calendar)
  (add-hook 'calendar-today-invisible-hook #'kisaragi-diary//mark-calendar)
  (let ((org-read-date-prefer-future nil))
    (unwind-protect (org-read-date nil nil nil prompt)
      (remove-hook 'calendar-today-visible-hook #'kisaragi-diary//mark-calendar)
      (remove-hook 'calendar-today-invisible-hook #'kisaragi-diary//mark-calendar))))

;;;###autoload
(defun kisaragi-diary/new-entry (&optional day? time)
  "Create a new diary entry in `kisaragi-notes/diary-directory'.

The entry will be stored as a file named after the current time
under `kisaragi-notes/diary-directory'. Example:

    diary/20211019T233513+0900.org

When DAY? is non-nil (with a \\[universal-argument]), the file
will be named as the current day instead. In addition, the
\"daily\" template under `kisaragi-notes/templates-directory'
will be used. Example:

    diary/20211019.org

When TIME is non-nil, create an entry for TIME instead of
`current-time'."
  (interactive "P")
  (let* ((now (or time (current-time)))
         (filename (if day?
                       (format-time-string "%Y%m%d" now)
                     (format-time-string "%Y%m%dT%H%M%S%z" now)))
         (title (if day?
                    (format-time-string "%F" now)
                  (format-time-string "%FT%T%z" now)))
         ;; Put this here so if we allow different templates later
         ;; it's easier to change
         (ext "org"))
    (find-file (f-join
                kisaragi-notes/diary-directory
                (concat filename "." ext)))
    ;; TODO: Markdown templates -> Markdown files
    (insert
     (or (and day?
              (let (;; This is how you pass arguments to org-capture-fill-templates
                    ;; It's either this or `org-capture-put'; this is
                    ;; less ugly.
                    (org-capture-plist (list :default-time now))
                    ;; Since we're creating a daily note, this
                    ;; variable isn't expected.
                    (org-extend-today-until 0))
                (kisaragi-notes-templates//make-note "daily")))
         (concat "#+title: " title "\n")))))

;;;###autoload
(defun kisaragi-diary/visit-entry-date (day)
  "Visit a diary entry written on DAY.

DAY defaults to today. With a \\[universal-argument], ask for DAY
first.

When there are multiple entries, prompt for selection.

DAY should be written in the format \"YYYY-MM-DD\" or
\"YYYYMMDD\".

This only considers files with names starting with DAY (with
dashes removed), and does not use any other method to determine
whether an entry is from DAY or not."
  (interactive
   (list
    ;; Why not `cond': if we're in the calendar buffer but our cursor
    ;; is not on a date (so `calendar-cursor-to-date' is nil), we want
    ;; to fall back to the next case. `cond' doesn't do that.
    (or (and (derived-mode-p 'calendar-mode)
             (-some-> (calendar-cursor-to-date)
               kisaragi-notes//date/calendar.el->ymd))

        (and current-prefix-arg
             (kisaragi-diary//read-date "Visit diary entry from day:"))

        (kisaragi-notes//today))))

  (setq day (s-replace "-" "" day))
  (let ((file-list
         (directory-files
          kisaragi-notes/diary-directory :full
          (format (rx bos "%s" (0+ any) ".org")
                  day)
          :nosort)))
    (pcase (length file-list)
      (0 (when (y-or-n-p
                (format "No entry from %s. Create one? " day))
           (kisaragi-diary/new-entry t (parse-iso8601-time-string
                                        (concat day "T00:00:00")))))
      (1 (find-file (car file-list)))
      (_
       (let* ((title-file-alist
               (--map
                ;; try to use an org-roam internal function to get the title
                ;; otherwise just use f-base
                `(,(or (kisaragi-notes-db//fetch-title it)
                       (f-base it))
                  .
                  ,it)
                file-list))
              (selected-key
               (completing-read
                (format "Open an entry from %s: " day)
                title-file-alist)))
         (find-file
          (cdr
           (assoc selected-key title-file-alist))))))))

;;;###autoload
(defun kisaragi-diary/visit-entry-yesterday ()
  "Visit a diary entry written yesterday."
  (interactive)
  (kisaragi-diary/visit-entry-date (kisaragi-notes//today -1)))

(defun kisaragi-diary//set-calendar-mark-diary-entries-flag-nil (&rest _)
  "Set `calendar-mark-diary-entries-flag' to nil.

This is used as an advice before `org-read-date' to ensure diary
entries are NOT highlighted in it."
  (setq calendar-mark-diary-entries-flag nil))

(defun kisaragi-diary//set-calendar-mark-diary-entries-flag-t (&rest _)
  "Set `calendar-mark-diary-entries-flag' to t.

This is used as an advice after `org-read-date' to reenable diary
entry highlighting."
  (setq calendar-mark-diary-entries-flag t))

(defun kisaragi-diary//mark-calendar ()
  "In a calendar window, mark days that have diary entries.

This is used as an advice to override `diary-mark-entries' when
`org-roam-mode' (in this version) is enabled. My `org-roam-mode'
also sets `calendar-mark-diary-entries-flag' to t.

The marker is specified by `diary-entry-marker'."
  (interactive)
  (cl-loop for file in (directory-files kisaragi-notes/diary-directory)
           when (>= (length file) 8)
           when (s-match (rx bos
                             (group digit digit digit digit)
                             (group digit digit)
                             (group digit digit))
                         file)
           ;; (string year month day)
           do
           (calendar-mark-date-pattern
            (string-to-number (cl-third it))
            (string-to-number (cl-fourth it))
            (string-to-number (cl-second it)))))

(provide 'kisaragi-diary)
;;; kisaragi-diary.el ends here
