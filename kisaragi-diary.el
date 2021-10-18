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

(defcustom kisaragi-diary/directory "diary/"
  "A path under `org-roam-directory' to store new diary entries."
  :group 'org-roam
  :type 'string)

(defun kisaragi-diary//read-date (prompt)
  "Like `org-read-date', but also highlight days with diary entries in calendar.

PROMPT is passed to `org-read-date'."
  (add-hook 'calendar-initial-window-hook #'kisaragi-diary//mark-calendar)
  (let ((org-read-date-prefer-future nil))
    (unwind-protect (org-read-date nil nil nil prompt)
      (remove-hook 'calendar-initial-window-hook #'kisaragi-diary//mark-calendar))))

;;;###autoload
(defun kisaragi-diary/new-entry (&optional time)
  "Create a new diary entry in `kisaragi-diary/directory'.

The entry will be stored as a file named after the current time.

To create an entry for a different time, pass the time in as the
TIME argument. Interactively, a \\[universal-argument] will
prompt for an ISO 8601 timestamp (YYYY-MM-DDThh:mm:ssZ)."
  (interactive
   (list (when current-prefix-arg
           (parse-iso8601-time-string
            (read-string "Timestamp: " (format-time-string "%FT%T%z"))))))
  (let ((now (or time (current-time))))
    (find-file (f-join org-roam-directory
                       kisaragi-diary/directory
                       (format-time-string "%Y%m%dT%H%M%S%z.org" now)))
    (insert "#+title: " (format-time-string "%FT%T%z" now) "\n")))

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
    (if current-prefix-arg
        (kisaragi-diary//read-date "Visit diary entry from day:")
      (kisaragi-notes//today))))
  (setq day (s-replace "-" "" day))
  (let ((file-list
         (-some--> org-roam-directory
           (f-join it kisaragi-diary/directory)
           (directory-files
            it :full
            (format (rx bos "%s" (0+ any) ".org")
                    day)
            :nosort))))
    (pcase (length file-list)
      (0 (message "No entry from %s" day))
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

(defun kisaragi-diary//mark-calendar ()
  "In a calendar window, mark days that have diary entries.
Implementation of `diary-mark-entries'."
  (interactive)
  (calendar-redraw)
  (cl-loop for file in (directory-files
                        (f-join org-roam-directory kisaragi-diary/directory))
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
