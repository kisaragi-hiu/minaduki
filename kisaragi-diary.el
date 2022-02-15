;;; kisaragi-diary.el --- My own way of keeping a diary  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; My alternative to org-roam-dailies, org-journal, or diary.el
;;
;; This was previously part of my .emacs.d. It probably fits here better.
;;
;; Usage:
;;
;; - M-x `minaduki/visit-entry-date' to visit entries from today.
;; - M-x `minaduki/visit-entry-yesterday' to visit entries from yesterday.
;; - C-u M-x `minaduki/visit-entry-date' to select a day from
;;   the calendar, then visit entries from that day. Days with diary
;;   entries are highlighted in the calendar.
;;
;;; Code:

(require 'calendar)
(require 'dash)
(require 'diary-lib)
(require 'f)
(require 'org)
(require 'parse-time)
(require 's)

(require 'org-roam-db)
(require 'kisaragi-notes-utils)
(require 'kisaragi-notes-vars)

(defun minaduki//read-date (prompt)
  "Like `org-read-date', but also highlight days with diary entries in calendar.

Highlighting is normally enabled by default when running
`calendar', and disabled in `org-read-date' using a pair of
advices. This then dances around that.

PROMPT is passed to `org-read-date'."
  ;; Adding them globally ensures that they are present even in the
  ;; calendar buffer.
  ;;
  ;; These hooks are run when the view changes.
  (add-hook 'calendar-today-visible-hook #'minaduki//mark-calendar)
  (add-hook 'calendar-today-invisible-hook #'minaduki//mark-calendar)
  (let ((org-read-date-prefer-future nil))
    (unwind-protect (org-read-date nil nil nil prompt)
      (remove-hook 'calendar-today-visible-hook #'minaduki//mark-calendar)
      (remove-hook 'calendar-today-invisible-hook #'minaduki//mark-calendar))))

(defun minaduki//set-calendar-mark-diary-entries-flag-nil (&rest _)
  "Set `calendar-mark-diary-entries-flag' to nil.

This is used as an advice before `org-read-date' to ensure diary
entries are NOT highlighted in it."
  (setq calendar-mark-diary-entries-flag nil))

(defun minaduki//set-calendar-mark-diary-entries-flag-t (&rest _)
  "Set `calendar-mark-diary-entries-flag' to t.

This is used as an advice after `org-read-date' to reenable diary
entry highlighting."
  (setq calendar-mark-diary-entries-flag t))

(defun minaduki//mark-calendar ()
  "In a calendar window, mark days that have diary entries.

This is used as an advice to override `diary-mark-entries' when
`org-roam-mode' (in this version) is enabled. My `org-roam-mode'
also sets `calendar-mark-diary-entries-flag' to t.

The marker is specified by `diary-entry-marker'."
  (interactive)
  (cl-loop for file in (directory-files minaduki/diary-directory)
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

(defun minaduki//find-entry-for-day (day)
  "Find a diary entry written on DAY and return it.

When there are multiple entries, prompt for selection.

DAY should be written in the format \"YYYY-MM-DD\" or
\"YYYYMMDD\".

This only considers files with names starting with DAY (with
dashes removed), and does not use any other method to determine
whether an entry is from DAY or not."
  (setq day (s-replace "-" "" day))
  (let ((file-list
         (directory-files
          minaduki/diary-directory :full
          (format (rx bos "%s" (0+ any) ".org")
                  day)
          :nosort)))
    (pcase (length file-list)
      (0 nil)
      (1 (car file-list))
      (_
       (let* ((title-file-alist
               (--map
                `(,(or (minaduki-db//fetch-title it)
                       (f-base it))
                  .
                  ,it)
                file-list))
              (selected-key
               (completing-read
                (format "Open an entry from %s: " day)
                title-file-alist)))
         (cdr
          (assoc selected-key title-file-alist)))))))

(provide 'kisaragi-diary)
;;; kisaragi-diary.el ends here
