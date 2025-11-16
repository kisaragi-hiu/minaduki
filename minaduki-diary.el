;;; minaduki-diary.el --- My own way of keeping a diary  -*- lexical-binding: t; -*-
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

(require 'minaduki-db)
(require 'minaduki-utils)
(require 'minaduki-vars)

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
`minaduki-mode' is enabled; it also sets
`calendar-mark-diary-entries-flag' to t.

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

This considers:

- files in `minaduki/diary-directory' whose names start with DAY
  (without dashes) like diary/20000102.org,
  \"diary/20150501-whatever.org\"

- files that declared a timestamp in a \"modified\", \"created\", or
  \"date\" file property"
  (setq day (s-replace "-" "" day))
  (let* ((day (s-replace "-" "" day))
         (long-day (format "%s-%s-%s"
                           (substring day 0 4)
                           (substring day 4 6)
                           (substring day 6 8)))
         (modified-file-list
          ;; First select using glob, then use elisp to do precise filter
          (->> (minaduki-db-select
                "select file, meta from files where meta glob ?"
                (format "*%s*" long-day))
               (-filter
                (pcase-lambda (`(,_path ,meta))
                  (-some--> (minaduki-db::parse-value meta)
                    (map-elt it "modified")
                    (s-starts-with? long-day it))))
               (-map #'car)))
         (diary-file-list
          (directory-files
           minaduki/diary-directory :full
           (format (rx bos "%s" (0+ any) ".org")
                   day)
           :nosort))
         (file-list (append modified-file-list diary-file-list)))
    (pcase (length file-list)
      (0 nil)
      (1 (car file-list))
      (_
       (let* ((title-file-alist
               (--map
                `(,(or (minaduki-db::fetch-title it)
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

(defun minaduki--file-date ()
  "Return the date associated with this file."
  (cl-block nil
    (dolist (candidate
             (list 'main-title 'file-name 'created-prop))
      (when-let* ((cand-value
                   (pcase candidate
                     ('main-title
                      (car (minaduki-extract/main-title)))
                     ('file-name
                      (f-base (buffer-file-name)))
                     ('created-prop
                      (car (minaduki--get-file-prop "created")))))
                  (date (car
                         (s-match
                          (rx bos
                              (= 4 digit) (opt "-")
                              (= 2 digit) (opt "-")
                              (= 2 digit))
                          cand-value))))
        (when (iso8601-parse-date date)
          (cl-return date))))))

(provide 'minaduki-diary)
;;; minaduki-diary.el ends here
