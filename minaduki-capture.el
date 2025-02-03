;;; minaduki-capture.el --- Capture functionality -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides capture functionality for org-roam
;;; Code:
;;;; Library Requires
(require 'dash)
(require 's)
(require 'cl-lib)
(require 'org-capture)

(require 'minaduki-utils)
(require 'minaduki-vault)
(require 'minaduki-db)
(require 'minaduki-extract)
(require 'minaduki-completion)
(require 'minaduki-vars)

;; Declarations

(defvar minaduki-capture//file-path nil
  "The file path for the Org-roam capture.
This variable is set during the Org-roam capture process.")

(defvar minaduki-capture//info nil
  "An alist of additional information passed to the Org-roam template.
This variable is populated dynamically, and is only non-nil
during the Org-roam capture process.")

(defvar minaduki-capture//context nil
  "A symbol, that reflects the context for obtaining the exact point in a file.
This variable is populated dynamically, and is only active during
an Org-roam capture process.

The `title' context is used in `org-roam-insert' and
`org-roam-find-file', where the capture process is triggered upon
trying to create a new file without that `title'.

The `ref' context is used by `org-roam-protocol', where the
capture process is triggered upon trying to find or create a new
note with the given `ref'.")

(defvar minaduki-capture/additional-template-props nil
  "Additional props to be added to the Org-roam template.")

(defconst minaduki-capture//template-keywords '(:file-name :head :olp)
  "Keywords used in `minaduki-capture/templates' specific to Org-roam.")

(defcustom minaduki-capture/templates
  `(("d" "default" plain (function minaduki-capture//get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+title: ${title}\n"
     :unnarrowed t))
  "Capture templates for Org-roam.
The Org-roam capture-templates  builds on the default behaviours of
`org-capture-templates' by expanding them in 3 areas:

1. Template-expansion capabilities are extended with additional
   custom syntax. See `minaduki-capture//fill-template' for more
   details.

2. The `:file-name' key is added, which defines the naming format
   to use when creating new notes. This file-name is relative to
   the main vault, and is without the file-extension.

3. The `:head' key is added, which contains the template that is
   inserted upon the creation of a new file. This is where you
   your note metadata should go.

Each template should have the following structure:

\(KEY DESCRIPTION `plain' `(function minaduki-capture//get-point)'
  TEMPLATE
  `:file-name' FILENAME-FORMAT
  `:head' HEADER-FORMAT
  `:unnarrowed t'
  OPTIONS-PLIST)

The elements of a template-entry and their placement are the same
as in `org-capture-templates', except that the entry type must
always be the symbol `plain', and that the target must always be
the list `(function minaduki-capture//get-point)'.

Org-roam requires the plist elements `:file-name' and `:head' to
be present, and it’s recommended that `:unnarrowed' be set to t."
  :group 'minaduki
  ;; Adapted from `org-capture-templates'
  :type
  '(repeat
    (choice :value ("d" "default" plain (function minaduki-capture//get-point)
                    "%?"
                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                    :head "#+title: ${title}\n"
                    :unnarrowed t)
            (list :tag "Multikey description"
                  (string :tag "Keys       ")
                  (string :tag "Description"))
            (list :tag "Template entry"
                  (string :tag "Keys              ")
                  (string :tag "Description       ")
                  (const :format "" plain)
                  (const :format "" (function minaduki-capture//get-point))
                  (choice :tag "Template          "
                          (string :tag "String"
                                  :format "String:\n            \
Template string   :\n%v")
                          (list :tag "File"
                                (const :format "" file)
                                (file :tag "Template file     "))
                          (list :tag "Function"
                                (const :format "" function)
                                (function :tag "Template function ")))
                  (const :format "File name format  :" :file-name)
                  (string :format " %v" :value "#+title: ${title}\n")
                  (const :format "Header format     :" :head)
                  (string :format "\n%v" :value "%<%Y%m%d%H%M%S>-${slug}")
                  (const :format "" :unnarrowed) (const :format "" t)
                  (plist :inline t
                         :tag "Options"
                         ;; Give the most common options as checkboxes
                         :options
                         (((const :format "%v " :prepend) (const t))
                          ((const :format "%v " :immediate-finish) (const t))
                          ((const :format "%v " :jump-to-captured) (const t))
                          ((const :format "%v " :empty-lines) (const 1))
                          ((const :format "%v " :empty-lines-before) (const 1))
                          ((const :format "%v " :empty-lines-after) (const 1))
                          ((const :format "%v " :clock-in) (const t))
                          ((const :format "%v " :clock-keep) (const t))
                          ((const :format "%v " :clock-resume) (const t))
                          ((const :format "%v " :time-prompt) (const t))
                          ((const :format "%v " :tree-type) (const week))
                          ((const :format "%v " :table-line-pos) (string))
                          ((const :format "%v " :kill-buffer) (const t))))))))

(defcustom minaduki-capture/immediate-template
  (append (car minaduki-capture/templates) '(:immediate-finish t))
  "Capture template to use for immediate captures in Org-roam.
This is a single template, so do not enclose it into a list.
See `minaduki-capture/templates' for details on templates."
  :group 'minaduki
  ;; Adapted from `org-capture-templates'
  :type
  '(list :tag "Template entry"
         :value ("d" "default" plain (function minaduki-capture//get-point)
                 "%?"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+title: ${title}\n"
                 :unnarrowed t
                 :immediate-finish t)
         (string :tag "Keys              ")
         (string :tag "Description       ")
         (const :format "" plain)
         (const :format "" (function minaduki-capture//get-point))
         (choice :tag "Template          "
                 (string :tag "String"
                         :format "String:\n            \
Template string   :\n%v")
                 (list :tag "File"
                       (const :format "" file)
                       (file :tag "Template file     "))
                 (list :tag "Function"
                       (const :format "" function)
                       (function :tag "Template function ")))
         (const :format "File name format  :" :file-name)
         (string :format " %v" :value "#+title: ${title}\n")
         (const :format "Header format     :" :head)
         (string :format "\n%v" :value "%<%Y%m%d%H%M%S>-${slug}")
         (const :format "" :unnarrowed) (const :format "" t)
         (const :format "" :immediate-finish) (const :format "" t)
         (plist :inline t
                :tag "Options"
                ;; Give the most common options as checkboxes
                :options
                (((const :format "%v " :prepend) (const t))
                 ((const :format "%v " :jump-to-captured) (const t))
                 ((const :format "%v " :empty-lines) (const 1))
                 ((const :format "%v " :empty-lines-before) (const 1))
                 ((const :format "%v " :empty-lines-after) (const 1))
                 ((const :format "%v " :clock-in) (const t))
                 ((const :format "%v " :clock-keep) (const t))
                 ((const :format "%v " :clock-resume) (const t))
                 ((const :format "%v " :time-prompt) (const t))
                 ((const :format "%v " :tree-type) (const week))
                 ((const :format "%v " :table-line-pos) (string))
                 ((const :format "%v " :kill-buffer) (const t))))))

(defcustom minaduki-capture/ref-templates
  '(("r" "ref" plain #'minaduki-capture//get-point
     "%?"
     :file-name "${slug}"
     :head "#+title: ${title}\n#+key: ${ref}"
     :unnarrowed t))
  "The Org-roam templates used during a capture from the roam-ref protocol.
Details on how to specify for the template is given in
`minaduki-capture/templates'."
  :group 'minaduki
  ;; Adapted from `org-capture-templates'
  :type
  '(repeat
    (choice :value ("d" "default" plain (function minaduki-capture//get-point)
                    "%?"
                    :file-name "${slug}"
                    :head "#+title: ${title}\n#+key: ${ref}\n"
                    :unnarrowed t)
            (list :tag "Multikey description"
                  (string :tag "Keys       ")
                  (string :tag "Description"))
            (list :tag "Template entry"
                  (string :tag "Keys              ")
                  (string :tag "Description       ")
                  (const :format "" plain)
                  (const :format "" (function minaduki-capture//get-point))
                  (choice :tag "Template          "
                          (string :tag "String"
                                  :format "String:\n            \
Template string   :\n%v")
                          (list :tag "File"
                                (const :format "" file)
                                (file :tag "Template file     "))
                          (list :tag "Function"
                                (const :format "" function)
                                (function :tag "Template function ")))
                  (const :format "File name format  :" :file-name)
                  (string :format " %v" :value "#+title: ${title}\n")
                  (const :format "Header format     :" :head)
                  (string :format "\n%v" :value "%<%Y%m%d%H%M%S>-${slug}")
                  (const :format "" :unnarrowed) (const :format "" t)
                  (plist :inline t
                         :tag "Options"
                         ;; Give the most common options as checkboxes
                         :options
                         (((const :format "%v " :prepend) (const t))
                          ((const :format "%v " :immediate-finish) (const t))
                          ((const :format "%v " :jump-to-captured) (const t))
                          ((const :format "%v " :empty-lines) (const 1))
                          ((const :format "%v " :empty-lines-before) (const 1))
                          ((const :format "%v " :empty-lines-after) (const 1))
                          ((const :format "%v " :clock-in) (const t))
                          ((const :format "%v " :clock-keep) (const t))
                          ((const :format "%v " :clock-resume) (const t))
                          ((const :format "%v " :time-prompt) (const t))
                          ((const :format "%v " :tree-type) (const week))
                          ((const :format "%v " :table-line-pos) (string))
                          ((const :format "%v " :kill-buffer) (const t))))))))

(defun minaduki-capture/p ()
  "Return t if the current capture process is an Org-roam capture.
This function is to only be called when org-capture-plist is
valid for the capture (i.e. initialization, and finalization of
the capture)."
  (plist-get org-capture-plist :org-roam))

(defun minaduki-capture//get (keyword)
  "Get the value for KEYWORD from the `minaduki-capture/template'."
  (plist-get (plist-get org-capture-plist :org-roam) keyword))

(defun minaduki-capture//put (&rest stuff)
  "Put properties from STUFF into the `minaduki-capture/template'."
  (let ((p (plist-get org-capture-plist :org-roam)))
    (while stuff
      (setq p (plist-put p (pop stuff) (pop stuff))))
    (setq org-capture-plist
          (plist-put org-capture-plist :org-roam p))))

;; FIXME: Pending upstream patch
;; https://orgmode.org/list/87h7tv9pkm.fsf@hidden/T/#u
;;
;; Org-capture's behaviour right now is that `org-capture-plist' is valid only
;; during the initialization of the Org-capture buffer. The value of
;; `org-capture-plist' is saved into buffer-local `org-capture-current-plist'.
;; However, the value for that particular capture is no longer accessible for
;; hooks in `org-capture-after-finalize-hook', since the capture buffer has been
;; cleaned up.
;;
;; This advice restores the global `org-capture-plist' during finalization, so
;; the plist is valid during both initialization and finalization of the
;; capture.
(defun minaduki-capture//update-plist (&optional _)
  "Update global plist from local var."
  (setq org-capture-plist org-capture-current-plist))

(advice-add 'org-capture-finalize :before #'minaduki-capture//update-plist)

(defun minaduki-capture//finalize ()
  "Finalize the `minaduki-capture' process."
  (let* ((finalize (minaduki-capture//get :finalize))
         (region (minaduki-capture//get :region))
         (beg (car region))
         (end (cdr region)))
    (unless org-note-abort
      (pcase finalize
        ('find-file
         (when-let ((file-path (minaduki-capture//get :file-path)))
           (minaduki::find-file file-path)
           (run-hooks 'minaduki-capture/after-find-file-hook)))
        ('insert-link
         (when-let* ((mkr (minaduki-capture//get :insert-at))
                     (buf (marker-buffer mkr)))
           (with-current-buffer buf
             (when region
               (delete-region (car region) (cdr region)))
             (let ((path (minaduki-capture//get :file-path))
                   (id? (equal "id" (minaduki-capture//get :link-type)))
                   (desc (minaduki-capture//get :link-description)))
               (if (eq (point) (marker-position mkr))
                   (insert (minaduki::format-link :target path
                                                  :desc desc
                                                  :type (and id? 'id)))
                 (org-with-point-at mkr
                   (insert (minaduki::format-link :target path
                                                  :desc desc
                                                  :type (and id? 'id)))))))))))
    (when region
      (set-marker beg nil)
      (set-marker end nil))
    (minaduki-capture//save-file-maybe)
    (remove-hook 'org-capture-after-finalize-hook #'minaduki-capture//finalize)))

(defun minaduki-capture//install-finalize ()
  "Install `minaduki-capture//finalize' if the capture is an Org-roam capture."
  (when (minaduki-capture/p)
    (add-hook 'org-capture-after-finalize-hook #'minaduki-capture//finalize)))

(add-hook 'org-capture-prepare-finalize-hook #'minaduki-capture//install-finalize)

(defun minaduki-capture//fill-template (str)
  "Expand the template STR, returning the string.
This is an extension of org-capture's template expansion.

First, it expands ${var} occurrences in STR, using `minaduki-capture//info'.
If there is a ${var} with no matching var in the alist, the value
of var is prompted for via `completing-read'.

Next, it expands the remaining template string using
`org-capture-fill-template'."
  (-> str
      (s-format (lambda (key)
                  (or (s--aget minaduki-capture//info key)
                      (when-let ((val (completing-read (format "%s: " key) nil)))
                        (push (cons key val) minaduki-capture//info)
                        val))) nil)
      (org-capture-fill-template)))

(defun minaduki-capture//save-file-maybe ()
  "Save the file conditionally.
The file is saved if the original value of :no-save is not t and
`org-note-abort' is not t. It is added to
`org-capture-after-finalize-hook'."
  (cond
   ((and (minaduki-capture//get :new-file)
         org-note-abort)
    (with-current-buffer (org-capture-get :buffer)
      (set-buffer-modified-p nil)
      (kill-buffer)))
   ((and (not (minaduki-capture//get :orig-no-save))
         (not org-note-abort))
    (with-current-buffer (org-capture-get :buffer)
      (save-buffer)))))

(defun minaduki-capture//get-file-path (basename)
  "Return path for Org-roam file with BASENAME."
  (let* ((ext (or (car minaduki-file-extensions)
                  "org"))
         (file (concat basename "." ext)))
    (expand-file-name file (minaduki-vault-main))))

(defun minaduki-capture//new-file (&optional allow-existing-file-p)
  "Return the path to file during an Org-roam capture.

This function reads the file-name attribute of the currently
active Org-roam template.

If the file path already exists, and not ALLOW-EXISTING-FILE-P,
raise a warning.

Else, to insert the header content in the file, `org-capture'
prepends the `:head' property of the Org-roam capture template.

To prevent the creation of a new file if the capture process is
aborted, we do the following:

1. Save the original value of the capture template's :no-save.
2. Set the capture template's :no-save to t.
3. Add a function on `org-capture-before-finalize-hook' that saves
the file if the original value of :no-save is not t and
`org-note-abort' is not t."
  (let* ((name-templ (or (minaduki-capture//get :file-name)
                         (user-error "Template needs to specify `:file-name'")))
         (new-id (s-trim (minaduki-capture//fill-template
                          name-templ)))
         (file-path (minaduki-capture//get-file-path new-id))
         (roam-head (or (minaduki-capture//get :head)
                        ""))
         (org-template (org-capture-get :template))
         (roam-template (concat roam-head org-template)))
    (if (or (file-exists-p file-path)
            (find-buffer-visiting file-path))
        (unless allow-existing-file-p
          (minaduki::warn
           :warning
           "Attempted to recreate existing file: %s.
This can happen when your org-roam db is not in sync with your notes.
Using existing file..." file-path))
      (make-directory (file-name-directory file-path) t)
      (minaduki-capture//put :orig-no-save (org-capture-get :no-save)
                             :new-file t)
      (pcase minaduki-capture//context
        ('dailies
         ;; Populate the header of the daily file before capture to prevent it
         ;; from appearing in the buffer-restriction
         (save-window-excursion
           (find-file file-path)
           (insert (substring (org-capture-fill-template (concat roam-head "*"))
                              0 -2))
           (set-buffer-modified-p nil))
         (org-capture-put :template org-template))
        (_
         (org-capture-put :template roam-template
                          :type 'plain)))
      (org-capture-put :no-save t))
    file-path))

(defun minaduki-capture/find-or-create-olp (olp)
  "Return a marker pointing to the entry at OLP in the current buffer.
If OLP does not exist, create it. If anything goes wrong, throw
an error, and if you need to do something based on this error,
you can catch it with `condition-case'."
  (let* ((level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" (current-buffer)))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading olp)
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (org-insert-heading nil nil t)
           (unless (= lmax 1) (org-do-demote))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

(defun minaduki-capture//get-ref-path (type path)
  "Get the file path to the ref with TYPE and PATH."
  (caar (minaduki-db-select
         '("select file from refs"
           "where type = ?"
           "and ref = ?"
           "limit 1")
         type path)))

(defun minaduki-capture//get-point ()
  "Return exact point to file for org-capture-template.
The file to use is dependent on the context:

If the search is via title, it is assumed that the file does not
yet exist, and Org-roam will attempt to create new file.

If the search is via daily notes, \\='time will be passed via
`minaduki-capture//info'. This is used to alter the default time
in `org-capture-templates'.

If the search is via ref, it is matched against the Org-roam database.
If there is no file with that ref, a file with that ref is created.

This function is used solely in Org-roam\\='s capture templates: see
`minaduki-capture/templates'."
  (let* ((file-path (pcase minaduki-capture//context
                      ('capture
                       (or (cdr (assoc 'file minaduki-capture//info))
                           (minaduki-capture//new-file)))
                      ('title
                       (minaduki-capture//new-file))
                      ('dailies
                       (org-capture-put :default-time (cdr (assoc 'time minaduki-capture//info)))
                       (minaduki-capture//new-file 'allow-existing))
                      ('ref
                       (if-let ((ref (cdr (assoc 'ref minaduki-capture//info))))
                           (pcase (minaduki-extract//process-ref ref)
                             (`(,type . ,path)
                              (or (minaduki-capture//get-ref-path type path)
                                  (minaduki-capture//new-file)))
                             (_ (user-error "%s is not a valid ref" ref)))
                         (error "Ref not found in `minaduki-capture//info'")))
                      (_ (error "Invalid minaduki-capture/context")))))
    (org-capture-put :template
                     (minaduki-capture//fill-template (org-capture-get :template)))
    (minaduki-capture//put :file-path file-path
                           :finalize (or (org-capture-get :finalize)
                                         (minaduki-capture//get :finalize)))
    (while minaduki-capture/additional-template-props
      (let ((prop (pop minaduki-capture/additional-template-props))
            (val (pop minaduki-capture/additional-template-props)))
        (minaduki-capture//put prop val)))
    (set-buffer (org-capture-target-buffer file-path))
    (widen)
    (if-let* ((olp (minaduki-capture//get :olp)))
        (condition-case err
            (when-let ((marker (minaduki-capture/find-or-create-olp olp)))
              (goto-char marker)
              (set-marker marker nil))
          (error
           (when (minaduki-capture//get :new-file)
             (kill-buffer))
           (signal (car err) (cdr err))))
      (goto-char (point-max)))))

(defun minaduki-capture//convert-template (template)
  "Convert TEMPLATE from Org-roam syntax to `org-capture-templates' syntax."
  (pcase template
    (`(,_key ,_description) template)
    (`(,key ,description ,type ,target . ,rest)
     (let ((converted `(,key ,description ,type ,target
                             ,(unless (keywordp (car rest)) (pop rest))))
           org-roam-plist
           options)
       (while rest
         (let* ((key (pop rest))
                (val (pop rest))
                (custom (member key minaduki-capture//template-keywords)))
           (when (and custom
                      (not val))
             (user-error "Invalid capture template format: %s\nkey %s cannot be nil" template key))
           (push val (if custom org-roam-plist options))
           (push key (if custom org-roam-plist options))))
       (append converted options `(:org-roam ,org-roam-plist))))
    (_ (user-error "Invalid capture template format: %s" template))))

(defcustom minaduki-capture/after-find-file-hook nil
  "Hook that is run right after an Org-roam capture process is finalized.
Suitable for moving point."
  :group 'minaduki
  :type 'hook)

(defcustom minaduki-capture/function #'org-capture
  "Function that is invoked to start the `org-capture' process."
  :group 'minaduki
  :type 'function)

(defun minaduki-capture//capture (&optional goto keys)
  "Create a new file, and return the path to the edited file.
The templates are defined at `minaduki-capture/templates'.  The
GOTO and KEYS argument have the same functionality as
`org-capture'."
  (let* ((org-capture-templates (mapcar #'minaduki-capture//convert-template minaduki-capture/templates))
         (one-template-p (= (length org-capture-templates) 1))
         org-capture-templates-contexts)
    (when one-template-p
      (setq keys (caar org-capture-templates)))
    (if (or one-template-p
            (eq minaduki-capture/function 'org-capture))
        (org-capture goto keys)
      (funcall-interactively minaduki-capture/function))))

;;;###autoload
(defun minaduki-capture (&optional goto keys)
  "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `minaduki-capture/templates'.
Arguments GOTO and KEYS see `org-capture'."
  (interactive "P")
  (let* ((completions (minaduki-db::fetch-all-nodes))
         (title-with-keys (completing-read "File: " completions))
         (res (cdr (assoc title-with-keys completions)))
         (title (or (plist-get res :title) title-with-keys))
         (file-path (plist-get res :path)))
    (let ((minaduki-capture//info (list (cons 'title title)
                                        (cons 'slug (minaduki::to-slug title))
                                        (cons 'file file-path)))
          (minaduki-capture//context 'capture))
      (condition-case err
          (minaduki-capture//capture goto keys)
        (error (user-error "%s.  Please adjust `minaduki-capture/templates'"
                           (error-message-string err)))))))

(provide 'minaduki-capture)

;;; minaduki-capture.el ends here
