;;; minaduki.el --- An Org-roam v1 fork -*- lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;         Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/kisaragi-hiu/minaduki
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "27.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.5") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (bibtex-completion "2.0.0") (markdown-mode "2.4") (transient "0.3.7"))

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
;; This is a fork of Org-roam v1, as I am unwilling to move away from
;; file links.
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)
(require 'seq)
(require 'bibtex-completion)
(eval-when-compile (require 'subr-x))

(require 'minaduki-vars)
(require 'minaduki-completion)
(require 'kisaragi-notes-commands)
(require 'kisaragi-notes-utils)

(when (featurep 'marginalia)
  (require 'kisaragi-notes-marginalia))

(when (featurep 'embark)
  (require 'kisaragi-notes-embark))

;;;; Features
;; These features should be able to be loaded order independently.
;; @TODO: implement something akin to `org-modules' that allows
;; selectively loading different sets of features.
;; ~NV [2020-05-22 Fri]

(when (featurep 'oc)
  (require 'kisaragi-notes-cite))

(require 'minaduki-buffer)
(require 'minaduki-bibtex)
(require 'org-roam-capture)
(require 'minaduki-extract)
(require 'org-roam-db)
(require 'org-roam-doctor)
(require 'org-roam-graph)
(require 'kisaragi-notes-wikilink)

;;; Org-roam-mode

(defun minaduki//backlink-to-current-p ()
  "Return t if the link at point is to the current Org-roam file."
  (save-match-data
    (let ((current-file (buffer-file-name minaduki-buffer//current))
          (backlink-dest (save-excursion
                           (let* ((context (org-element-context))
                                  (type (org-element-property :type context))
                                  (dest (org-element-property :path context)))
                             (pcase type
                               ("id" (minaduki-id/get-file dest))
                               (_ dest))))))
      (string= current-file backlink-dest))))

;;;; Function Faces
;; These faces are used by `org-link-set-parameters', which take one argument,
;; which is the path.

(defun minaduki//file-link-face (path)
  "Conditional face for file: links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (org-roam--org-roam-file-p)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and custom
                  (not (file-remote-p path)) ;; Prevent lockups opening Tramp links
                  (not (file-exists-p path)))
             'org-roam-link-invalid)
            ((and (bound-and-true-p minaduki-buffer/mode)
                  (minaduki//backlink-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (org-roam--org-roam-file-p path))
             'org-roam-link)
            (t
             'org-link)))))

(defun minaduki//id-link-face (id)
  "Conditional face for id links.
Applies `org-roam-link-current' if ID corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if ID corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (org-roam--org-roam-file-p)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and (bound-and-true-p minaduki-buffer/mode)
                  (minaduki//backlink-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (minaduki-id/get-file id t))
             'org-roam-link)
            ((and custom
                  (not (minaduki-id/get-file id)))
             'org-roam-link-invalid)
            (t
             'org-link)))))

;;;; Hooks and Advices

(defun minaduki//delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (minaduki-db//clear-file (expand-file-name file))))

(defun minaduki//get-link-replacement (old-path new-path &optional old-desc new-desc)
  "Create replacement text for link at point if OLD-PATH is a match.
Will update link to NEW-PATH. If OLD-DESC is set, and is not the
same as the link description, it is assumed that the user has
modified the description, and the description will not be
updated. Else, update with NEW-DESC."
  (let (type path label new-label)
    (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
      (setq path (org-element-property :path link))
      (when (and (string-equal (expand-file-name path) old-path)
                 (org-in-regexp org-link-bracket-re 1))
        (setq label (if (match-end 2)
                        (match-string-no-properties 2)
                      (org-link-unescape (match-string-no-properties 1))))
        (setq new-label (if (string-equal label old-desc) new-desc label))
        (minaduki/format-link :target new-path
                              :desc new-label)))))

(defun org-roam--replace-link (old-path new-path &optional old-desc new-desc)
  "Replace Org-roam file links with path OLD-PATH to path NEW-PATH.
If OLD-DESC is passed, and is not the same as the link
description, it is assumed that the user has modified the
description, and the description will not be updated. Else,
update with NEW-DESC."
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (when-let ((link (save-match-data (minaduki//get-link-replacement old-path new-path old-desc new-desc))))
        (replace-match link)))))

(defun org-roam--fix-relative-links (old-path)
  "Fix file-relative links in current buffer.
File relative links are assumed to originate from OLD-PATH. The
replaced links are made relative to the current buffer."
  (org-with-point-at 1
    (let (link new-link type path)
      (while (re-search-forward org-link-bracket-re nil t)
        (when (setq link (save-match-data (org-element-lineage (org-element-context) '(link) t)))
          (setq type (org-element-property :type link))
          (setq path (org-element-property :path link))
          (when (and (string= type "file")
                     (f-relative-p path))
            (setq new-link
                  (concat type ":" (org-roam-link-get-path (expand-file-name path (file-name-directory old-path)))))
            (replace-match new-link nil t nil 1)))))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Rename backlinks of OLD-FILE to refer to NEW-FILE-OR-DIR.
When NEW-FILE-OR-DIR is a directory, we use it to compute the new file path."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir))
        files-affected)
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam--org-roam-file-p old-file))
      (minaduki-db//ensure-built)
      (setq files-affected (minaduki-db/query [:select :distinct [source]
                                               :from links
                                               :where (= dest $s1)]
                                              old-file))
      ;; Remove database entries for old-file.org
      (minaduki-db//clear-file old-file)
      ;; If the new path is in a different directory, relative links
      ;; will break. Fix all file-relative links:
      (unless (string= (file-name-directory old-file)
                       (file-name-directory new-file))
        (org-roam-with-file new-file nil
          (org-roam--fix-relative-links old-file)))
      (when (org-roam--org-roam-file-p new-file)
        (minaduki-db//update-file new-file))
      ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
      (mapc (lambda (file)
              (setq file (if (string-equal (car file) old-file)
                             new-file
                           (car file)))
              (org-roam-with-file file nil
                (org-roam--replace-link old-file new-file)
                (save-buffer)
                (minaduki-db//update-file)))
            files-affected))))

(defun org-roam--id-new-advice (&rest _args)
  "Update the database if a new Org ID is created."
  (when (and (org-roam--org-roam-file-p)
             (not (eq minaduki-db/update-method 'immediate))
             (not (minaduki-capture/p)))
    (minaduki-db/update)))

(defun org-roam--execute-file-row-col (s)
  "Move to row:col if S match the row:col syntax. To be used with `org-execute-file-search-functions'."
  (when (string-match (rx (group (1+ digit))
                          ":"
                          (group (1+ digit))) s)
    (let ((row (string-to-number (match-string 1 s)))
          (col (string-to-number (match-string 2 s))))
      (org-goto-line row)
      (move-to-column (- col 1))
      t)))

(define-minor-mode minaduki-local-mode
  "Minor mode active in files tracked by minaduki."
  :global nil
  (setq org-roam-last-window (get-buffer-window))
  ;; TODO: use minaduki-local-mode-hook
  (run-hooks 'minaduki/file-setup-hook) ; Run user hooks
  (add-hook 'post-command-hook #'minaduki-buffer//update-maybe nil t)
  (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
  (add-hook 'after-save-hook #'minaduki-db/update nil t)
  (dolist (fn '(minaduki-completion/tags-at-point
                minaduki-completion/everywhere))
    (add-hook 'completion-at-point-functions fn nil t))
  (minaduki-buffer//update-maybe :redisplay t))

(defun minaduki-initialize ()
  "Initialize minaduki for this buffer."
  (when (org-roam--org-roam-file-p)
    (minaduki-local-mode)))

(define-minor-mode minaduki-mode
  "Toggle Minaduki-Local mode in all buffers.

With prefix ARG, enable Minaduki mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Minaduki-Local mode is enabled in all buffers where
`minaduki-initialize' would do it.
See `minaduki-local-mode' for more information on Minaduki-Local mode."
  :global t
  :group 'minaduki
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ) a") #'minaduki/literature-note-actions)
            (define-key map (kbd "C-c ) i") #'orb-insert)
            (define-key map (kbd "C-c ) C-f") #'minaduki/open-non-literature-note)
            (define-key map (kbd "C-c ) C-i") #'orb-insert-non-ref)
            map)
  :require 'minaduki
  (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                   (file-executable-p emacsql-sqlite3-executable))
              (executable-find "sqlite3"))
    (minaduki//warn :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'."))
  (if minaduki-mode
      (progn
        (add-hook 'after-change-major-mode-hook 'minaduki-initialize)
        (add-hook 'find-file-hook 'minaduki-initialize)
        (add-hook 'kill-emacs-hook #'minaduki-db//close)
        (when (and (not minaduki-db/file-update-timer)
                   (eq minaduki-db/update-method 'idle-timer))
          (setq minaduki-db/file-update-timer (run-with-idle-timer minaduki-db/update-idle-seconds t #'minaduki-db/update-cache-on-timer)))
        (advice-add 'rename-file :after #'org-roam--rename-file-advice)
        (advice-add 'delete-file :before #'minaduki//delete-file-advice)
        (add-to-list 'org-execute-file-search-functions 'org-roam--execute-file-row-col)
        (add-hook 'org-open-at-point-functions #'minaduki-id/open-id-at-point)
        (advice-add 'org-id-new :after #'org-roam--id-new-advice)
        (add-to-list 'bibtex-completion-find-note-functions #'orb-find-note-file)
        (advice-add 'bibtex-completion-edit-notes :override #'orb-edit-notes-ad)
        (advice-add 'bibtex-completion-parse-bibliography :before #'orb-bibtex-completion-parse-bibliography-ad)
        (setq calendar-mark-diary-entries-flag t)
        (advice-add 'diary-mark-entries :override #'minaduki//mark-calendar)
        (advice-add 'org-read-date :before #'minaduki//set-calendar-mark-diary-entries-flag-nil)
        (advice-add 'org-read-date :after #'minaduki//set-calendar-mark-diary-entries-flag-t)
        (when (fboundp 'org-link-set-parameters)
          (org-link-set-parameters "file" :face 'minaduki//file-link-face)
          (org-link-set-parameters "id" :face 'minaduki//id-link-face))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (minaduki-initialize))))
    (remove-hook 'after-change-major-mode-hook 'minaduki-initialize)
    (remove-hook 'find-file-hook 'minaduki-initialize)
    (remove-hook 'kill-emacs-hook #'minaduki-db//close)
    (remove-hook 'org-open-at-point-functions #'minaduki-id/open-id-at-point)
    (when minaduki-db/file-update-timer
      (cancel-timer minaduki-db/file-update-timer))
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'minaduki//delete-file-advice)
    (advice-remove 'org-id-new #'org-roam--id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))
    (minaduki-db//close)
    (setq bibtex-completion-find-note-functions
          (delq #'orb-find-note-file
                bibtex-completion-find-note-functions))
    (advice-remove 'bibtex-completion-edit-notes
                   #'orb-edit-notes-ad)
    (advice-remove 'bibtex-completion-parse-bibliography
                   #'orb-bibtex-completion-parse-bibliography-ad)
    (setq calendar-mark-diary-entries-flag nil)
    (advice-remove 'diary-mark-entries
                   #'minaduki//mark-calendar)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-nil)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-t)
    ;; TODO: we could keep track of the buffers with
    ;; minaduki-local-mode on so we don't have to iterate through
    ;; every open buffer.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when minaduki-local-mode
          (minaduki-local-mode -1))))))

;;;; org-roam-mode
(defun minaduki//find-file-hook-function ()
  "Called by `find-file-hook' when mode symbol `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (run-hooks 'minaduki/file-setup-hook) ; Run user hooks
    (add-hook 'post-command-hook #'minaduki-buffer//update-maybe nil t)
    (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
    (add-hook 'after-save-hook #'minaduki-db/update nil t)
    (dolist (fn '(minaduki-completion/tags-at-point
                  minaduki-completion/everywhere))
      (add-hook 'completion-at-point-functions fn nil t))
    (minaduki-buffer//update-maybe :redisplay t)))

;;;###autoload
(define-minor-mode org-roam-mode
  "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

This also sets `orb-edit-notes' as a function for editing
literature notes, as well as setting up Bibtex-completion.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively."
  :lighter " Org-roam"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ) a") #'minaduki/literature-note-actions)
            (define-key map (kbd "C-c ) i") #'orb-insert)
            (define-key map (kbd "C-c ) C-f") #'minaduki/open-non-literature-note)
            (define-key map (kbd "C-c ) C-i") #'orb-insert-non-ref)
            map)
  :group 'minaduki
  :require 'org-roam
  :global t
  (cond

   (org-roam-mode
    (add-hook 'find-file-hook #'minaduki//find-file-hook-function)

    ;; DB
    (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                     (file-executable-p emacsql-sqlite3-executable))
                (executable-find "sqlite3"))
      (minaduki//warn :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at Org-roam > Installation > Post-Installation Tasks."))
    (add-hook 'kill-emacs-hook #'minaduki-db//close)
    (when (and (not minaduki-db/file-update-timer)
               (eq minaduki-db/update-method 'idle-timer))
      (setq minaduki-db/file-update-timer (run-with-idle-timer minaduki-db/update-idle-seconds t #'minaduki-db/update-cache-on-timer)))
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'minaduki//delete-file-advice)

    ;; Link
    (add-to-list 'org-execute-file-search-functions 'org-roam--execute-file-row-col)
    (add-hook 'org-open-at-point-functions #'minaduki-id/open-id-at-point)
    (advice-add 'org-id-new :after #'org-roam--id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (org-link-set-parameters "file" :face 'minaduki//file-link-face)
      (org-link-set-parameters "id" :face 'minaduki//id-link-face))

    ;; Apply these now. New buffers get this in the find-file hook.
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (add-hook 'post-command-hook #'minaduki-buffer//update-maybe nil t)
        (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
        (add-hook 'after-save-hook #'minaduki-db/update nil t)))

    ;; Bibtex
    (add-to-list 'bibtex-completion-find-note-functions
                 #'orb-find-note-file)
    (advice-add 'bibtex-completion-edit-notes
                :override #'orb-edit-notes-ad)
    (advice-add 'bibtex-completion-parse-bibliography
                :before #'orb-bibtex-completion-parse-bibliography-ad)

    ;; Calendar
    (setq calendar-mark-diary-entries-flag t)
    (advice-add 'diary-mark-entries :override #'minaduki//mark-calendar)
    (advice-add 'org-read-date
                :before #'minaduki//set-calendar-mark-diary-entries-flag-nil)
    (advice-add 'org-read-date
                :after #'minaduki//set-calendar-mark-diary-entries-flag-t))

   (t

    (remove-hook 'find-file-hook #'minaduki//find-file-hook-function)

    ;; DB
    (setq org-execute-file-search-functions (delete 'org-roam--execute-file-row-col org-execute-file-search-functions))
    (remove-hook 'kill-emacs-hook #'minaduki-db//close)
    (when minaduki-db/file-update-timer
      (cancel-timer minaduki-db/file-update-timer))
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'minaduki//delete-file-advice)
    (minaduki-db//close)

    ;; Link
    (remove-hook 'org-open-at-point-functions #'minaduki-id/open-id-at-point)
    (advice-remove 'org-id-new #'org-roam--id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))

    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'minaduki-buffer//update-maybe t)
        (remove-hook 'before-save-hook #'org-roam-link--replace-link-on-save t)
        (remove-hook 'after-save-hook #'minaduki-db/update t)))

    ;; Bibtex
    (setq bibtex-completion-find-note-functions
          (delq #'orb-find-note-file
                bibtex-completion-find-note-functions))
    (advice-remove 'bibtex-completion-edit-notes
                   #'orb-edit-notes-ad)
    (advice-remove 'bibtex-completion-parse-bibliography
                   #'orb-bibtex-completion-parse-bibliography-ad)

    ;; Calendar
    (setq calendar-mark-diary-entries-flag nil)
    (advice-remove 'diary-mark-entries
                   #'minaduki//mark-calendar)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-nil)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-t))))

(add-hook 'org-roam-mode-hook #'minaduki-db/build-cache)
(add-hook 'minaduki-mode-hook #'minaduki-db/build-cache)

;;; Interactive Commands
;;;###autoload
(defalias 'org-roam 'minaduki-buffer/toggle-display)

(provide 'minaduki)
;;; minaduki.el ends here
