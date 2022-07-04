;;; minaduki.el --- An Org-roam v1 fork -*- lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;         Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/kisaragi-hiu/minaduki
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "27.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.5") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (markdown-mode "2.4") (transient "0.3.7"))

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
(require 'markdown-mode)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments

(require 'oc)
(require 'oc-basic)

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)
(require 'seq)
(eval-when-compile (require 'subr-x))

(require 'minaduki-vars)
(require 'minaduki-completion)
(require 'minaduki-commands)
(require 'minaduki-utils)
(require 'minaduki-vault)

(when (featurep 'marginalia)
  (require 'kisaragi-notes-marginalia))

(when (featurep 'embark)
  (require 'kisaragi-notes-embark))

(require 'minaduki-buffer)
(require 'minaduki-bibtex)
(require 'org-roam-capture)
(require 'minaduki-extract)
(require 'minaduki-db)

(require 'kisaragi-notes-wikilink)

(require 'minaduki-mode)

;;;; Function Faces
;; These faces are used by `org-link-set-parameters', which take one argument,
;; which is the path.

(defun minaduki//backlink-to-current-p ()
  "Return t if the link at point is to the current Org-roam file."
  (save-match-data
    (let ((current-file (buffer-file-name minaduki-buffer//current))
          (backlink-dest (save-excursion
                           (let* ((context (org-element-context))
                                  (type (org-element-property :type context))
                                  (dest (org-element-property :path context)))
                             (pcase type
                               ("id" (minaduki-db//fetch-file :id dest))
                               (_ dest))))))
      (string= current-file backlink-dest))))

(defun minaduki//file-link-face (path)
  "Conditional face for file: links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (minaduki//in-vault?)))
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
                  (minaduki//in-vault? path))
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
                        (minaduki//in-vault?)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and (bound-and-true-p minaduki-buffer/mode)
                  (minaduki//backlink-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (minaduki-db//fetch-file :id id))
             'org-roam-link)
            ;; FIXME: this breaks the display of ID links to untracked
            ;; files.
            ((and custom
                  (not (minaduki-db//fetch-file :id id)))
             'org-roam-link-invalid)
            (t
             'org-link)))))

;;; Interactive Commands
;;;###autoload
(defalias 'minaduki/sidebar 'minaduki-buffer/toggle-display)

(provide 'minaduki)
;;; minaduki.el ends here
