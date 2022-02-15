;;; orb-utils.el --- Org Roam BibTeX: utility macros and functions -*- lexical-binding: t -*-

;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Mykhailo Shevchuk <mail@mshevchuk.com>
;;         Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex

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
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;; N.B. This file contains code snippets adopted from other
;; open-source projects. These snippets are explicitly marked as such
;; in place. They are not subject to the above copyright and
;; authorship claims.

;;; Commentary:
;;
;; This file contains utility macros and helper functions used accross
;; different org-mode-bibtex modules.  This library may be required
;; directly or through orb-core.el.  Definitions in this file should
;; only depend on built-in Emacs libraries.

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================
;;
;; org-roam requires org,org-element, dash, f, s, emacsql, emacsql-sqlite,
;; so all these libraries are always at our disposal

(require 'kisaragi-notes-completion)
(require 'org-roam-db)

(require 'warnings)

(defvar orb-citekey-format)

;; ============================================================================
;;;; Document properties
;; ============================================================================

;;;; Cache all refs.
;; This is because `orb-find-note-file' is called on every existing
;; cite key when `bibtex-completion' is preparing to display entries.
;;
;; Rapidly querying the DB is slower than `gethash' on a hash table.
;; It's about an order of magnitude slower on my device. Try it:
;;
;;     ;; Fetch a list of citekeys
;;     (let ((cite-refs (->> (org-roam--get-ref-path-completions nil "cite")
;;                        (--map (plist-get (cdr it) :ref)))))
;;       (list
;;        (benchmark-run 10
;;          (progn
;;            (setq orb-notes-cache nil)
;;            (mapc #'orb-find-note-file cite-refs)))
;;        (benchmark-run 10
;;          (mapc (lambda (key)
;;                  (minaduki-db/query
;;                   [:select [file] :from refs
;;                    :where (= ref $s1)]
;;                   key))
;;                cite-refs))))
;;
;; -> ((0.24450254200000002 0 0.0) (3.7526077939999998 0 0.0))

(defvar orb-notes-cache nil
  "Cache of ORB notes.")

(defun orb-make-notes-cache ()
  "Update ORB notes hash table `orb-notes-cache'."
  (let* ((db-entries (org-roam--get-ref-path-completions nil "cite"))
         (size (round (/ (length db-entries) 0.8125))) ;; ht oversize
         (ht (make-hash-table :test #'equal :size size)))
    (dolist (entry db-entries)
      (let* ((key (car entry))
             (value (plist-get (cdr (assoc key db-entries)) :path)))
        (puthash key value ht)))
    (setq orb-notes-cache ht)))

(defun orb-find-note-file (citekey)
  "Find note file associated with CITEKEY.
Returns the path to the note file, or nil if it doesn’t exist."
  (gethash citekey (or orb-notes-cache
                       (orb-make-notes-cache))))

(provide 'orb-utils)
;;; orb-utils.el ends here
