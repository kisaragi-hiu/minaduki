;;; org-roam-protocol.el --- Protocol handler for roam:// links  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (org "9.3"))

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
;; We extend org-protocol, adding custom Org-roam handlers. The setup
;; instructions for `org-protocol' can be found in org-protocol.el.
;;
;; We define 2 protocols:
;;
;; 1. "roam-file": This protocol simply opens the file given by the FILE key
;; 2. "roam-ref": This protocol creates or opens a note with the given REF
;;
;;; Code:
(require 'org-protocol)
(require 'org-roam)
(require 'org-roam-bibtex) ; orb-edit-notes

;;;; Functions
(cl-defun kisaragi-notes-protocol/open-file ((&key file key))
  "An org-protocol handler to open a note file.

Arguments are passed in as a plist like (:file FILE :key KEY).
This corresponds to the org-protocol URL
\"org-protocol://notes?file=FILE&key=KEY\".

FILE: a path relative to `org-roam-directory'.
KEY: a cite key corresponding to the ROAM_KEY keyword

FILE takes precedence over KEY.

Example:

emacsclient 'org-protocol://notes?file=characters/闇音レンリ.org'
emacsclient 'org-protocol://notes?key=banjoazusa2020'"
  (cond (file
         (find-file (f-join org-roam-directory file)))
        (key
         (orb-edit-notes key))))

(push '("kisaragi-notes"
        :protocol "notes"
        :function kisaragi-notes-protocol/open-file)
      org-protocol-protocol-alist)

(provide 'org-roam-protocol)

;;; org-roam-protocol.el ends here
