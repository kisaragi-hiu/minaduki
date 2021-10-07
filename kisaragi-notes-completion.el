;;; kisaragi-notes-completion.el --- Completion utilities -*- coding: utf-8; lexical-binding: t; -*-

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
;; Completion utilities.
;;
;;; Code:

(defun kisaragi-notes-completion//mark-category (seq category)
  "Mark SEQ as being in CATEGORY.

Return a collection (as defined by `completing-read') that is the
same as passing SEQ to `completing-read' except with category
information attached.

See Info node `(elisp) Programmed Completion' for the official
documentation on this system.

Category information is conveyed by having collection functions
respond metadata when Emacs asks for it. This has begun being
utilized by packages such as Marginalia to display different
information for different types of completion entries, or by
Embark to create what are in effect context menus."
  (lambda (str pred action)
    (pcase action
      ('metadata
       `(metadata (category . ,category)))
      (_
       (all-completions str seq pred)))))

(provide 'kisaragi-notes-completion)

;;; kisaragi-notes-completion.el ends here
