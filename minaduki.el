;;; minaduki.el --- An extremely opinionated Org-roam v1 fork -*- lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;         Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://github.com/kisaragi-hiu/minaduki
;; Keywords: org-mode, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (yaml "0.5.1") (dash "2.13") (f "0.17.2")
;;         (s "1.12.0") (org "9.5") (markdown-mode "2.4") (transient "0.3.7")
;;         (parsebib "6.7"))

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

(require 'minaduki-buffer)
(require 'minaduki-extract)
(require 'minaduki-db)
(require 'minaduki-timeline)

(require 'minaduki-wikilink)

(require 'minaduki-mode)

(provide 'minaduki)
;;; minaduki.el ends here
