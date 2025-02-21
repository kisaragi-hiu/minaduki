;;; minaduki-org-protocol.el --- Org-protocol handler for org-protocol://notes links  -*- lexical-binding: t; -*-

;; Copyright © 2021 Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;;         Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/kisaragi-hiu/org-roam
;;
;; Keywords: org-mode, convenience, org-protocol
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
;; An org-protocol handler. After loading this file,
;;
;;    emacsclient 'org-protocol://notes?key=banjoazusa2020'
;;
;; will open the file associated the cite key "banjoazusa2020", and
;;
;;    emacsclient 'org-protocol://notes?file=blender.org'
;;
;; will open /path/to/main-vault/blender.org.
;;
;; One way to set up org-protocol:// links on Linux, assuming you
;; always want to use `emacsclient -c':
;;
;; 1. Copy /usr/share/applications/emacs.desktop to
;;    ~/.local/share/applications/emacs.desktop, where it will shadow
;;    the system-wide file
;;
;; 2. Change Exec= from "emacs %F" to "emacsclient -c %U"
;;    - We use %U to get URLs
;;    - It seems to open files just fine, though if the desktop passed
;;      a file:// link to Emacs it will fail
;;
;; 3. Add "x-scheme-handler/org-protocol;" to the end of MimeType
;;
;; 4. Wait a bit for it to take effect
;;
;; 5. Try opening an org-protocol:// link again from, say, Firefox. It
;;    should ask you whether you want to open this link with it.
;;
;;; Code:
(require 'org-protocol)
(require 'minaduki)
(require 'minaduki-vault)

;;;; Functions

;;;###autoload
(cl-defun minaduki-org-protocol/open-file ((&key file key title))
  "An org-protocol handler to open a note file.

Arguments are passed in as a plist like
  \(minaduki-org-protocol/open-file \\='(:file FILE :key KEY)\)
.

This corresponds to the org-protocol URL
\"org-protocol://notes?file=FILE&key=KEY\".

TITLE: open the file with TITLE.
FILE: a path relative to the main vault.
KEY: a cite key corresponding to the KEY keyword

Example:

emacsclient \\='org-protocol://notes?title=Books\\='
emacsclient \\='org-protocol://notes?file=characters/闇音レンリ.org\\='
emacsclient \\='org-protocol://notes?key=banjoazusa2020\\='"
  (cond
   (title
    (minaduki-open title))
   (file
    (find-file (f-join (minaduki-vault-main) file)))
   (key
    (minaduki/edit-citekey-notes key))))

;;;###autoload
(define-minor-mode minaduki-org-protocol-mode
  "Enable Minaduki\\='s \"notes\" org-protocol.

With this mode turned on, these can work:

emacsclient \\='org-protocol://notes?file=characters/闇音レンリ.org\\='
emacsclient \\='org-protocol://notes?key=banjoazusa2020\\='"
  :global t :lighter ""
  :group 'minaduki
  (if minaduki-org-protocol-mode
      (cl-pushnew '("minaduki"
                    :protocol "notes"
                    :function minaduki-org-protocol/open-file)
                  org-protocol-protocol-alist
                  :test #'equal)
    (setq org-protocol-protocol-alist
          (--remove (equal (car it) "minaduki")
                    org-protocol-protocol-alist))))

(provide 'minaduki-org-protocol)

;;; minaduki-org-protocol.el ends here
