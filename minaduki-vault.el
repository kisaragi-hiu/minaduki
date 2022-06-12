;;; minaduki-vault.el --- Vault logic -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'minaduki-utils)
(require 'minaduki-vars)

(defsubst minaduki//excluded? (path)
  "Should PATH be excluded from indexing?"
  (and minaduki-file-exclude-regexp
       (string-match-p minaduki-file-exclude-regexp path)))

(defun minaduki//in-vault? (&optional path)
  "Return t if PATH is in a vault.

If PATH is not specified, use the current buffer's file-path.

Currently only one vault is supported, which is specified by
`org-directory'.

A path is in a vault if it has the right extension, is not
excluded (by `minaduki-file-exclude-regexp'), and is located
under the vault (`org-directory')."
  (when-let ((path (or path
                       minaduki//file-name
                       (-> (buffer-base-buffer)
                           (buffer-file-name)))))
    (save-match-data
      (and
       (member (minaduki//file-name-extension path)
               minaduki-file-extensions)
       (not (minaduki//excluded? path))
       (f-descendant-of-p path (expand-file-name org-directory))))))

(defun org-roam--org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of Org-roam system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (setq path (buffer-file-name (buffer-base-buffer)))
           (minaduki//in-vault? path)))))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
