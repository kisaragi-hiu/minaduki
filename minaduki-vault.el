;;; minaduki-vault.el --- Vault logic -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'minaduki-utils)
(require 'minaduki-vars)

(require 'f)
(require 's)

(defcustom minaduki-nested-vault-root-files
  (list ".obsidian"
        "config.yaml")
  "Files that mark a nested vault."
  :group 'minaduki
  :type '(repeat string))

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

(defun minaduki//closest-vault (&optional path)
  "Return the innermost vault that contains PATH."
  (unless path
    (setq path default-directory))
  (catch 'ret
    (while t
      (when (f-same? path org-directory)
        (throw 'ret path))
      (when (not (s-starts-with?
                  (f-full org-directory)
                  (f-full path)))
        (throw 'ret nil))
      (if (--any? (f-exists? (f-join path it))
                  minaduki-nested-vault-root-files)
          (throw 'ret path)
        (setq path (f-dirname path))))))

(defun minaduki//vault-path (path)
  "Return PATH's relative path in the current vault."
  (f-relative path (f-expand org-directory)))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
