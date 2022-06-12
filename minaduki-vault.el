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

(defcustom minaduki-nested-vault-search-path
  (list "external")
  "Folders below these paths in the vault are nested vaults.

For example, when \"external\" is part of this list,
<vault>/external/a and <vault>/external/b are both considered
nested vaults regardless of whether they contain
`minaduki-nested-vault-root-files'."
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
  (let ((nested-vaults
         ;; These folders are nested vaults no matter what.
         (mapcan (lambda (x) (f-directories (f-join org-directory x)))
                 minaduki-nested-vault-search-path)))
    (catch 'ret
      (while t
        (when (f-same? path org-directory)
          (throw 'ret path))
        (when (not (s-starts-with?
                    (f-full org-directory)
                    (f-full path)))
          (throw 'ret nil))
        (if (or
             (member path nested-vaults)
             (--any? (f-exists? (f-join path it))
                     minaduki-nested-vault-root-files))
            (throw 'ret path)
          (setq path (f-dirname path)))))))

(defun minaduki--in-obsidian-vault? (&optional path)
  "Is PATH in an Obsidian vault?

If PATH is nil, use `default-directory'."
  (f-exists? (f-join (minaduki//closest-vault path) ".obsidian")))

(defun minaduki//vault-path (path)
  "Return PATH's relative path in the current vault."
  (f-relative path (f-expand org-directory)))

;; When following, if the path isn't just a basename, then it's
;; treated as a normal local-absolute path.
;;
;; If it is just a basename, if there is only one file with that
;; basename, it visits that; otherwise return the candidate that is
;; closest to the current path.
(defun minaduki-obsidian-path (written)
  "Convert WRITTEN path to actual path."
  (let ((split (f-split written))
        (closest-vault (minaduki//closest-vault)))
    (if (> (length split) 1)
        ;; Not just a base name -> just a local-absolute path
        (f-join closest-vault written)
      (let ((files (->> (minaduki//list-files closest-vault)
                        (--filter (or (equal (f-filename it) written)
                                      (equal (f-base it) written))))))
        (when files
          (if (= 1 (length files))
              (car files)
            (--max-by
             (length (f-common-parent (list it default-directory)))
             files)))))))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
