;;; minaduki-vault.el --- Vault logic -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'minaduki-utils)
(require 'minaduki-vars)

(require 'org)

(require 'f)
(require 's)
(require 'dash)

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

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (append
   (mapcar (lambda (ext) (concat "*." ext)) exts)
   (mapcar (lambda (ext) (concat "*." ext ".gpg")) exts)))

(defun minaduki//list-files/rg (executable dir)
  "List all tracked files in DIR with Ripgrep.

EXECUTABLE is the Ripgrep executable."
  (let* ((globs (org-roam--list-files-search-globs minaduki-file-extensions))
         (arguments `("-L" ,dir "--files"
                      ,@(cons "-g" (-interpose "-g" globs)))))
    (with-temp-buffer
      (apply #'call-process executable
             nil '(t nil) nil
             arguments)
      (goto-char (point-min))
      (-some--> (buffer-string)
        (s-split "\n" it :omit-nulls)
        (-remove #'minaduki//excluded? it)
        (-map #'f-expand it)))))

(defun minaduki//list-files/elisp (dir)
  "List all tracked files in DIR with `directory-files-recursively'."
  (let ((regexp (concat "\\.\\(?:"
                        (mapconcat #'regexp-quote minaduki-file-extensions "\\|")
                        "\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (directory-files-recursively dir regexp nil nil t))
      (when (and (file-readable-p file)
                 (not (minaduki//excluded? file)))
        (push file result)))
    result))

(defun minaduki//list-files (dir)
  "List tracked files in DIR.

Uses either Ripgrep or `directory-files-recursively'."
  (let ((cmd (executable-find "rg")))
    (if cmd
        (minaduki//list-files/rg cmd dir)
      (minaduki//list-files/elisp dir))))

(defun minaduki//list-all-files ()
  "Return a list of all tracked files within `org-directory'."
  (minaduki//list-files (expand-file-name org-directory)))

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
       (or (f-descendant-of? path (expand-file-name org-directory))
           (--any? (f-descendant-of? path it)
                   (mapcar #'cdr minaduki/vaults)))))))

(defun minaduki//closest-vault (&optional path)
  "Return the innermost vault that contains PATH."
  (unless path
    (setq path default-directory))
  (let ((nested-vaults
         ;; These folders are nested vaults no matter what.
         (mapcan (lambda (x)
                   (let ((d (f-join org-directory x)))
                     (when (f-dir? d)
                       (f-directories d))))
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
  (let ((closest-vault (minaduki//closest-vault)))
    (if (s-contains? (f-path-separator) written)
        ;; Not just a base name -> just a local-absolute path
        (f-join closest-vault written)
      (let ((files (->> (minaduki//list-files closest-vault)
                        (--filter
                         (let ((f (f-filename it)))
                           (or (equal written f)
                               (equal written (file-name-sans-extension f))))))))
        (when files
          (if (= 1 (length files))
              (car files)
            (--max-by
             (let ((here (f-split default-directory)))
               (>
                (length (-common-prefix (f-split it) here))
                (length (-common-prefix (f-split other) here))))
             files)))))))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
