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
(require 'map)

;;;; Vault objects
(defcustom minaduki/vaults nil
  "A list of vaults that are tracked by Minaduki.

Each vault is either a string, representing the path of the
vault, or a plist with the keys `:name' and `:path', and
optionally `:skipped' and `:main'.

The first vault with `:main' is the main vault.

A vault with a name can be linked to. Right now this is only
supported in Org mode by being added to `org-link-abbrev-alist'.

Files under the paths are indexed by Minaduki.

Each vault is represented with a list (NAME PATH).
Files under any PATH are indexed by Minaduki.
Each NAME is added to `org-link-abbrev-alist'."
  :group 'minaduki
  :type '(repeat
          (choice (directory :tag "Path without other properties")
                  (list :tag "A vault with a name and whether it's skipped"
                        (const :name (string :tag "Name"))
                        (const :path (directory :tag "path"))
                        (const :skipped (boolean :tag "Skipped?"))
                        (const :main (boolean :tag "Main?"))))))

(defun minaduki-vault-path (vault)
  "Return the path of VAULT."
  (if (stringp vault)
      vault
    (plist-get vault :path)))

(defun minaduki-vault-name (vault)
  "Return the name of VAULT."
  (when (listp vault)
    (plist-get vault :name)))

(defun minaduki-vault-skipped (vault)
  "Return whether VAULT is skipped by `minaduki-db:build-cache'."
  (when (listp vault)
    (plist-get vault :skipped)))

(defun minaduki-vault-main? (vault)
  "Return whether VAULT is the main vault."
  (when (listp vault)
    (plist-get vault :main)))

;;;; etc.
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

(defsubst minaduki-vault:excluded? (path)
  "Should PATH be excluded from indexing?"
  (and minaduki-file-exclude-regexp
       (string-match-p minaduki-file-exclude-regexp path)))

;;;; Internal
(defun minaduki-vault::list-files/rg (executable dir)
  "List all tracked files in DIR with Ripgrep.

EXECUTABLE is the Ripgrep executable."
  (let* ((exts minaduki-file-extensions)
         (globs (append
                 (mapcar (lambda (ext) (concat "*." ext)) exts)
                 (mapcar (lambda (ext) (concat "*." ext ".gpg")) exts)
                 (mapcar (lambda (ext) (concat "*." ext ".gz")) exts)))
         (arguments `("-L" ,dir "--files"
                      ,@(cons "-g" (-interpose "-g" globs)))))
    (with-temp-buffer
      (apply #'call-process executable
             nil '(t nil) nil
             arguments)
      (goto-char (point-min))
      (-some--> (buffer-string)
        (s-split "\n" it :omit-nulls)
        (-remove #'minaduki-vault:excluded? it)
        (-map #'f-expand it)))))

(defun minaduki-vault::list-files/elisp (dir)
  "List all tracked files in DIR with `directory-files-recursively'."
  (let ((regexp (rx "."
                    (regexp
                     (mapconcat #'regexp-quote minaduki-file-extensions "\\|"))
                    (opt "." (or "gpg" "gz"))
                    eos))
        result)
    (dolist (file (directory-files-recursively dir regexp nil nil t))
      (when (and (file-readable-p file)
                 (not (minaduki-vault:excluded? file)))
        (push file result)))
    result))

(defun minaduki-vault::search-files (dir regexp)
  "Return files whose names match REGEXP within DIR.
This relies on Fd, which means REGEXP is not Emacs Regular
Expressions, but the syntax defined by the Rust Regex crate."
  (with-temp-buffer
    (let (exts args)
      (dolist (ext minaduki-file-extensions)
        (dolist (variant '("" ".gz" ".gpg"))
          (push (list "-e" (concat ext variant)) exts)))
      ;; -e org -e org.gz -e org.gpg ...
      (setq exts (-flatten exts))
      (setq args `("--print0"
                   ,@exts
                   ,regexp
                   ,(f-expand dir)))
      (apply #'call-process "fd" nil '(t nil) nil args))
    (-some--> (buffer-string)
      (s-split "\0" it :omit-nulls)
      ;; Feeding `minaduki-file-exclude-regexp' to find seems to yield basically
      ;; the same performance
      (-remove #'minaduki-vault:excluded? it))))

(defun minaduki-vault::list-files (dir)
  "List tracked files in DIR.

Uses either Ripgrep or `directory-files-recursively'."
  (let ((cmd (executable-find "rg")))
    (if cmd
        (minaduki-vault::list-files/rg cmd dir)
      (minaduki-vault::list-files/elisp dir))))

(cl-defun minaduki-vault::paths (&key skip)
  "Return the paths of all vaults in `minaduki/vaults'.

If SKIP is non-nil, list only those whose `skipped' prop is nil.

The result is guaranteed to only contain paths."
  (let (ret)
    (dolist (vault minaduki/vaults)
      (let ((path (minaduki-vault-path vault)))
        (when path
          (unless (and skip (minaduki-vault-skipped vault))
            (cl-pushnew path ret :test #'equal)))))
    (when org-directory
      (cl-pushnew org-directory ret :test #'equal))
    (nreverse ret)))

;;;; Other public functions
(defun minaduki-vault:main ()
  "Return the path to the main vault."
  (minaduki-vault-path
   (or (-first #'minaduki-vault-main? minaduki/vaults)
       org-directory
       (car minaduki/vaults))))

(defun minaduki-vault:all-files ()
  "Return a list of all tracked files.

Tracked files are those within one of `minaduki/vaults' that
isn't skipped."
  (let ((paths (minaduki-vault::paths :skip t)))
    (apply #'append
           (--map (minaduki-vault::list-files (expand-file-name it))
                  paths))))

(defun minaduki-vault:in-vault? (&optional path)
  "Return t if PATH is in a vault.

If PATH is not specified, use the current buffer's file-path.

Vaults are those defined in `minaduki/vaults'.

A path is in a vault if it:
- has the right extension,
- is not excluded (by `minaduki-file-exclude-regexp'),
- and is located under a vault."
  (-when-let (path (or path
                       (minaduki::current-file-name)))
    (setq path (expand-file-name path))
    (save-match-data
      (and
       (member (minaduki::file-name-extension path)
               minaduki-file-extensions)
       (not (minaduki-vault:excluded? path))
       (--any? (s-prefix? (expand-file-name it) path)
               (minaduki-vault::paths))))))

;; TODO: return closest registered vault.
;; Currently this just returns the closest nested vault.
(defun minaduki-vault:closest (&optional path)
  "Return the innermost vault that contains PATH."
  (unless path
    (setq path default-directory))
  (let* ((main-vault (minaduki-vault:main))
         (nested-vaults
          ;; These folders are nested vaults no matter what.
          (mapcan (lambda (x)
                    (let ((d (f-join main-vault x)))
                      (when (f-dir? d)
                        (f-directories d))))
                  minaduki-nested-vault-search-path)))
    (catch 'ret
      (while t
        (when (equal (f-expand path)
                     (f-expand main-vault))
          (throw 'ret path))
        (unless (s-starts-with?
                 (f-full main-vault)
                 (f-full path))
          (throw 'ret nil))
        (if (or
             (member path nested-vaults)
             (equal (f-full path)
                    (f-full (minaduki-vault:main)))
             (--any? (f-exists? (f-join path it))
                     minaduki-nested-vault-root-files))
            (throw 'ret path)
          (setq path (f-dirname path)))))))

(defun minaduki-vault:in-obsidian-vault? (&optional path)
  "Is PATH in an Obsidian vault?

If PATH is nil, use `default-directory'."
  (f-exists? (f-join (minaduki-vault:closest path) ".obsidian")))

(defun minaduki-vault:path-relative (path &optional vault)
  "Return PATH's relative path in VAULT.

If VAULT is not given, use the main vault."
  (let ((vault-path
         (f-expand (minaduki-vault-path (or vault (minaduki-vault:main))))))
    ;; A short common parent means forcing PATH to be relative to vault-path
    ;; might just end up pointing all the way back to root. Leave it as-is in
    ;; this case.
    ;;
    ;; We don't test "equals '/'" or "has zero element when split with path
    ;; separator" because I don't think that would work on Windows.
    ;; `f-common-parent' would be more accurate, but it's too slow. We just test
    ;; that the first 3 characters are the same.
    (if (equal (substring path 0 (min (length path) 3))
               (substring vault-path 0 (min (length vault-path) 3)))
        (f-relative path vault-path)
      path)))

(defun minaduki-vault:path-absolute (path vault-name)
  "Given PATH relative to VAULT-NAME, return its absolute path.

VAULT-NAME is a name in `minaduki/vaults'."
  (let ((dir
         (--> minaduki/vaults
              (--first (equal vault-name (minaduki-vault-name it)) it)
              minaduki-vault-path)))
    (expand-file-name path dir)))

(defun minaduki-vault:obsidian-vault? (&optional vault)
  "Return if VAULT is an Obsidian vault.
VAULT is the result of `minaduki-vault:closest' by default."
  (let ((vault (or vault (minaduki-vault:closest))))
    (f-dir-p (f-join vault ".obsidian"))))

(defun minaduki-vault:obsidian:get-prop (namespace prop &optional vault)
  "Get VAULT's Obsidian configuration PROP from NAMESPACE.
This reads PROP from .obsidian/NAMESPACE.json.
VAULT is computed from `minaduki-vault:closest' if not given."
  (cl-block nil
    (let* ((vault (or vault (minaduki-vault:closest)))
           (json-file (f-join vault
                              ".obsidian"
                              (format "%s.json" namespace))))
      (unless (f-file? json-file)
        (cl-return))
      (with-temp-buffer
        (insert-file-contents json-file)
        (gethash prop (json-parse-buffer))))))

;; When following, if the path isn't just a basename, then it's
;; treated as a normal local-absolute path.
;;
;; If it is just a basename, if there is only one file with that
;; basename, it visits that; otherwise return the candidate that is
;; closest to the current path.
(defun minaduki-obsidian-path (written)
  "Convert WRITTEN path to actual path."
  (cl-block nil
    (let ((closest-vault (minaduki-vault:closest))
          obsidian?
          files)
      ;; Early return if we're not in a vault
      (unless closest-vault
        (cl-return))
      (setq obsidian? (minaduki-vault:obsidian-vault? closest-vault))
      ;; Not just a base name -> just a local-absolute path
      (when
          (or
           ;; Not just a base name
           (s-contains? (f-path-separator) written)
           ;; Has a protocol / abbrev
           (s-contains? ":" written))
        ;; Support org link abbrev in wiki links outside of an
        ;; obsidian vault
        (unless obsidian?
          (setq written (org-link-expand-abbrev written)))
        (cl-return (f-join closest-vault written)))
      (setq files
            ;; XXX: this means `written' is interpreted as a regexp. Ideally
            ;; `regexp-quote' would work, but it's for Elisp regexps while Fd
            ;; wants Rust Regex crate expressions. (We're using Fd for speed.)
            (minaduki-vault::search-files
             closest-vault
             (format "^(%s)(\\..*)?$" written)))
      (cond
       ((= 0 (length files))
        (let ((attachment nil))
          (when obsidian?
            (setq attachment
                  (-some--> (minaduki-vault:obsidian:get-prop
                             "app" "attachmentFolderPath"
                             closest-vault)
                    (f-join it written))))
          (if (and attachment (f-file? attachment))
              attachment
            (substring-no-properties written))))
       ((= 1 (length files))
        (car files))
       (t (--max-by
           (let ((here (f-split default-directory)))
             (>
              (length (-common-prefix (f-split it) here))
              (length (-common-prefix (f-split other) here))))
           files))))))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
