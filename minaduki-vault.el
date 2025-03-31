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

(require 'yaml)

;;;; Vault objects
(defvar minaduki/vaults nil
  "A list of vaults that are tracked by Minaduki.

Do not set this variable directly; vaults are saved/loaded from
`minaduki-vaults-file' instead. If you must use a variable, set
it in `minaduki-vaults-extra' instead, which will be merged into
this variable.

Each vault is either a string, representing the path of the
vault, or a plist with the keys `:name' and `:path', and
optionally `:skipped' and `:main'.

The first vault with `:main' is the main vault.

A vault with a name can be linked to. Right now this is only
supported in Org mode by being added to `org-link-abbrev-alist'.

Files under the paths are indexed by Minaduki.

Each vault is represented with a list (NAME PATH).
Files under any PATH are indexed by Minaduki.
Each NAME is added to `org-link-abbrev-alist'.")

(defcustom minaduki-vaults-extra nil
  "A list of extra vaults, on top of those declared in `minaduki-vaults-file'.

Each vault is either a string, representing the path of the
vault, or a plist with the keys `:name' and `:path', and
optionally `:skipped' and `:main'.

The first vault with `:main' is the main vault.

A vault with a name can be linked to. Right now this is only
supported in Org mode by being added to `org-link-abbrev-alist'.

Files under the paths are indexed by Minaduki.

Note that paths should be passed in as the same form Emacs would
visit them. For performance, Minaduki does not resolve symlinks,
so if you use `find-file-visit-truename' you would have to make
sure vault paths are also truenames.

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

;; Somewhat inspired by projectile-known-projects and how that's handled
(defcustom minaduki-vaults-file
  (expand-file-name "minaduki-vaults.json" user-emacs-directory)
  "Path to the vaults file.
This file is used to declare or register known vaults.

This needs to be set before enabling `minaduki-mode', or
`minaduki-vaults-load' has to be run afterwards for it to take
effect."
  :type 'string
  :group 'minaduki)

(defun minaduki-vaults-save ()
  "Save `minaduki/vaults' into `minaduki-vaults-file'."
  (cl-letf (((symbol-function 'json-alist-p)
             ;; `minaduki/vaults' is a simple list of plists; unfortunately it
             ;; also looks like an alist and gets treated as an alist first.
             ;; This overrides that behavior.
             (lambda (_list)
               nil)))
    ;; This is meant to be able to be hand-edited, as well as being diff-able. So
    ;; I want it to be pretty-printed JSON.
    (let ((json-encoding-pretty-print t)
          (json-encoding-default-indentation " ")
          (json-encoding-object-sort-predicate #'string<))
      (let* ((extra-paths (mapcar #'minaduki-vault-path minaduki-vaults-extra))
             (non-extra (--remove (member (minaduki-vault-path it) extra-paths)
                                  minaduki/vaults))
             (json (json-encode non-extra)))
        (with-temp-file minaduki-vaults-file
          (insert json))))))

(defun minaduki-vaults-load ()
  "Load vaults from `minaduki-vaults-file' into `minaduki/vaults'.
This also loads from the `minaduki-vaults-extra' variable.
If there are errors on loading, nothing will be loaded from
`minaduki-vaults-file'."
  (let ((json-object-type 'plist)
        (json-array-type 'list))
    (setq minaduki/vaults
          (append (ignore-errors
                    (json-read-file minaduki-vaults-file))
                  minaduki-vaults-extra))))

(define-minor-mode minaduki-vaults-save-load-mode
  "Minor mode to load minaduki vaults, and save it when appropriate.
If ARG is a number less than 1, disable it, otherwise enable it."
  :global t :lighter nil :group 'minaduki
  (cond (minaduki-vaults-save-load-mode ; enable
         (add-hook 'kill-emacs-hook #'minaduki-vaults-save)
         (minaduki-vaults-load))
        (t ; disable
         (minaduki-vaults-save)
         (remove-hook 'kill-emacs-hook #'minaduki-vaults-save))))

(defun minaduki-vault-config (vault)
  "Get the config object for VAULT."
  (let* ((path (minaduki-vault-path vault))
         (config-path (f-join path ".minaduki.yml")))
    (when (file-regular-p config-path)
      (yaml-parse-string
       (with-temp-buffer
         (insert-file-contents config-path)
         (buffer-string))))))

(defun minaduki-vault-resources (vault)
  "Get registered resources for VAULT.
Resources are used to set up custom link types via
`org-link-abbrev-alist'.

A resource can have the following properties:
- \"path\": a path value resolved according to
  `minaduki-vault-config--path'. If the result is relative, it is
  relative to the root path of VAULT."
  (let* ((config (minaduki-vault-config vault))
         (vault-path (minaduki-vault-path vault))
         (resources (map-elt config 'resources))
         ret)
    (->>
     resources
     (maphash
      (lambda (key resource)
        (when-let (path (minaduki-vault-config--path
                         (map-elt resource 'path)))
          (push (cons (format "%s" key)
                      (f-slash
                       (expand-file-name path vault-path)))
                ret)))))
    ret))

(defun minaduki-vault-config--path (value)
  "Resolve path VALUE.
VALUE can be:
- a string, which is used directly, or
- a hash table. There are two modes:
  1. with a `join' key, its value is a list of path
     values (passed through this function).
  2. with a `variable' key, its value is a string naming a
     variable whose value is resolved with `symbol-value' and
     coerced to a string.
  If the hash table has both, `variable' has no effect."
  (cl-block nil
    (when (stringp value)
      (cl-return value))
    (when (not (hash-table-p value))
      (cl-return nil))
    (when-let (join (map-elt value 'join))
      (cl-return
       (apply #'f-join (mapcar #'minaduki-vault-config--path join))))
    (when-let (variable (map-elt value 'variable))
      (let ((resolved (ignore-errors
                        (symbol-value
                         (intern variable)))))
        (cond ((not resolved) nil)
              ((stringp resolved) resolved)
              (t (format "%s" resolved)))))))

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

(defsubst minaduki-vault-excluded? (path)
  "Should PATH be excluded from indexing?"
  (and minaduki-file-exclude-regexp
       (string-match-p minaduki-file-exclude-regexp path)))

;;;; Internal
(defun minaduki-vault--list-files/rg (executable dir)
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
        (-remove #'minaduki-vault-excluded? it)
        (-map #'f-expand it)))))

(defun minaduki-vault--list-files/elisp (dir)
  "List all tracked files in DIR with `directory-files-recursively'."
  (let ((regexp (rx "."
                    (regexp
                     (mapconcat #'regexp-quote minaduki-file-extensions "\\|"))
                    (opt "." (or "gpg" "gz"))
                    eos))
        result)
    (dolist (file (directory-files-recursively dir regexp nil nil t))
      (when (and (file-readable-p file)
                 (not (minaduki-vault-excluded? file)))
        (push file result)))
    result))

(defun minaduki-vault--search-files (dir regexp &optional case-sensitive)
  "Return files whose names match REGEXP within DIR.
This relies on Fd, which means REGEXP is not Emacs Regular
Expressions, but the syntax defined by the Rust Regex crate.

If CASE-SENSITIVE is non-nil, do a case sensitive search,
otherwise (by default) tell Fd to ignore case."
  (with-temp-buffer
    (let (exts args)
      (dolist (ext minaduki-file-extensions)
        (dolist (variant '("" ".gz" ".gpg"))
          (push (list "-e" (concat ext variant)) exts)))
      ;; -e org -e org.gz -e org.gpg ...
      (setq exts (-flatten exts))
      (setq args `("--print0"
                   ,@(unless case-sensitive
                       '("--ignore-case"))
                   ,@exts
                   ,regexp
                   ,(f-expand dir)))
      (apply #'call-process "fd" nil '(t nil) nil args))
    (-some--> (buffer-string)
      (s-split "\0" it :omit-nulls)
      ;; Feeding `minaduki-file-exclude-regexp' to find seems to yield basically
      ;; the same performance
      (-remove #'minaduki-vault-excluded? it))))

(defun minaduki-vault--list-files (dir)
  "List tracked files in DIR.

Uses either Ripgrep or `directory-files-recursively'."
  (let ((cmd (executable-find "rg")))
    (if cmd
        (minaduki-vault--list-files/rg cmd dir)
      (minaduki-vault--list-files/elisp dir))))

(cl-defun minaduki-vault--abbrev-alist (&key skip)
  "Return vaults in the same form as `org-link-abbrev-alist'.

In other words, as a list of (NAME . PATH) pairs.

If SKIP is non-nil, list only those whose `skipped' prop is nil.
Vaults without a name or path are always skipped."
  (let (ret)
    (dolist (vault minaduki/vaults)
      (let ((path (-some-> (minaduki-vault-path vault)
                    expand-file-name))
            (name (minaduki-vault-name vault)))
        (when (and path name)
          (unless (and skip (minaduki-vault-skipped vault))
            (push (cons name path) ret)))))
    (nreverse ret)))

(cl-defun minaduki-vault--paths (&key skip)
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
(defun minaduki-vault-main ()
  "Return the path to the main vault."
  (minaduki-vault-path
   (or (-first #'minaduki-vault-main? minaduki/vaults)
       org-directory
       (car minaduki/vaults))))

(defun minaduki-vault-all-files ()
  "Return a list of all tracked files.

Tracked files are those within one of `minaduki/vaults' that
isn't skipped."
  (let ((paths (minaduki-vault--paths :skip t)))
    (apply #'append
           (--map (minaduki-vault--list-files (expand-file-name it))
                  paths))))

(defun minaduki-vault-in-vault? (&optional path)
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
       (not (minaduki-vault-excluded? path))
       (--any? (s-prefix? (expand-file-name it) path)
               (minaduki-vault--paths))))))

(defun minaduki-vault-closest (&optional path)
  "Return the innermost vault that contains PATH."
  (unless path
    (setq path default-directory))
  (cl-block nil
    (dolist (vault-path (minaduki-vault--paths :skip t))
      (let* ((nested-vaults
              ;; These folders are nested vaults no matter what.
              (mapcan (lambda (x)
                        (let ((d (f-join vault-path x)))
                          (when (f-dir? d)
                            (f-directories d))))
                      minaduki-nested-vault-search-path))
             (result (cl-block nil
                       (let ((path path)) ; don't actually consume the outer reference
                         (while path ; traverse up until worst case we hit root
                           ;; path = vault-path: path belongs in vault-path
                           (when (equal (f-expand path)
                                        (f-expand vault-path))
                             (cl-return path))
                           ;; path doesn't start with vault-path: path cannot be
                           ;; in vault path
                           (unless (s-starts-with?
                                    (f-full vault-path)
                                    (f-full path))
                             (cl-return nil))
                           ;; in these cases path is the vault we want
                           (when (or (member path nested-vaults)
                                     (equal (f-full path)
                                            (f-full vault-path))
                                     (--any? (f-exists? (f-join path it))
                                             minaduki-nested-vault-root-files))
                             (cl-return path))
                           ;; nothing matches, traverse up and try again
                           (setq path (f-dirname path)))))))
        ;; if we've found it, stop here and return it
        ;; if not, try the next one
        (when result
          (cl-return result))))))

(defvar minaduki-vault-path-abbrev--cache nil
  "Used to store cache for `minaduki-vault-path-abbrev'.")
(defun minaduki-vault-path-abbrev (path &optional use-cache)
  "Abbreviate PATH to one of the vaults.

This function can be called *very* often, like in
`minaduki--format-node'. To help with that, if USE-CACHE is
non-nil then `minaduki-vault-path-abbrev--cache' is used as a
cache for values reused across calls within e.g. a loop, until
the next time USE-CACHE is nil again."
  (cl-block nil
    ;; In non-cached mode, clear it if it's initialized
    (unless use-cache
      (when minaduki-vault-path-abbrev--cache
        (setq minaduki-vault-path-abbrev--cache nil)))
    (let (vault-abbrev-alist value)
      (when use-cache
        (setq vault-abbrev-alist minaduki-vault-path-abbrev--cache))
      (unless vault-abbrev-alist
        (setq vault-abbrev-alist (minaduki-vault--abbrev-alist :skip t)))
      (when use-cache
        (setq minaduki-vault-path-abbrev--cache vault-abbrev-alist))
      (minaduki::apply-link-abbrev path vault-abbrev-alist :dont))))

(defvar minaduki-vault-path-relative--cache nil
  "Used to store cache for `minaduki-vault-path-relative'.")
(defun minaduki-vault-path-relative (path &optional vault use-cache)
  "Return PATH's relative path in VAULT.

If VAULT is not given, use the main vault.

This function can be called *very* often, like in
`minaduki--format-node'. To help with that, if USE-CACHE is
non-nil then calls are cached into
`minaduki-vault-path-relative--cache' (as a hash table) until the
next time USE-CACHE is nil again."
  (cl-block nil
    ;; In cached mode, if the cache hasn't been initialized, initialize it.
    ;; In non-cached mode, clear it if it's initialized
    (if use-cache
        (unless minaduki-vault-path-relative--cache
          (setq minaduki-vault-path-relative--cache (make-hash-table :test #'equal)))
      (when minaduki-vault-path-relative--cache
        (setq minaduki-vault-path-relative--cache nil)))
    (let ((vault-path (f-expand (minaduki-vault-path
                                 (or vault (minaduki-vault-main)))))
          (key (and use-cache (list path vault)))
          value)
      (when use-cache
        (when-let (cached-value (gethash key minaduki-vault-path-relative--cache))
          (cl-return cached-value)))
      ;; A short common parent means forcing PATH to be relative to vault-path
      ;; might just end up pointing all the way back to root. Leave it as-is in
      ;; this case.
      ;;
      ;; We don't test "equals '/'" or "has zero element when split with path separator"
      ;; because I don't think that would work on Windows.
      ;; `f-common-parent' would be more accurate, but it's too slow. We just
      ;; test that the first 3 characters are the same.
      (setq value (if (equal (substring path 0 (min (length path) 3))
                             (substring vault-path 0 (min (length vault-path) 3)))
                      (f-relative path vault-path)
                    path))
      (when use-cache
        (puthash key value minaduki-vault-path-relative--cache))
      value)))

(defun minaduki-vault-path-absolute (path &optional vault-name)
  "Given PATH relative to VAULT-NAME, return its absolute path.

VAULT-NAME is a name in `minaduki/vaults'. If nil, use the
current closest vault, or the main vault if there isn't one."
  (let ((dir
         (or (and
              vault-name
              (--> minaduki/vaults
                   (--first (equal vault-name (minaduki-vault-name it)) it)
                   minaduki-vault-path))
             (minaduki-vault-closest)
             (minaduki-vault-main))))
    (expand-file-name path dir)))

(defun minaduki-vault-obsidian-vault? (&optional vault)
  "Return if VAULT is an Obsidian vault.
VAULT is the result of `minaduki-vault-closest' by default."
  (let ((vault (or vault (minaduki-vault-closest))))
    (f-dir-p (f-join vault ".obsidian"))))

(defun minaduki-vault-obsidian-get-prop (namespace prop &optional vault)
  "Get VAULT's Obsidian configuration PROP from NAMESPACE.
This reads PROP from .obsidian/NAMESPACE.json.
VAULT is computed from `minaduki-vault-closest' if not given."
  (cl-block nil
    (let* ((vault (or vault (minaduki-vault-closest)))
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
    (let ((closest-vault (minaduki-vault-closest))
          obsidian?
          files)
      ;; Early return if we're not in a vault
      (unless closest-vault
        (cl-return))
      (setq obsidian? (minaduki-vault-obsidian-vault? closest-vault))
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
            (minaduki-vault--search-files
             closest-vault
             (format "^(%s)(\\..*)?$" written)))
      (cond
       ((= 0 (length files))
        (let ((attachment nil))
          (when obsidian?
            (setq attachment
                  (-some--> (minaduki-vault-obsidian-get-prop
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

(defun minaduki-vault-register (directory &optional message?)
  "Register DIRECTORY into `minaduki-vaults-file' for indexing.
If MESSAGE? is non-nil, display a message after registering."
  (interactive
   (list
    (read-directory-name "Register directory as vault: "
                         nil nil t)
    t))
  (unless (--any? (f-equal? directory
                            (minaduki-vault-path it))
                  minaduki/vaults)
    (push
     `(:name ,(f-base directory)
       :path ,directory)
     minaduki/vaults)
    (minaduki-vaults-save)
    (when message?
      (message "%s has been registered into %s" directory minaduki-vaults-file))))

(defun minaduki-vault-create (directory)
  "Create a vault in DIRECTORY."
  (interactive
   (list (read-directory-name "New vault path: ")))
  (let ((directory (f-expand directory)))
    (cl-block init
      (when (and (f-dir? directory)
                 (not (directory-empty-p directory)))
        (minaduki::message "%s already has content, creation skipped; registering"
                           directory)
        (cl-return-from init))
      (make-directory directory t)
      (call-process "git" nil nil nil "init" directory))
    (minaduki-vault-register directory)))

(provide 'minaduki-vault)

;;; minaduki-vault.el ends here
