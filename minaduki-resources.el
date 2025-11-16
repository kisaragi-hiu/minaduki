;;; minaduki-resources.el --- Resources -*- lexical-binding: t -*-

;;; Commentary:

;; Resources are per-vault keywords.
;; They are specified in the config file.

;;; Code:

(require 'minaduki-vault)

(defvar minaduki-file-finders-cache-vars nil
  "List of symbols to caches of file finders that have been defined.")

(defun minaduki-file-finders-reset-cache (&rest caches)
  "Reset CACHES of file finders.

Interactively, prompt for a cache variable from
`minaduki-file-finders-cache-vars' to reset; with a
\\[universal-argument], reset all caches in that list."
  (interactive
   (if current-prefix-arg
       minaduki-file-finders-cache-vars
     (list
      (intern
       (completing-read
        "File finder caches to reset: "
        minaduki-file-finders-cache-vars)))))
  (dolist (cache caches)
    (set cache nil)))

(cl-defun minaduki-define-file-finder
    (&key key vault directory template match-type sort-pred split-path extra-args)
  "Define a file finder suitable for use in `org-link-abbrev-alist'.

The function name is composed from KEY and the name of VAULT.

The file finder receives a TAG argument, and searches DIRECTORY
for the first file whose name matches it.

TEMPLATE is expanded with \"%s\" replaced with TAG using `format'; this
allows for better control over what \"matches it\" means.

MATCH-TYPE determines how the result of TEMPLATE filled in with
TAG is passed to `find'. It should not be quoted. When it's
`regex' (default), TAG will be quoted with `regexp-quote';
otherwise this should be a symbol specifying a pattern matching
argument, for example `name' for \"-name\", `iname' for
\"-iname\", and so on. nil also means `regex'.

SORT-PRED, if non-nil, is the predicate used to sort matching
results. For example, use `string<' to select favor the
\"smaller\" string when there are multiple results.

SPLIT-PATH, if non-nil, means to only try to find TAG up to the
first slash character. If it matches, the rest of TAG will be
appended back and returned.

EXTRA-ARGS, if non-nil, is a list of arguments to pass to `find'
before the matching options. Put `-maxdepth' here, for example."
  (let* ((name (intern (format "minaduki-file-finders--%s-%s"
                               (minaduki-vault-path vault)
                               key)))
         (cache-sym (intern (format "%s--cache" (symbol-name name)))))
    ;; HACK: unfortunately the function handlers in org-link-abbrev-alist *have
    ;; to* be symbols, they test symbolp and not functionp. So we have to do
    ;; defun at runtime with information we only get at runtime, then send the
    ;; symbols through. This means the eval is unavoidable.
    (eval `(let* ((split-path ',split-path)
                  (match-type (or ',match-type 'regex))
                  (match-args (pcase match-type
                                ('regex '("-regextype" "emacs" "-regex"))
                                ('nil nil)
                                (_ (list (concat "-" (symbol-name match-type))))))
                  (extra-args ,extra-args))
             (defvar ,cache-sym nil)
             (cl-pushnew ',cache-sym minaduki-file-finders-cache-vars)
             (defun ,name (tag)
               "File finder for finding files based on TAG."
               (let ((rest ""))
                 (when split-path
                   `((when-let ((first-slash-index (cl-position ?/ tag)))
                       (setq rest (substring tag (1+ first-slash-index))
                             tag (substring tag 0 first-slash-index)))))
                 (when (eq match-type 'regex)
                   (setq tag (regexp-quote tag)))
                 ;; (message "rest: %s, tag: %s" rest tag)
                 (or (cdr (assoc tag ,cache-sym))
                     (-some--> ,directory
                       (with-temp-buffer
                         (apply
                          #'call-process
                          find-program nil '(t nil) nil
                          it
                          `(
                            ;; > The regular expressions understood by find
                            ;; > are by default Emacs Regular Expressions
                            ;; > (except that `.' matches newline)
                            ;; -- man page for find
                            ,@extra-args
                            ;; -regextype emacs -regex if MATCH-TYPE is
                            ;; `regex', or:
                            ;; -name if it's `name', -iname if it's
                            ;; `iname', etc.
                            ,@match-args
                            ,(format ,template tag)))
                         (s-split "\n" (buffer-string) :omit))
                       (cond ((= (length it) 1) it)
                             (,sort-pred (sort it ,sort-pred))
                             (t it))
                       car
                       (f-join it rest)
                       (setf (alist-get tag ,cache-sym nil nil #'equal) it))
                     ;; I don't want this to error out or return a nil
                     "/tmp/filenotfound")))
             (put ',name 'k/file-finders-abbrev-path ,directory)
             ;; return the defined symbol
             ',name)
          t)))

(defun minaduki-vault-resources (vault)
  "Get registered resources for VAULT.
Resources are used to set up custom link types via
`org-link-abbrev-alist'.

A resource can have the following properties:
- \"path\" (necessary): a path value resolved according to
  `minaduki-resources--path'. If the result is relative, it is
  relative to the root path of VAULT.
- \"regexp\" (optional): make this a file finder."
  (let* ((config (minaduki-vault-config vault))
         (vault-path (minaduki-vault-path vault))
         (resources (map-elt config 'resources))
         ret)
    (when resources
      ;; unlike a maphash this puts the hash table before the body.
      ;; this does expand to a maphash.
      (cl-loop
       for key being the hash-keys of resources using (hash-values resource)
       do (when-let (path (minaduki-resources--path
                           (map-elt resource 'path)))
            (let ((regexp
                   (-some--> (map-elt resource 'regexp)
                     ;; this eval is to allow writing rx.
                     ;; ...this is not a great idea.
                     (eval (read it) t))))
              (push
               (cons (format "%s" key)
                     (if regexp
                         ;; file finder
                         (minaduki-define-file-finder
                          :key key
                          :vault vault
                          :directory path
                          :template regexp
                          :match-type (-some-> (map-elt resource 'match-type)
                                        intern)
                          :sort-pred (-some--> (map-elt resource 'sort-pred)
                                       ;; also not a great idea. This is to eval a lambda.
                                       (eval (read it) t))
                          :split-path (map-elt resource 'split-path)
                          :extra-args (map-elt resource 'extra-args))
                       ;; simple replacement
                       (f-slash
                        (expand-file-name path vault-path))))
               ret)))))
    ret))

(defun minaduki-resources--path (value)
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
       (apply #'f-join (mapcar #'minaduki-resources--path join))))
    (when-let (variable (map-elt value 'variable))
      (let ((resolved (ignore-errors
                        (symbol-value
                         (intern variable)))))
        (cond ((not resolved) nil)
              ((stringp resolved) resolved)
              (t (format "%s" resolved)))))))

(provide 'minaduki-resources)

;;; minaduki-resources.el ends here
