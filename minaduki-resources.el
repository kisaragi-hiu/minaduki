;;; minaduki-resources.el --- Resources -*- lexical-binding: t -*-

;;; Commentary:

;; Resources are per-vault keywords.
;; They are specified in the config file.

;;; Code:

(require 'minaduki-vault)

;; TODO: caching. We can't just use cache vars.
;; (defvar minaduki-file-finders-cache-vars nil
;;   "List of symbols to caches of file finders that have been defined.")
;;
;; (defun minaduki-file-finders-reset-cache (&rest caches)
;;   "Reset CACHES of file finders.
;;
;; Interactively, prompt for a cache variable from
;; `minaduki-file-finders-cache-vars' to reset; with a
;; \\[universal-argument], reset all caches in that list."
;;   (interactive
;;    (if current-prefix-arg
;;        minaduki-file-finders-cache-vars
;;      (list
;;       (intern
;;        (completing-read
;;         "File finder caches to reset: "
;;         minaduki-file-finders-cache-vars)))))
;;   (dolist (cache caches)
;;     (set cache nil)))

(cl-defun minaduki-make-file-finder
    (&key directory template (match-type 'regex) sort-pred split-path extra-args)
  "Create a file finder function, suitable for use in `org-link-abbrev-alist'.

The file finder receives a TAG argument, and searches DIRECTORY
for the first file whose name matches it.

TEMPLATE is expanded with \"%s\" replaced with TAG (using
`format'); this allows for better control over what \"matches
it\" means.

MATCH-TYPE determines how the result of TEMPLATE filled in with
TAG is passed to `find'. When it's `regex' (default), TAG will be
quoted with `regexp-quote'; otherwise this should be a symbol
specifying a pattern matching argument, for example `name' for
\"-name\", `iname' for \"-iname\", and so on.

SORT-PRED, if non-nil, is the predicate used to sort matching
results. For example, use `string<' to select favor the
\"smaller\" string when there are multiple results.

SPLIT-PATH, if non-nil, means to only try to find TAG up to the
first slash character. If it matches, the rest of TAG will be
appended back and returned.

EXTRA-ARGS, if non-nil, is a list of arguments to pass to `find'
before the matching options. Put `-maxdepth' here, for example."
  (lambda (tag)
    (let ((rest "")
          (match-args
           (pcase match-type
             ('regex '("-regextype" "emacs" "-regex"))
             (_ (list (concat "-" (symbol-name match-type))))))
          (tag
           (pcase match-type
             ('regex (regexp-quote tag))
             (_ tag))))
      (when split-path
        (when-let ((first-slash-index (cl-position ?/ tag)))
          (setq rest (substring tag (1+ first-slash-index))
                tag (substring tag 0 first-slash-index))))
      (or (-some--> directory
            (with-temp-buffer
              (apply #'call-process find-program nil '(t nil) nil
                     it
                     ;; "The regular expressions understood by find
                     ;; are by default Emacs Regular Expressions
                     ;; (except that `.' matches newline)"
                     ;; -- man page for find
                     `(,@extra-args
                       ;; -regextype emacs -regex if MATCH-TYPE is
                       ;; `regex', or:
                       ;; -name if it's `name', -iname if it's
                       ;; `iname', etc.
                       ,@match-args
                       ,(format
                         template
                         ;; quoted above
                         tag)))
              (s-split "\n" (buffer-string) :omit))
            (cond ((= (length it) 1) it)
                  (sort-pred (sort it sort-pred))
                  (t it))
            car
            (f-join it rest))
          ;; I don't want this to error out or return a nil
          "/tmp/filenotfound"))))

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
        (when-let (path (minaduki-resources--path
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
                       (minaduki-make-file-finder
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
             ret))))))
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
       (apply #'f-join (mapcar #'minaduki-vault-config--path join))))
    (when-let (variable (map-elt value 'variable))
      (let ((resolved (ignore-errors
                        (symbol-value
                         (intern variable)))))
        (cond ((not resolved) nil)
              ((stringp resolved) resolved)
              (t (format "%s" resolved)))))))

(provide 'minaduki-resources)

;;; minaduki-resources.el ends here
