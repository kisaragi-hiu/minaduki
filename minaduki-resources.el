;;; minaduki-resources.el --- Resources -*- lexical-binding: t -*-

;;; Commentary:

;; Resources are per-vault keywords.
;; They are specified in the config file.

;;; Code:

(require 'minaduki-vault)

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
          (push (cons (format "%s" key)
                      (f-slash
                       (expand-file-name path vault-path)))
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
