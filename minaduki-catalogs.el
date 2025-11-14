;;; minaduki-catalogs.el --- Catalogs -*- lexical-binding: t -*-

;;; Commentary:

;; Support functions for catalogs.
;; TODO: feed "the directory of the current catalog file" to template creation
;; so that when creating corresponding entries they are created there

;;; Code:

(require 'minaduki-vault)

(defun minaduki-vault-catalogs (vault)
  "Return the catalogs of VAULT."
  (minaduki-vault-config-load vault "catalogs"))

(defun minaduki-catalogs-put-catalog (table name file dir)
  "Put a catalog into TABLE.
NAME: the name of the catalog.
FILE: the file listing entries of the catalog.
DIR: the directory where notes for entries usually go."
  (let ((obj (make-hash-table :test #'equal)))
    (puthash "file" file obj)
    (puthash "dir" dir obj)
    (puthash name obj table))
  table)

(defun minaduki-current-catalog ()
  "Return the catalog file we\\='re currently in.
The actual return value is the properties of the catalog."
  (let ((vault (minaduki-vault-closest)))
    (cl-loop
     for v being the hash-values of (minaduki-vault-catalogs vault)
     when
     (f-same?
      (minaduki::current-file-name)
      (minaduki-vault-path-absolute (gethash 'file v) vault))
     return v)))

(defun minaduki-current-catalog-dir ()
  "Return the directory of the catalog we\\='re in right now, or nil."
  (minaduki-vault-path-absolute
   (gethash 'dir (minaduki-current-catalog))
   (minaduki-vault-closest)))

(provide 'minaduki-catalogs)

;;; minaduki-catalogs.el ends here
