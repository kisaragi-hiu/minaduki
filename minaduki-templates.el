;;; minaduki-templates.el --- File templates -*- lexical-binding: t -*-

;;; Commentary:

;; TODO: Replace minaduki-capture with this
;; TODO: passing in arguments: just use %s?
;; TODO: each special template needs a documentation on what arguments
;; they will receive. "new" will get the title as the first argument,
;; "new-lit" will get the title and the key, and so on.

;; Builtin templates:
;; - "daily": `minaduki/new-daily-note'

;;; Code:

(require 'f)
(require 'dash)
(require 'map)

(require 'org-capture)

(require 'minaduki-vars)
(require 'minaduki-completion)

(cl-defun minaduki-templates::list-templates (&key with-content all)
  "List templates, including files and those from the alist.

When WITH-CONTENT is non-nil, also include their contents.

When ALL is `files', return file templates only.
If it's any other non-nil value, return all available templates.
Otherwise, allow shadowing to take place (files over the alist).

Those from the alist (`minaduki-templates-alist') have their
names prepended with a colon."
  (let ((templates (make-hash-table :test #'equal)))
    (unless (eq all 'files)
      (pcase-dolist (`(,name . ,content) minaduki-templates-alist)
        (puthash (if all
                     (concat ":" name)
                   name)
                 (if with-content
                     content
                   t)
                 templates)))
    (dolist (file (minaduki-templates::list-files t))
      (puthash (if all
                   file
                 (f-filename file))
               (if with-content
                   (minaduki::file-content file)
                 t)
               templates))
    (if with-content
        (map-into templates 'alist)
      (map-keys templates))))

(defun minaduki-templates::list-files (&optional full)
  "List all template files.
When FULL is non-nil, return full paths."
  (directory-files minaduki/templates-directory full (rx bos (not "."))))

(defun minaduki-templates:get (name)
  "Get the string content for the template named NAME."
  (let ((has-colon (s-prefix? ":" name)))
    ;; Use a prefix colon to only get templates from the alist
    (when has-colon
      (setq name (substring name 1)))
    (or
     ;; Files have priority
     (and (not has-colon)
          (let ((files (minaduki-templates::list-files t)))
            (when-let (file (or (--first (equal name it) files)
                                (--first (equal name (f-filename it)) files)
                                (--first (equal name (f-base it)) files)))
              (minaduki::file-content file))))
     ;; Then `minaduki-templates-alist', using the same matching logic
     (let ((templates minaduki-templates-alist))
       (when-let (pair (or (--first (equal name (car it)) templates)
                           ;; second case above is not applicable
                           (--first (equal name (f-base (car it))) templates)))
         (cdr pair))))))

(cl-defun minaduki-templates:read (prompt &key all return-content)
  "Ask the user to select a template, using PROMPT.
When ALL is `files', select from file templates only.
If it's any other non-nil value, select from all available templates.
Otherwise, allow shadowing to take place (files over the alist).

When RETURN-CONTENT is non-nil, return the content of the
selected template instead of the name."
  (declare (indent 1))
  (if return-content
      (let ((templates (minaduki-templates::list-templates :all all :with-content t)))
        (map-elt templates (completing-read prompt (map-keys templates))))
    (completing-read prompt (minaduki-templates::list-templates :all all))))

(defun minaduki-templates:fill (template
                                 moment
                                 &rest args)
  "Fill out TEMPLATE and return the result as a string.

TEMPLATE is the raw template text, in the syntax described by the
docstring of `org-capture-templates'. (Search \"%T\" in that
docstring to jump to the relevant section.)

MOMENT is used for expanding date and time within the template.

ARGS is a plist of arguments that are available to the template.
For example,
  (:url \"https://example.com\")
means that the template can write \"%:url\" and have it replaced
with \"https://example.com\". This uses the
`org-store-link-plist' mechanism.

This is a wrapper around `org-capture-fill-template'."
  (declare (indent 2))
  (let ((org-capture-plist
         `(:default-time ,moment))
        (org-store-link-plist args))
    (org-capture-fill-template template)))

(provide 'minaduki-templates)

;;; minaduki-templates.el ends here
