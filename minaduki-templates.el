;;; minaduki-templates.el --- File templates -*- lexical-binding: t -*-

;;; Commentary:

;; TODO specify location of resulting file
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

(cl-defstruct (minaduki-template (:copier nil)
                                 (:constructor minaduki-template))
  "Object representing a template.
A template has its content string, as well as frontmatter props that
specify properties like where the resulting file from the template
should go.

The frontmatter of the template itself is the first YAML block,
delimited by \"---\" lines, regardless of the format of the content."
  -content -frontmatter)

(defun minaduki-template-frontmatter (template)
  "Return frontmatter of TEMPLATE.
TEMPLATE can be a `minaduki-template' object or a string. If it is a
string, this is just nil."
  (cond ((stringp template) nil)
        ((minaduki-template-p template)
         (minaduki-template--frontmatter template))
        (t (signal 'wrong-type-argument nil))))

(defun minaduki-template-content (template)
  "Return content of TEMPLATE.
TEMPLATE can be a `minaduki-template' object or a string. If it is a
string, the entire string is treated as the content."
  (cond ((stringp template) template)
        ((minaduki-template-p template)
         (minaduki-template--content template))
        (t (signal 'wrong-type-argument nil))))

(defun minaduki-templates--get (name)
  "Get the template object for the template named NAME."
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
              (minaduki::with-temp-buffer file
                (let ((frontmatter-region (minaduki::find-front-matter)))
                  (if frontmatter-region
                      (minaduki-template
                       :-frontmatter (buffer-substring-no-properties
                                      (car frontmatter-region)
                                      (cdr frontmatter-region))
                       :-content (buffer-substring-no-properties
                                  ;; This jumps past the marker.
                                  (+ (cdr frontmatter-region)
                                     (length "---"))
                                  (point-max)))
                    (minaduki-template :-frontmatter nil
                                       :-content (buffer-substring-no-properties
                                                  (point-min) (point-max)))))))))
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

(defun minaduki-templates--fill (template moment &rest args)
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

%:now is always available as an ISO 8601 timestamp of MOMENT.

This is a wrapper around `org-capture-fill-template'."
  (declare (indent 2))
  (let* ((moment (or moment (current-time)))
         (org-capture-plist
          `(:default-time ,moment))
         (org-store-link-plist
          (append args `(:now ,(format-time-string "%FT%T%z" moment)))))
    ;; Because this is never interactive, we shouldn't have much in `org-mode-hook'.
    (let ((org-mode-hook nil))
      (-some-> (org-capture-fill-template template)
        ;; org-capture-fill-template tries to ensure there is an ending newline.
        ;; Undo that as that's almost never what we want.
        s-trim-right))))

(defun minaduki-templates--insert (template moment &rest args)
  "Insert the result of `minaduki-templates--fill' on TEMPLATE, MOMENT, and ARGS.
TEMPLATE names a template and will be resolved via `minaduki-templates--get'.

If there is no template named TEMPLATE, or if its content is somehow
nil, nothing will be done."
  (when-let* ((template-obj (minaduki-templates--get template))
              (content (minaduki-template-content template-obj)))
    (insert (apply #'minaduki-templates--fill content moment args))))

(provide 'minaduki-templates)

;;; minaduki-templates.el ends here
