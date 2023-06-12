;;; minaduki-templates.el --- File templates -*- lexical-binding: t -*-

;;; Commentary:

;; TODO: Replace minaduki-capture with this
;; TODO: passing in arguments: just use %s?
;; TODO: each special template needs a documentation on what arguments
;; they will receive. "new" will get the title as the first argument,
;; "new-lit" will get the title and the key, and so on.

;;; Code:

(require 'f)
(require 'dash)

(require 'org-capture)

(require 'minaduki-vars)
(require 'minaduki-completion)

(defun minaduki-templates//read-template (prompt)
  "Ask the user to select a template, using PROMPT.

Return the absolute path to the selected template."
  (let ((dir minaduki/templates-directory))
    (--> (f-files dir)
         (-remove #'f-hidden? it)
         (--map (f-relative it dir) it)
         (minaduki-completion//mark-category it 'file)
         (completing-read prompt it)
         (f-expand it dir))))

(defun minaduki-templates//path-to (template)
  "Return absolute path to TEMPLATE.

TEMPLATE is a file under `minaduki/templates-directory'. If
TEMPLATE has no extension, TEMPLATE.org or TEMPLATE.md will be used."
  (unless (f-ext? template)
    ;; TODO: choose an appropriate template
    (setq template (car (f-files minaduki/templates-directory
                                 (lambda (f) (equal template (f-base f)))))))
  (setq template (f-expand template minaduki/templates-directory))
  template)

;; (defun minaduki-templates//capture (template)
;;   "Create a new note using TEMPLATE.

;; The note is stored in a location specified in the #+path: keyword.

;; Template is a file under `minaduki/templates-directory'.

;; Return the captured file.

;; The template's contents are expanded with `org-capture-fill-template'."
;;   (let ((file (minaduki-templates//path-to template))
;;         target content)
;;     (minaduki::with-file file nil
;;       (-when-let* ((path (car (minaduki-extract//file-prop "path"))))
;;         (setq target (f-expand (s-trim (org-capture-fill-template path))
;;                                (minaduki-vault:main))))
;;       (setq content (buffer-string)))
;;     (with-temp-file target
;;       (insert (org-capture-fill-template content))
;;       (save-excursion
;;         (goto-char 1)
;;         (while (search-forward "^#+path:" nil t)
;;           (delete-region (line-beginning-position)
;;                          (1+ (line-end-position))))))
;;     target))

(defun minaduki-templates//make-note (&optional template)
  "Fill out TEMPLATE and return the new note text as a string.

TEMPLATE is a file path. When it is relative, it is relative to
`minaduki/templates-directory'.

The file should contain text in the format described in the
docstring of `org-capture-templates'. (Search \"%T\" in that
docstring to jump to the relevant section.)

When TEMPLATE is nil, prompt the user to select one.

When TEMPLATE is specified but does not exist as a file, return
nil.

When TEMPLATE is relative and does not have an extension, the
first file under `minaduki/templates-directory' that
TEMPLATE is a prefix of will be selected. This allows for code
like (minaduki-template//make-note \"daily\"). This
behavior does not apply when TEMPLATE is absolute."
  ;; Fill in `template' if not provided
  (unless template
    (setq template (completing-read "Template: "
                                    (minaduki-completion//mark-category
                                     (f-files minaduki/templates-directory)
                                     'file)
                                    nil t)))
  ;; Try to use files under `minaduki/templates-directory'
  (when (f-relative? template)
    (setq template (f-expand template minaduki/templates-directory))
    ;; Specified path without extension -> try to find one with extension
    ;; Only applies for relative paths
    (unless (f-ext? template)
      (setq template (-> minaduki/templates-directory
                         (f-files
                          (lambda (f)
                            (s-prefix? template f)))
                         car))))
  (when (f-exists? template)
    (org-capture-fill-template
     (f-read-text template))))

(provide 'minaduki-templates)

;;; minaduki-templates.el ends here
