;;; kisaragi-notes-templates.el --- File templates -*- lexical-binding: t -*-

;;; Commentary:

;; TODO: Replace org-roam-capture with this

;;; Code:

(require 'f)
(require 'dash)

(require 'org-capture)

(require 'kisaragi-notes-vars)
(require 'kisaragi-notes-completion)

(defun kisaragi-notes-templates//read-template (prompt)
  "Ask the user to select a template, using PROMPT.

Return the absolute path to the selected template."
  (let ((dir kisaragi-notes/templates-directory))
    (--> (f-files dir)
      (-remove #'f-hidden? it)
      (--map (f-relative it dir) it)
      (kisaragi-notes-completion//mark-category it 'file)
      (completing-read prompt it)
      (f-expand it dir))))

(defun kisaragi-notes-templates//make-note (&optional template)
  "Fill out TEMPLATE and return the new note text as a string.

TEMPLATE is a file path. When it is relative, it is relative to
`kisaragi-notes/templates-directory'.

The file should contain text in the format described in the
docstring of `org-capture-templates'. (Search \"%T\" in that
docstring to jump to the relevant section.)

When TEMPLATE is nil, prompt the user to select one.

When TEMPLATE is specified but does not exist as a file, return
nil.

When TEMPLATE is relative and does not have an extension, the
first file under `kisaragi-notes/templates-directory' that
TEMPLATE is a prefix of will be selected. This allows for code
like (kisaragi-notes-template//make-note \"daily\"). This
behavior does not apply when TEMPLATE is absolute."
  ;; Fill in `template' if not provided
  (unless template
    (setq template (completing-read "Template: "
                                    (kisaragi-notes-completion//mark-category
                                     (f-files kisaragi-notes/templates-directory)
                                     'file)
                                    nil t)))
  ;; Try to use files under `kisaragi-notes/templates-directory'
  (when (f-relative? template)
    (setq template (f-expand template kisaragi-notes/templates-directory))
    ;; Specified path without extension -> try to find one with extension
    ;; Only applies for relative paths
    (unless (f-ext? template)
      (setq template (-> kisaragi-notes/templates-directory
                       (f-files
                        (lambda (f)
                          (s-prefix? template f)))
                       car))))
  (when (f-exists? template)
    (org-capture-fill-template
     (f-read-text template))))

(provide 'kisaragi-notes-templates)

;;; kisaragi-notes-templates.el ends here
