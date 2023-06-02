;;; minaduki-bibtex.el --- BibTeX integration -*- lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; 	Jethro Kuan <jethrokuan95@gmail.com>

;;; Commentary:

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'minaduki-utils)
(require 'minaduki-vars)

(require 'org-roam-capture)

(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;;;; Utils
;; ============================================================================

;;;###autoload
(defun orb-process-file-field (citekey)
  "Return a source of CITEKEY.

Sources are resources like PDFs, URLs, etc. that are associated
with a literature entry.

If CITEKEY has multiple sources, prompt to select one of them."
  (ignore-errors
    (when-let* ((entry (minaduki-db//fetch-lit-entry citekey))
                (props (minaduki-lit-entry-props entry))
                (paths (minaduki::resolve-org-links
                        (gethash "sources" props))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (completing-read "File to use: "
                           (minaduki-completion//mark-category
                            paths 'file)))))))

;; ============================================================================
;;;; Orb edit notes
;; ============================================================================

(defun orb--store-link-functions-advice (action)
  "Add or remove advice for each of `orb-ignore-bibtex-store-link-functions'.
ACTION should be a symbol `add' or `remove'.  A piece of advice
is the function `ignore', it is added as `:override'."
  (when orb-ignore-bibtex-store-link-functions
    (let ((advice-func (intern (format "advice-%s" action)))
          (advice (cl-case action
                    (add (list :override #'ignore))
                    (remove (list #'ignore))
                    (t (user-error "Action type not recognised: %s" action)))))
      (dolist (advisee orb-ignore-bibtex-store-link-functions)
        (apply advice-func (push advisee advice))))))

;; TODO: ENTRY should be in `minaduki-lit''s entry format.
(defun orb--preformat-template (template entry)
  "Helper function for `orb--preformat-templates'.
TEMPLATE is an element of `minaduki-capture/templates' and ENTRY
is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  ;; Handle minaduki-capture part
  (let* (;; Org-capture templates: handle different types of
         ;; org-capture-templates: string, file and function; this is
         ;; a stripped down version of `org-capture-get-template'
         (tp
          (pcase (nth 4 template)       ; org-capture template is here
            (`nil 'nil)
            ((and (pred stringp) tmpl) tmpl)
            (`(file ,file)
             (let ((flnm (expand-file-name file (minaduki-vault:main))))
               (if (file-exists-p flnm) (f-read-text flnm)
                 (format "Template file %S not found" file))))
            (`(function ,fun)
             (if (functionp fun) (funcall fun)
               (format "Template function %S not found" fun)))
            (_ "Invalid capture template")))
         ;;  org-roam capture properties are here
         (plst (cdr template))
         ;; regexp for org-capture prompt wildcard
         (rx "\\(%\\^{[[:alnum:]_-]*}\\)")
         (file-keyword (when orb-process-file-keyword
                         (or (and (stringp orb-process-file-keyword)
                                  orb-process-file-keyword)
                             "file")))
         lst)
    ;; First run:
    ;; 1) Make a list of (rplc-s field-value match-position) for the
    ;; second run
    ;; 2) replace minaduki-capture wildcards
    (dolist (keyword orb-preformat-keywords)
      (let* (;; prompt wildcard keyword
             ;; bibtex field name
             (field-name (or (car (rassoc keyword orb-bibtex-field-aliases))
                             keyword))
             ;; get the bibtex field value
             (field-value
              ;; maybe process file keyword
              (or (if (and file-keyword (string= field-name file-keyword))
                      (prog1
                          (orb-process-file-field
                           (cdr (assoc "=key=" entry)))
                        ;; we're done so don't even compare file-name with
                        ;; file-keyword in the successive cycles
                        (setq file-keyword nil))
                    (cdr (assoc field-name entry)))
                  ""))
             ;; org-capture prompt wildcard
             (rplc-s (concat "%^{" (or keyword "citekey") "}"))
             ;; minaduki-capture prompt wildcard
             (rplc-s2 (concat "${" (or keyword "citekey") "}"))
             ;; minaduki-capture :head template
             (head (plist-get plst :head))
             ;; minaduki-capture :file-name template
             (fl-nm (plist-get plst :file-name))
             (i 1)                        ; match counter
             pos)
        ;; Search for rplc-s, set flag m if found
        (when tp
          (while (string-match rx tp pos)
            (if (string= (match-string 1 tp) rplc-s)
                (progn
                  (setq pos (length tp))
                  (cl-pushnew (list rplc-s field-value i) lst))
              (setq pos (match-end 1)
                    i (1+ i)))))
        ;; Replace minaduki-capture prompt wildcards
        (when head
          (plist-put plst :head (s-replace rplc-s2 field-value head)))
        (when fl-nm
          (plist-put plst :file-name (s-replace rplc-s2 field-value fl-nm)))))
    ;; Second run: replace prompts and prompt matches in org-capture
    ;; template string
    (dolist (l lst)
      (when (and tp (nth 1 l))
        (let ((pos (concat "%\\" (number-to-string (nth 2 l)))))
          ;; replace prompt match wildcards with prompt wildcards
          ;; replace prompt wildcards with BibTeX field value
          (setq tp (s-replace pos (car l) tp)
                tp (s-replace (car l) (nth 1 l) tp))))
      (setf (nth 4 template) tp))
    template))

(provide 'minaduki-bibtex)

;;; minaduki-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
