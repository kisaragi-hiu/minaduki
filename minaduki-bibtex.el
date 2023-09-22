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
(require 'minaduki-db)

(require 'minaduki-capture)

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
    (when-let* ((entry (minaduki-db::fetch-lit-entry citekey))
                (props (minaduki-lit-entry-props entry))
                (paths (minaduki::resolve-org-links
                        (gethash "sources" props))))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (completing-read "File to use: "
                           (minaduki-completion//mark-category
                            paths 'file)))))))

(provide 'minaduki-bibtex)

;;; minaduki-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
