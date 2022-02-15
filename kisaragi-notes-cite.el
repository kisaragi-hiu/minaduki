;;; kisaragi-notes-cite.el --- Org Cite integration -*- lexical-binding: t -*-

;;; Commentary:

;; Citation integration.

;;; Code:

(require 'oc)
(require 'org-element)

(org-cite-register-processor 'minaduki
  :follow (lambda (datum _)
            (let ((key
                   ;; Taken from the `basic' processor's follow function
                   (if (eq 'citation-reference (org-element-type datum))
                       (org-element-property :key datum)
                     (pcase (org-cite-get-references datum t)
                       (`(,key) key)
                       (keys
                        (or (completing-read "Select citation key: " keys nil t)
                            (user-error "Aborted")))))))
              (minaduki/literature-note-actions key))))

;;;###autoload
(with-eval-after-load 'oc
  (setq org-cite-follow-processor 'minaduki))

(provide 'kisaragi-notes-cite)

;;; kisaragi-notes-cite.el ends here
