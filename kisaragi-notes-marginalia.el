;;; kisaragi-notes-marginalia.el --- Marginalia annotators for note entries -*- lexical-binding: t -*-

;;; Commentary:

;; A Marginalia annotator for note entries.

;;; Code:

(require 's)
(require 'f)

(require 'minaduki-vars)
(require 'minaduki-completion)

(require 'marginalia)

(defun minaduki-completion/annotate-note (cand)
  "Marginalia annotation for note entries.

CAND is the entry in the completion. Metadata is passed through
text properties to distinguish between entries with the same
title."
  (when-let (metadata (get-text-property 0 :metadata cand))
    (let-alist metadata
      (marginalia--fields
       ((when .tags
          (format "(%s)" (s-join "," .tags)))
        :width 30 :face 'marginalia-list)
       ((f-relative .path org-directory)
        :truncate 40 :face 'marginalia-file-name)))))

(add-to-list 'marginalia-annotator-registry
             '(note minaduki-completion/annotate-note none))

(provide 'kisaragi-notes-marginalia)

;;; kisaragi-notes-marginalia.el ends here
