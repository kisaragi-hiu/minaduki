;;; kisaragi-notes-embark.el --- Embark actions -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'embark)

(defun kisaragi-notes-embark/open (entry)
  "Open ENTRY."
  (when-let (metadata (get-text-property 0 :metadata entry))
    (let-alist metadata
      (kisaragi-notes/open (list :path .path)))))

(defun kisaragi-notes-embark/insert (entry)
  "Insert ENTRY as a link."
  (when-let (metadata (get-text-property 0 :metadata entry))
    (let-alist metadata
      (insert (org-roam-format-link .path entry)))))

(embark-define-keymap kisaragi-notes-embark/note-map
  "Embark keymap for note items."
  ("RET" kisaragi-notes-embark/open)
  ("i" kisaragi-notes-embark/insert))

(add-to-list 'embark-keymap-alist
             '(note . kisaragi-notes-embark/note-map))

(provide 'kisaragi-notes-embark)

;;; kisaragi-notes-embark.el ends here
