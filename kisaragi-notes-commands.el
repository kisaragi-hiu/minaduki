;;; kisaragi-notes-commands.el --- Commands -*- lexical-binding: t -*-

;;; Commentary:

;; Editing commands.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dom)
(require 'dash)
(require 's)

;;;###autoload
(defun kisaragi-notes/remove-markup (&optional beg end)
  "Remove markup between BEG and END."
  (interactive
   (when (region-active-p)
     (list (region-beginning)
           (region-end))))
  (unless (and beg end)
    (user-error "Please select text to remove markup from"))
  (save-restriction
    (narrow-to-region beg end)
    (let ((parsed (org-element-parse-buffer)))
      (delete-region beg end)
      (insert (->> parsed
                ;; Yes, this works.
                dom-texts
                (s-replace-regexp "[ \t]+" " "))))))

(provide 'kisaragi-notes-commands)

;;; kisaragi-notes-commands.el ends here
