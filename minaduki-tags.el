;;; minaduki-tags.el --- Tag library -*- lexical-binding: t -*-

;;; Commentary:

;; Managing a tag library.

;;; Code:

(require 'org)
(require 'f)

(require 'minaduki-vars)
(require 'minaduki-utils)

(defun minaduki-tags--extract-tags ()
  "Return all tags in the current buffer."
  (let (ret)
    (org-map-region
     (lambda ()
       (unless (minaduki--org-has-children?)
         (push (org-entry-get nil "ITEM") ret)))
     (point-min)
     (point-max))
    ret))

(defun minaduki-tags-library ()
  "Return all tags in library."
  (minaduki::with-temp-buffer
      (minaduki-vault:path-absolute minaduki:tags-file)
    (minaduki-tags--extract-tags)))

(defvar kisaragi-tags/insert/history nil)

(defun kisaragi-tags/insert ()
  "Select and insert a tag from the library."
  (interactive)
  (let ((tag (completing-read
              "Tag: " (minaduki-tags-library)
              nil t nil 'kisaragi-tags/insert/history)))
    (insert tag)))

(defun k/org-tag ()
  "Add or remove tags in `org-mode', using tags from `kisaragi-tags'."
  (interactive)
  (let ((current-tags (org-get-tags (point) t))
        input)
    (setq input
          (->> (completing-read (format "Tags (%s): " (s-join ", " current-tags))
                                (minaduki-tags-library)
                                nil nil nil
                                'org-tags-history)
               ;; Replace all forbidden characters
               (s-replace-regexp "[^[:alnum:]_@#%]+" ":")
               (s-split ":")))
    (unless (equal input "")
      (save-excursion
        (org-back-to-heading)
        (org-set-tags (if (-intersection input current-tags)
                          (-difference current-tags input)
                        (append current-tags input)))))))

(provide 'minaduki-tags)

;;; minaduki-tags.el ends here
