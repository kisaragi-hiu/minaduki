;;; minaduki-tags.el --- Tag library -*- lexical-binding: t -*-

;;; Commentary:

;; Managing a tag library.

;;; Code:

(require 'org)
(require 'dash)

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

(defun minaduki-tags--current-library ()
  "Return path of the current tags library."
  (minaduki-vault-path-absolute minaduki:tags-file))

(defun minaduki-tags-library ()
  "Return all tags in library."
  (minaduki::with-temp-buffer (minaduki-tags--current-library)
    (minaduki-tags--extract-tags)))

;;;###autoload
(defun minaduki-org-set-heading-tags ()
  "Add or remove tags in `org-mode', utilizing the tags library."
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

(defun minaduki-tags-describe (tag)
  "Jump to the definition of TAG in the tags library."
  (interactive
   (list (completing-read "Describe tag: "
                          (minaduki-tags-library))))
  (find-file (minaduki-tags--current-library))
  (let ((target
         ;; The correct way to do this is to save the tag position on scan
         ;; instead of doing another scan. But, err, this works fo now.
         (cl-block here
           (org-map-region
            (lambda ()
              (when (and (equal (org-entry-get nil "ITEM")
                                tag)
                         (not (minaduki--org-has-children?)))
                (cl-return-from here (point))))
            (point-min) (point-max)))))
    (goto-char target)))

(defun minaduki-tags-new ()
  "Jump to the current tags library and prepare to create a new tag."
  (interactive)
  (find-file (minaduki-tags--current-library))
  (goto-char (point-min))
  (re-search-forward org-complex-heading-regexp nil t))

(provide 'minaduki-tags)

;;; minaduki-tags.el ends here
