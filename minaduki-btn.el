;;; minaduki-btn.el --- Buttons for commands -*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes it is useful to have commands available in a buffer as an
;; interactive button, rather than in a keybind. Perhaps a command is
;; particularly useful in a specific file, for instance.
;;
;; This module facilitates that.

;;; Code:

(require 'minaduki-utils)
(require 'minaduki-vars)

(defvar minaduki-btn::pressed nil
  "This becomes t during `minaduki-btn:follow'.")

(defun minaduki-btn:follow (cmd)
  "Follow CMD."
  (if (rassoc (intern-soft cmd) minaduki::global-commands)
      (let ((minaduki-btn::pressed t))
        (call-interactively (intern-soft cmd)))
    (message "%s is not a valid command in a button" cmd)))

(defun minaduki-btn:insert ()
  "Insert a button for a global command."
  (interactive)
  (let* ((candidates minaduki::global-commands)
         (selection (completing-read "Insert a button for action: " candidates))
         (pair (assoc selection candidates)))
    (insert (minaduki::format-plain-link
             :target (format "minaduki-btn:%s" (cdr pair))
             :desc (format "Minaduki: %s" (car pair))))))

(provide 'minaduki-btn)

;;; minaduki-btn.el ends here
