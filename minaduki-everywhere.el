;;; minaduki-everywhere.el --- Write a new note from anywhere -*- lexical-binding: t -*-

;;; Commentary:

;; Functions to make it easy to bind an OS-level shortcut to save a new note.
;;
;; Add an OS shortcut to this:
;;
;;     emacsclient --eval \\='(minaduki-everywhere)\\='
;;
;; Then pressing that shortcut will pop up a window for a fleeting note.
;; TODO: support any template, not just "fleeting"
;; TODO: templates need to support specifying paths

;;; Code:

(require 'emacs-everywhere)
(require 'minaduki-commands)

(defun minaduki-everywhere ()
  "Write a note from anywhere.

This function is meant to be called from emacsclient. Bind this command
to an OS shortcut:

    emacsclient --eval \\='(minaduki-everywhere)\\='"
  (minaduki-everywhere-mode)
  (emacs-everywhere))

(defgroup minaduki-everywhere nil
  "Capture notes from anywhere."
  :group 'minaduki
  :prefix "minaduki-everywhere-")

(defvar minaduki-everywhere--moment nil
  "Global variable for storing the moment of a `minaduki-everywhere' buffer.")

(defun minaduki-everywhere--finish-advice (orig &optional abort)
  "Around advice for `emacs-everywhere-finish'.
This sets things up to actually
ORIG is the original function; ABORT is passed to the original function."
  (let ((emacs-everywhere-copy-command nil) ; don't actually copy
        (emacs-everywhere-paste-command nil) ; and don't actually paste
        (emacs-everywhere-final-hooks '())) ; don't do any conversion
    (save-buffer)
    (funcall orig abort)
    (minaduki-everywhere-mode -1)))

(defun minaduki-everywhere--init-advice (orig)
  "Around advice for `emacs-everywhere-initialise'."
  (let ((emacs-everywhere-init-hooks '(minaduki/new-fleeting-note)))
    (funcall orig)
    ;; Because we've switched to a different buffer in `minaduki/new-fleeting-note',
    ;; `emacs-everywhere-current-app' is now different. Set it to what it
    ;; expects again.
    (setq-local emacs-everywhere-current-app
                (or (frame-parameter nil 'emacs-everywhere-app)
                    (emacs-everywhere-app-info)))))

(define-minor-mode minaduki-everywhere-mode
  "Tweak `emacs-everywhere' to use it for `minaduki-everywhere' features."
  :group 'minaduki-everywhere
  :init-value nil
  :lighter " ME"
  :global t
  (cond (minaduki-everywhere-mode
         (setq minaduki-everywhere--moment (current-time))
         (advice-add 'emacs-everywhere-finish :around #'minaduki-everywhere--finish-advice)
         (advice-add 'emacs-everywhere-initialise :around #'minaduki-everywhere--init-advice))
        (t
         (setq minaduki-everywhere--moment nil)
         (advice-remove 'emacs-everywhere-finish #'minaduki-everywhere--finish-advice)
         (advice-remove 'emacs-everywhere-initialise #'minaduki-everywhere--init-advice))))

(provide 'minaduki-everywhere)

;;; minaduki-everywhere.el ends here
