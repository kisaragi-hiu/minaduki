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

(declare-function evil-insert-state "evil-states")

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
    (let (content)
      ;; Grab the content now
      (unless abort
        (setq content (buffer-string)))
      (unless abort
        (with-current-buffer (minaduki/new-fleeting-note
                              minaduki-everywhere--moment
                              nil nil)
          (erase-buffer)
          (insert content)
          (save-buffer)))
      (minaduki-everywhere-mode -1)
      ;; This might run itself again, because it calls server-buffer-done at the
      ;; end, which triggers server-done-hook, which runs
      ;; emacs-everywhere-finish again. It avoids an infinite loop by turning
      ;; off emacs-everywhere-mode unconditionally and not doing anything else
      ;; if it is already off.
      ;;
      ;; So we turn off `minaduki-everywhere-mode' *before* calling this so that
      ;; our advices are removed when it refers to itself. This way this advice
      ;; doesn't run twice.
      (funcall orig abort))))

(defun minaduki-everywhere--init-advice (orig)
  "Around advice for `emacs-everywhere-initialise'.
ORIG is the original function."
  ;; We'll set frame name, position, major mode, etc. ourselves
  (org-mode)
  (let ((emacs-everywhere-init-hooks nil))
    (funcall orig))
  ;; Override app info with stuff more appropriate for our use case
  (let ((current-app emacs-everywhere-current-app))
    (setq-local emacs-everywhere-current-app
                (make-emacs-everywhere-app
                 ;; ID should be kept so it returns us to the right place afterwards
                 ;; Geometry is also kept because we might as well
                 :id (emacs-everywhere-app-id current-app)
                 :geometry (emacs-everywhere-app-geometry current-app)
                 ;; For doom modeline's use
                 :class "Minaduki"
                 :title (format-time-string "%FT%T%z" minaduki-everywhere--moment))))
  (set-frame-name
   (format "Minaduki :: New fleeting note for %s"
           (format-time-string "%FT%T%z" minaduki-everywhere--moment)))
  ;; the moment is already set by the entry point.
  (minaduki-templates--insert "fleeting" minaduki-everywhere--moment)
  ;; This is done by `emacs-everywhere-insert-selection' in a normal
  ;; `emacs-everywhere' session. Here we have to do it ourselves because we
  ;; override that function to disable its main functionality. (We want to fill
  ;; the buffer with the template, as done above.)
  (when (bound-and-true-p evil-local-mode)
    (evil-insert-state)))

(define-minor-mode minaduki-everywhere-mode
  "Tweak `emacs-everywhere' to use it for `minaduki-everywhere' features."
  :group 'minaduki-everywhere
  :init-value nil
  :lighter " ME"
  :global t
  (cond (minaduki-everywhere-mode
         (setq minaduki-everywhere--moment (current-time))
         (advice-add 'emacs-everywhere-insert-selection :override #'ignore)
         (advice-add 'emacs-everywhere-finish :around #'minaduki-everywhere--finish-advice)
         (advice-add 'emacs-everywhere-initialise :around #'minaduki-everywhere--init-advice))
        (t
         (setq minaduki-everywhere--moment nil)
         (advice-remove 'emacs-everywhere-insert-selection #'ignore)
         (advice-remove 'emacs-everywhere-finish #'minaduki-everywhere--finish-advice)
         (advice-remove 'emacs-everywhere-initialise #'minaduki-everywhere--init-advice))))

(provide 'minaduki-everywhere)

;;; minaduki-everywhere.el ends here
