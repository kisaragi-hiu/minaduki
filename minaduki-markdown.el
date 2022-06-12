;;; minaduki-markdown.el --- Markdown mode commands -*- lexical-binding: t -*-

;;; Commentary:

;; User commands in Markdown mode.
;;
;; Remember extraction functions should still go into `minaduki-extract'.

;;; Code:

(require 'minaduki-vault)
(require 'markdown-mode)

(defun minaduki-markdown-follow (&optional other)
  "Follow thing at point.

Like `markdown-follow-thing-at-point', but add support for Obsidian links.

When OTHER is non-nil (with a \\[universal-argument]),
open in another window instead of in the current one."
  (interactive "P")
  (if (and (minaduki--in-obsidian-vault?)
           (let ((markdown-enable-wiki-links t))
             (markdown-wiki-link-p)))
      (let ((path (minaduki-obsidian-path (match-string 3))))
        (when other (other-window 1))
        (minaduki//find-file path))
    (markdown-follow-thing-at-point other)))

(provide 'minaduki-markdown)

;;; minaduki-markdown.el ends here
