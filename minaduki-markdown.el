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

Like `markdown-follow-thing-at-point', but with support for Obsidian links.

When OTHER is non-nil (with a \\[universal-argument]),
open in another window instead of in the current one."
  (interactive "P")
  (if (let ((markdown-enable-wiki-links t))
        (markdown-wiki-link-p))
      (let ((path (minaduki-obsidian-path (match-string 3))))
        (when other (other-window 1))
        (minaduki//find-file path))
    (markdown-follow-thing-at-point other)))

(defun minaduki-markdown-get-id (&optional skip-match)
  "Return (ID TEXT LEVEL) if current heading has an ID.

If SKIP-MATCH is non-nil, assume the caller has already matched
on `markdown-regex-header.'"
  (when (or skip-match
            (save-excursion
              (and (outline-back-to-heading)
                   (looking-at markdown-regex-header))))
    (-when-let* ((whole (or (match-string-no-properties 1)
                            (match-string-no-properties 5)))
                 ((_ text id) (s-match "\\(.*\\) {#\\(.*?\\)}" whole)))
      (list id text (markdown-outline-level)))))

(provide 'minaduki-markdown)

;;; minaduki-markdown.el ends here
