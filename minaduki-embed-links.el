;;; minaduki-embed-links.el --- Add titles to bare links -*- lexical-binding: t -*-

;;; Commentary:

;; Fetch and insert titles to bare links on their own lines.
;; This is similar to obsidian-link-embed, but only for inserting plain text
;; links, but also supports doing so for many links at once.
;;
;; obsidian-link-embed: https://github.com/Seraphli/obsidian-link-embed

;;; Code:

(require 'minaduki-utils)

(require 'eww)
(require 'dash)
(require 'org)
(require 'markdown-mode)

(defun minaduki-embed-links (start end)
  "Find and insert titles for links between START and END.
This only does it for bare links that are on their own lines.
If region isn\\='t active, do it for the current line only."
  (interactive "r")
  (if (region-active-p)
      (setq start (progn
                    (goto-char start)
                    (pos-bol))
            end (progn
                  (goto-char end)
                  (pos-eol)))
    (setq start (pos-bol)
          end (pos-eol)))
  (save-mark-and-excursion
    (with-restriction start end
      (goto-char (point-min))
      (while (not (eobp))
        (save-excursion
          (minaduki-embed-links--add-title-to-url-at-point :sync))
        (redisplay)
        (forward-line)))))

(defun minaduki-embed-links--add-title-to-url-at-point (&optional sync)
  "Fetch the title of the URL at point, then write it in.
If SYNC is non-nil, do this synchronously."
  (when-let ((bounds (bounds-of-thing-at-point 'url)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (url (string-trim
                 (buffer-substring-no-properties start end)))
           (text-buf (current-buffer))
           (callback (lambda (&optional _status)
                       (let (title)
                         (format-decode-buffer)
                         (goto-char (point-min))
                         (eww-parse-headers)
                         (when (re-search-forward "<title>" nil t)
                           (forward-char (- (length "<title>")))
                           (setq title (--> (xml-parse-tag)
                                            (dom-text it)
                                            (decode-coding-string it 'undecided)))
                           (with-current-buffer text-buf
                             (goto-char start)
                             (let ((inhibit-read-only t))
                               (delete-region start end)
                               (insert
                                (minaduki::format-plain-link
                                 :target url
                                 :desc title))
                               ;; (insert (format "[[%s][%s]]" url title))
                               (unless sync
                                 (read-only-mode -1)))))))))
      (if sync
          (progn
            (with-current-buffer (url-retrieve-synchronously url)
              (funcall callback)))
        (read-only-mode)
        (url-retrieve url callback)))))

(provide 'minaduki-embed-links)

;;; minaduki-embed-links.el ends here
