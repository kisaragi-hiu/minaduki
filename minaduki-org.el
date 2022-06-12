;;; minaduki-org.el --- Org-mode specific commands -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'transient)
(require 'org)

(require 'f)
(require 'dash)
(require 's)

(defun minaduki/org-heading-to-file//suffix (&optional dir full? visit?)
  "Write the current heading to a file under DIR.

DIR defaults to current directory (`default-directory').

The name of the created file is based on the heading. By default,
this is the first WORD of the heading; if FULL? is non-nil, this
happens:

- take the entire heading
- dashes and colons are removed,
- then spaces are replaced with dashes,
- and everything is turned into lowercase (except the T in a timestamp).

For example, given a heading \"2020-05-29T00:00:00+0800 my heading\",
when FULL? is non-nil the file name will be
\"20200529T000000+0800-my-heading.org\", otherwise it will be
\"20200529T000000+0800.org\".

When VISIT? is non-nil, visit the new file after creating it.

Interactively, please use the transient command instead."
  (interactive (let ((args (transient-args 'minaduki/org-heading-to-file)))
                 (transient-save)
                 (list (transient-arg-value "--dir=" args)
                       (transient-arg-value "--full" args)
                       (transient-arg-value "--open" args))))
  (let* ((dir (or dir default-directory))
         (title (org-entry-get nil "ITEM"))
         (filename (->> (if full?
                            title
                          (car (s-split " " title)))
                        (replace-regexp-in-string (rx (any "-/,:?\"!'\\")) "")
                        (replace-regexp-in-string " +" "-")
                        downcase
                        (replace-regexp-in-string (rx (group digit) "t" (group digit))
                                                  "\\1T\\2")
                        (format "%s.org")))
         (path (f-join dir filename))
         (content (save-mark-and-excursion
                    (org-mark-subtree)
                    (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))))
    (with-temp-file path
      (insert content))
    (when visit?
      (find-file path))))

(transient-define-prefix minaduki/org-heading-to-file ()
  "Export heading at point to a file."
  ["Options"
   ("-d" "Directory to export to" "--dir=" transient-read-directory)
   ("-f" "Use the entire heading instead of just the first WORD" "--full")
   ("-v" "Open the exported file" "--open")]
  ["Command"
   ("e" "Export" minaduki/org-heading-to-file//suffix)])

(provide 'minaduki-org)

;;; minaduki-org.el ends here
