;;; org-roam-extract.el --- Extraction functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)

(require 'org-element)
(require 'org)

(require 'org-roam-id)
(require 'org-roam-macs)

(defvar markdown-regex-link-inline)

(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current Org buffer.
Props are extracted from both the file-level property drawer (if
any), and Org keywords. Org keywords take precedence."
  (let (ret)
    ;; Org: keyword properties
    (pcase-dolist (`(,key . ,values) (org-collect-keywords props))
      (dolist (value values)
        (push (cons key value) ret)))
    ;; Org: file-level property drawer properties
    (org-with-point-at 1
      (dolist (prop props)
        (when-let ((v (org-entry-get (point) prop)))
          (push (cons prop v) ret))))
    ret))

(defun org-roam--extract-prop-as-list (prop)
  "Extract PROP from the current Org buffer as a list.

This is the common logic behind the extraction of roam_tags and
roam_alias."
  ;; Values are split in two ways:
  ;; 1. with spaces and double quotes:
  ;;     #+prop: a b c \"quoted string\"
  ;;     -> '(\"a\" \"b\" \"c\" \"quoted string\")
  ;; 2. and/or with multiple lines:
  ;;     #+prop: a b
  ;;     #+prop: c d
  ;;     -> '(\"a\" \"b\" \"c\" \"d\")
  (--> (org-roam--extract-global-props (list prop))
    ;; so that the returned order is the same as in the buffer
    nreverse
    ;; '(("ROAM_TAGS" . "a b") ("ROAM_TAGS" . "c d"))
    ;; -> '("a b" "c d")
    (mapcar #'cdr it)
    (mapcar #'split-string-and-unquote it)
    ;; We have a list of lists at this point. Join them.
    (apply #'append it)))

(defun kisaragi-notes-extract//org-links-context ()
  "Return the context around point."
  (let ((context (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    (when (eq 'quote-block
              (car (-> (org-element-at-point)
                     org-element-lineage
                     -last-item)))
      (setq context (format "#+begin_quote\n%s\n#+end_quote" context)))
    context))

(defun org-roam--extract-links-org (file-path)
  "Extract links in current buffer in Org mode format ([[target][desc]]).

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (let (links)
      (while (re-search-forward org-link-any-re nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (let ((link (org-element-link-parser)))
            (when link
              (goto-char (org-element-property :begin link))
              (let* ((type (org-roam--collate-types (org-element-property :type link)))
                     (path (org-element-property :path link))
                     (content (kisaragi-notes-extract//org-links-context))
                     (properties (list :outline (org-roam--get-outline-path)
                                       :point (point)
                                       :content content))
                     (names (pcase type
                              ("id"
                               (when-let ((file-path (org-roam-id-get-file path)))
                                 (list file-path)))
                              ("cite" (list path))
                              ("website" (list path))
                              ("fuzzy" (list path))
                              ("roam" (list path))
                              (_ (if (or (file-remote-p path)
                                         (org-roam--url-p path))
                                     (list path)
                                   (let ((file-maybe (expand-file-name path (file-name-directory file-path))))
                                     (if (f-exists? file-maybe)
                                         (list file-maybe)
                                       (list path))))))))
                (dolist (name names)
                  (when name
                    (push (vector file-path name type properties) links))))))))
      links)))

(defun org-roam--extract-links-wiki (file-path)
  "Extract links in current buffer in this format: [[file]].

The target is assumed to be file.md in the above example.

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
             collect
             (let* ((to-file (concat (match-string-no-properties 1) ".md"))
                    (begin-of-block (match-beginning 0))
                    (end-of-block (save-excursion
                                    (unless (eobp)
                                      (forward-sentence))
                                    (point)))
                    ;; get the text block = content around the link as context
                    (content
                     (buffer-substring-no-properties
                      begin-of-block end-of-block)))
               (vector file-path ; file-from
                       (file-truename
                        (expand-file-name
                         to-file (file-name-directory file-path))) ; file-to
                       "file" ; link-type
                       (list :content content :point begin-of-block))))))

(defun org-roam--extract-links-markdown (file-path)
  "Extract Markdown links from current buffer.

Links are assumed to originate from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward markdown-regex-link-inline nil t)
     collect
     (let ((imagep (match-string-no-properties 1))
           (link (match-string-no-properties 6))
           (begin-of-block)
           (end-of-block)
           (content)
           (link-type "file"))
       (when (and (not imagep)
                  (not (url-type (url-generic-parse-url link))))
         (save-excursion
           ;; get the text block = content around the link as context
           (unless (eobp)
             (forward-sentence))
           (setq end-of-block (point))
           (backward-sentence)
           (setq begin-of-block (point))
           (setq content
                 (buffer-substring-no-properties begin-of-block end-of-block)))
         (vector file-path ; file-from
                 (file-truename
                  (expand-file-name (url-filename (url-generic-parse-url link))
                                    (file-name-directory file-path))) ; file-to
                 link-type
                 (list :content content :point begin-of-block)))))))

(defun org-roam--extract-links-pandoc-cite (file-path)
  "Extract cite links defined like this: @bibkey.

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward
                    "\\(?:[^[:alnum:]]\\|^\\)\\(-?@\\)\\([-a-zA-Z0-9_+:]+\\)"
                    nil t)
             collect
             (let* ((to-file (match-string-no-properties 2))
                    begin-of-block
                    end-of-block
                    content
                    (link-type "cite"))
               (save-excursion
                 (forward-paragraph)
                 (setq end-of-block (point))
                 (backward-paragraph)
                 (setq begin-of-block (point))
                 (setq content (buffer-substring-no-properties begin-of-block end-of-block)))
               (vector file-path ; file-from
                       to-file
                       link-type
                       (list :content content :point begin-of-block))))))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [source dest type properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (require 'org-ref nil t)
  (setq file-path (or file-path org-roam-file-name (buffer-file-name)))
  (cond
   ;; Using `derived-mode-p' maybe adds 3 seconds per call to the
   ;; cache build when there are a million links. At that point 3
   ;; seconds is probably not that much of a deal.
   ((derived-mode-p 'org-mode)
    (org-roam--extract-links-org file-path))
   ((derived-mode-p 'markdown-mode)
    (append (org-roam--extract-links-wiki file-path)
            (org-roam--extract-links-markdown file-path)
            (org-roam--extract-links-pandoc-cite file-path)))))

(defun org-roam--extract-ids (&optional file-path)
  "Extract all IDs within the current buffer.
If FILE-PATH is nil, use the current file."
  (setq file-path (or file-path org-roam-file-name (buffer-file-name)))
  (let (result)
      ;; We need to handle the special case of the file property drawer (at outline level 0)
      (org-with-point-at (point-min)
        (when-let ((before-first-heading (= 0 (org-outline-level)))
                   (id (org-entry-get nil "ID")))
           (push (vector id file-path 0) result)))
      (org-map-region
       (lambda ()
         (when-let ((id (org-entry-get nil "ID")))
           (push (vector id file-path (org-outline-level)) result)))
       (point-min) (point-max))
      result))

(defun org-roam--extract-titles-title ()
  "Return title from \"#+title\" of the current buffer."
  (let* ((prop (org-roam--extract-global-props '("TITLE")))
         (title (cdr (assoc "TITLE" prop))))
    (when title
      (list title))))

(defun org-roam--extract-titles-alias ()
  "Return the aliases from the current buffer.
Reads from the \"roam_alias\" property."
  (condition-case nil
      (org-roam--extract-prop-as-list "ROAM_ALIAS")
    (error
     (progn
       (lwarn '(org-roam) :error
              "Failed to parse aliases for buffer: %s. Skipping"
              (or org-roam-file-name
                  (buffer-file-name)))
       nil))))

(defun org-roam--extract-titles-headline ()
  "Return the first headline of the current buffer."
  (let ((headline (save-excursion
                    (goto-char (point-min))
                    ;; "What happens if a heading star was quoted
                    ;; before the first heading?"
                    ;; - `org-map-region' also does this
                    ;; - Org already breaks badly when you do that;
                    ;; precede the heading star with a ",".
                    (re-search-forward org-outline-regexp-bol nil t)
                    (org-entry-get nil "ITEM"))))
    (when headline
      (list headline))))

(defun org-roam--extract-titles (&optional sources nested)
  "Extract the titles from current buffer using SOURCES.
If NESTED, return the first successful result from SOURCES."
  (org-with-wide-buffer
   (let (coll res)
     (cl-dolist (source (or sources
                            org-roam-title-sources))
       (setq res (if (symbolp source)
                     (funcall (intern (concat "org-roam--extract-titles-" (symbol-name source))))
                   (org-roam--extract-titles source t)))
       (when res
         (if (not nested)
             (setq coll (nconc coll res))
           (setq coll res)
           (cl-return))))
     (-uniq coll))))

(defun org-roam--extract-tags-all-directories (file)
  "Extract tags from using the directory path FILE.
All sub-directories relative to `org-roam-directory' are used as tags."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (expand-file-name org-roam-directory)))))
    (f-split dir-relative)))

(defun org-roam--extract-tags-last-directory (file)
  "Extract tags from using the directory path FILE.
The final directory component is used as a tag."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (expand-file-name org-roam-directory)))))
    (last (f-split dir-relative))))

(defun org-roam--extract-tags-first-directory (file)
  "Extract tags from path FILE.
The first directory component after `org-roam-directory' is used as a
tag."
  (when-let ((dir-relative (file-name-directory
                            (file-relative-name file (expand-file-name org-roam-directory)))))
    (list (car (f-split dir-relative)))))

(defun org-roam--extract-tags-prop (_file)
  "Extract tags from the current buffer's \"#roam_tags\" global property."
  (condition-case nil
      (org-roam--extract-prop-as-list "ROAM_TAGS")
    (error
     (progn
       (lwarn '(org-roam) :error
              "Failed to parse tags for buffer: %s. Skipping"
              (or org-roam-file-name
                  (buffer-file-name)))
       nil))))

(defun org-roam--extract-tags-vanilla (_file)
  "Extract vanilla `org-mode' tags.
This includes all tags used in the buffer."
  (org-set-regexps-and-options 'tags-only)
  (-flatten (org-get-buffer-tags)))

(defun org-roam--extract-tags (&optional file)
  "Extract tags from the current buffer.
If file-path FILE, use it to determine the directory tags.
Tags are obtained via:

1. Directory tags: Relative to `org-roam-directory': each folder
   path is considered a tag.
2. The key #+roam_tags."
  (let* ((file (or file (buffer-file-name (buffer-base-buffer))))
         (tags (-uniq
                (mapcan (lambda (source)
                          (funcall (intern (concat "org-roam--extract-tags-"
                                                   (symbol-name source)))
                                   file))
                        org-roam-tag-sources))))
    (pcase org-roam-tag-sort
      ('nil tags)
      ((pred booleanp) (cl-sort tags 'string-lessp :key 'downcase))
      (`(,(pred symbolp) . ,_)
       (apply #'cl-sort (push tags org-roam-tag-sort)))
      (wrong-type (signal 'wrong-type-argument
                          `((booleanp (list symbolp))
                            ,wrong-type))))))

(defun org-roam--collate-types (type)
  "Collate TYPE into a parent type.
Packages like `org-ref' introduce many different link prefixes,
but we collate them under the same parent type to clean up
backlinks."
  (cond ((and (boundp 'org-ref-cite-types)
              (member type org-ref-cite-types))
         "cite")
        ((member type '("http" "https"))
         "website")
        (t type)))

(defun kisaragi-notes-extract//process-ref (ref)
  "Processes REF into its type and path.

Returns a cons cell '(TYPE . PATH) if ref is a valid ref.

REF is either a plain link or a plain string. If it's a link, the
protocol is treated as the TYPE (after processing through
`org-roam--collate-types'). Otherwise, REF is assumed to be a cite ref."
  (save-match-data
    (if (string-match org-link-plain-re ref)
        (cons (org-roam--collate-types (match-string 1 ref))
              (match-string 2 ref))
      (cons "cite" ref))))

(defun org-roam--extract-refs ()
  "Extract all refs (ROAM_KEY statements) from the current buffer.

Each ref is returned as a cons of its type and its key."
  (let (refs)
    (pcase-dolist
        (`(,_ . ,roam-key)
         (org-roam--extract-global-props '("ROAM_KEY")))
      (pcase roam-key
          ('nil nil)
          ((pred string-empty-p)
           (user-error "Org property #+roam_key cannot be empty"))
          (ref
           (when-let ((r (kisaragi-notes-extract//process-ref ref)))
             (push r refs)))))
    refs))

(provide 'org-roam-extract)
;;; org-roam-extract.el ends here
