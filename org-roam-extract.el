;;; org-roam-extract.el --- Extraction functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)

(require 'org-element)
(require 'org)

(require 'org-roam-id)
(require 'org-roam-macs)

(defun org-roam--extract-global-props-drawer (props)
  "Extract PROPS from the file-level property drawer in Org."
  (let (ret)
    (org-with-point-at 1
      (dolist (prop props ret)
        (when-let ((v (org-entry-get (point) prop)))
          (push (cons prop v) ret))))))

(defun org-roam--collect-keywords (keywords)
  "Collect all Org KEYWORDS in the current buffer."
  (if (functionp 'org-collect-keywords)
      (org-collect-keywords keywords)
    (let ((buf (org-element-parse-buffer))
          res)
      (dolist (k keywords)
        (let ((p (org-element-map buf 'keyword
                   (lambda (kw)
                     (when (string-equal (org-element-property :key kw) k)
                       (org-element-property :value kw)))
                   :first-match nil)))
          (push (cons k p) res)))
      res)))

(defun org-roam--extract-global-props-keyword (keywords)
  "Extract KEYWORDS from the current Org buffer."
  (let (ret)
    (pcase-dolist (`(,key . ,values) (org-roam--collect-keywords keywords))
      (dolist (value values)
        (push (cons key value) ret)))
    ret))

(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current Org buffer.
Props are extracted from both the file-level property drawer (if
any), and Org keywords. Org keywords take precedence."
  (append
   (org-roam--extract-global-props-keyword props)
   (org-roam--extract-global-props-drawer props)))

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

(defun org-roam--get-outline-path ()
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as a
list of strings. Statistics cookies are removed and links are
kept.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline."
  (org-with-wide-buffer
   (save-match-data
     (and (or (condition-case nil
                  (org-back-to-heading t)
                (error nil))
              (org-up-heading-safe))
          (reverse (org-roam--get-outline-path-1))))))

(defun org-roam--get-outline-path-1 ()
  "Return outline path to current headline.

Outline path is a list of strings, in reverse order.  See
`org-roam--get-outline-path' for details.

Assume buffer is widened and point is on a headline."
  (when org-complex-heading-regexp
    (let ((heading (let ((case-fold-search nil))
                     (looking-at org-complex-heading-regexp)
                     (if (not (match-end 4)) ""
                       ;; Remove statistics cookies.
                       (org-trim
                        (replace-regexp-in-string
                         "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                         (match-string-no-properties 4)))))))
      (if (org-up-heading-safe)
          (cons heading (org-roam--get-outline-path-1))
        (list heading)))))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [source dest type properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (require 'org-ref nil t)
  (setq file-path (or file-path
                      org-roam-file-name
                      (buffer-file-name)))
  (save-excursion
    (let (links)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (goto-char (org-element-property :begin link))
          (let* ((type (org-roam--collate-types (org-element-property :type link)))
                 (path (org-element-property :path link))
                 (properties (list :outline (org-roam--get-outline-path)
                                   :point (point)))
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
                (push (vector file-path name type properties) links))))))
      links)))

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

(defun org-roam--split-ref (ref)
  "Processes REF into its type and path.
Returns a cons cell of type and path if ref is a valid ref."
  (save-match-data
    (when (string-match org-link-plain-re ref)
      (cons (org-roam--collate-types (match-string 1 ref))
            (match-string 2 ref)))))

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
           (when-let ((r (org-roam--split-ref ref)))
             (push r refs)))))
    refs))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer and return the type and the key of the ref."
  (car (org-roam--extract-refs)))

(provide 'org-roam-extract)
;;; org-roam-extract.el ends here
