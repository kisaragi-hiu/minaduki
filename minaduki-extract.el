;;; minaduki-extract.el --- Extraction functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)

(require 'org-element)
(require 'org-id)
(require 'org)

(require 'kisaragi-notes-utils)
(require 'minaduki-vars)

(defvar markdown-regex-link-inline)

;; Silence byte compiler on Org < 9.5. These are not present there and
;; aren't used.
(declare-function org-element-citation-reference-parser "org-element")
(defvar org-element-citation-prefix-re)

(declare-function minaduki-db//query-title "minaduki-db")

(defun minaduki-extract//markdown-props (prop)
  "Extract PROP in the Markdown front matter."
  (save-excursion
    (goto-char (point-min))
    ;; FIXME: extract this into a `with-front-matter' macro
    (-when-let* ((start
                  ;; The beginning of the frontmatter, which has to be at the
                  ;; beginning of the buffer (before char position 4).
                  (re-search-forward "^---$" 4 t))
                 (end
                  ;; The end of the frontmatter
                  (re-search-forward "^---$" nil t)))
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward
                 (format (rx bol "%s:" space (group (0+ any)))
                         prop)
                 nil t)
            (match-string-no-properties 1)))))))

(defun org-roam--extract-prop-as-list (prop)
  "Extract PROP from the current Org buffer as a list.

This is used to extract #+roam_tags."
  ;; Values are split in two ways:
  ;; 1. with spaces and double quotes:
  ;;     #+prop: a b c \"quoted string\"
  ;;     -> '(\"a\" \"b\" \"c\" \"quoted string\")
  ;; 2. and/or with multiple lines:
  ;;     #+prop: a b
  ;;     #+prop: c d
  ;;     -> '(\"a\" \"b\" \"c\" \"d\")
  (--> (minaduki//org-props (list prop))
       ;; so that the returned order is the same as in the buffer
       nreverse
       ;; '(("ROAM_TAGS" . "a b") ("ROAM_TAGS" . "c d"))
       ;; -> '("a b" "c d")
       (mapcar #'cdr it)
       (mapcar #'split-string-and-unquote it)
       ;; We have a list of lists at this point. Join them.
       (apply #'append it)))

(defun minaduki-extract/citation (file-from)
  "Extract Org 9.5+ citations.

Currently citations are treated as links of the `cite' type, from
FILE-FROM to the key."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-element-citation-prefix-re nil t)
             nconc
             (cl-loop when (org-element-citation-reference-parser)
                      collect
                      (vector
                       file-from
                       (org-element-property :key it)
                       "cite"
                       (list :outline (org-roam--get-outline-path)
                             :point (point)))
                      while (search-forward ";" (line-end-position) t)))))

(defun minaduki-extract//org-links-context ()
  "Return the context around point."
  (let* ((elem-at-point (org-element-at-point))
         (content
          (buffer-substring-no-properties
           (org-element-property :begin elem-at-point)
           (org-element-property :end elem-at-point))))
    (cond
     ((eq 'quote-block
          (org-element-type
           (-last-item
            (org-element-lineage
             elem-at-point))))
      (format "#+begin_quote\n%s\n#+end_quote"
              (s-trim content)))
     (t
      (s-trim content)))))

(defun minaduki-extract//id-file (id)
  "Find the file containing ID."
  (unless org-id-locations
    (org-id-locations-load))
  (or (and org-id-locations
           (hash-table-p org-id-locations)
           (gethash id org-id-locations))))

(defun org-roam--extract-links-org (file-path)
  "Extract links in current buffer in Org mode format ([[target][desc]]).

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (let (links)
      (while (re-search-forward org-link-any-re nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (let ((link (org-element-link-parser))
                (elem-at-point (org-element-at-point)))
            (when (and link
                       (not (and (eq 'keyword (car elem-at-point))
                                 (equal "ROAM_KEY"
                                        (org-element-property :key elem-at-point)))))
              (goto-char (org-element-property :begin link))
              (let* ((type (org-roam--collate-types (org-element-property :type link)))
                     (path (org-element-property :path link))
                     (content (minaduki-extract//org-links-context))
                     (properties (list :outline (org-roam--get-outline-path)
                                       :point (point)
                                       :content content))
                     (names (pcase type
                              ("id"
                               ;; The cache is not available yet
                               (when-let ((file-path (minaduki-extract//id-file path)))
                                 (list file-path)))
                              ("cite" (list path))
                              ("website" (list path))
                              ("fuzzy" (list path))
                              ("roam" (list path))
                              (_ (if (or (file-remote-p path)
                                         (minaduki//url? path))
                                     (list path)
                                   (let ((file-maybe (expand-file-name path (file-name-directory file-path))))
                                     (if (f-exists? file-maybe)
                                         (list file-maybe)
                                       (list path))))))))
                (dolist (name names)
                  (when name
                    (push (vector file-path name type properties) links))))))))
      links)))

;; Modified from md-roam's `md-roam--extract-wiki-links'
;;
;; This now depends on the title cache having been established, which
;; is problematic. db.el has be adapted to make sure titles are
;; available before this is run.
(defun org-roam--extract-links-wiki (file-path)
  "Extract links in current buffer in this format: [[foo]].

This link will point to a page titled \"foo\".

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
             collect
             (let* ((target (car (minaduki-db//query-title
                                  (match-string-no-properties 1))))
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
                       target ; file-to
                       "file" ; link-type
                       (list :content content :point begin-of-block))))))

;; Modified from md-roam's `md-roam--extract-file-links'
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
                 (buffer-substring-no-properties begin-of-block end-of-block))))
       (vector file-path ; file-from
               (file-truename
                (expand-file-name (url-filename (url-generic-parse-url link))
                                  (file-name-directory file-path))) ; file-to
               link-type
               (list :content content :point begin-of-block))))))

;; Modified from md-roam's `md-roam--extract-cite-links'
(defun org-roam--extract-links-pandoc-cite (file-path)
  "Extract cite links defined like this: @bibkey.

Assume links come from FILE-PATH."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward
                    (rx (or (not alnum) bol)
                        (group (opt "-") "@")
                        (group (one-or-more (any alnum "+:_-"))))
                    nil t)
             collect
             (let* ((target (match-string-no-properties 2))
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
                       target
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
  (setq file-path (or file-path minaduki//file-name (buffer-file-name)))
  (cond
   ;; Using `derived-mode-p' maybe adds 3 seconds per call to the
   ;; cache build when there are a million links. At that point 3
   ;; seconds is probably not that much of a deal.
   ((derived-mode-p 'org-mode)
    (append
     (org-roam--extract-links-org file-path)
     ;; Tracking this as links, as always; we should probably look at
     ;; Org-roam v2 and how it handles Org 9.5 citations.
     (when (featurep 'oc)
       (minaduki-extract/citation file-path))))
   ((derived-mode-p 'markdown-mode)
    (append
     ;; This one depends on the titles cache having been built.
     ;;
     ;; This comes from how org-roam expects links to contain file
     ;; path information, as that is how it is in Org. When working
     ;; with wiki links, however, that's simply not the case.
     ;;
     ;; (org-roam--extract-links-wiki file-path)

     ;; I won't bother to support Org links in Markdown.
     (org-roam--extract-links-markdown file-path)
     (org-roam--extract-links-pandoc-cite file-path)))))

(defun org-roam--extract-ids (&optional file-path)
  "Extract all IDs within the current buffer.
If FILE-PATH is nil, use the current file."
  (setq file-path (or file-path minaduki//file-name (buffer-file-name)))
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

(cl-defgeneric org-roam--extract-titles-title ()
  "Return title from \"#+title\" of the current buffer."
  nil)

(cl-defmethod org-roam--extract-titles-title (&context (major-mode org-mode))
  "Return title from \"#+title\" in Org mode."
  (let* ((prop (minaduki//org-props '("TITLE")))
         (title (cdr (assoc "TITLE" prop))))
    (when title
      (list title))))

(cl-defmethod org-roam--extract-titles-title (&context (major-mode markdown-mode))
  "Return title from the title front matter property in Markdown."
  (-some--> (minaduki-extract//markdown-props "title")
    (list it)))

(cl-defgeneric org-roam--extract-titles-alias ()
  "Return the aliases from the current buffer."
  nil)

(cl-defmethod org-roam--extract-titles-alias (&context (major-mode org-mode))
  "Return a list of aliases in Org mode.

Reads from the #+alias keyword."
  (condition-case nil
      (minaduki//org-prop "ALIAS")
    (error
     (minaduki//warn
      :error
      "Failed to parse aliases for buffer: %s. Skipping"
      (or minaduki//file-name
          (buffer-file-name))))))

(cl-defmethod org-roam--extract-titles-alias (&context (major-mode markdown-mode))
  "Return the aliases in Markdown.

Reads from the alias prop in the front matter.

alias: [\"alias 1\", \"alias 2\"]"
  (condition-case nil
      (-some-> (minaduki-extract//markdown-props "alias")
        json-parse-string)
    (json-parse-error
     (minaduki//warn
      :error
      "Failed to parse aliases for buffer: %s. Skipping"
      (or minaduki//file-name
          (buffer-file-name))))))

(cl-defgeneric org-roam--extract-titles-headline ()
  "Extract the first headline as the document title."
  nil)

;; Function body from md-roam's `org-roam--extract-titles-mdheadline'
(cl-defmethod org-roam--extract-titles-headline (&context (major-mode markdown-mode))
  "Extract the first headline as a title in Markdown mode."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           ;; Converted from md-roam's `md-roam-regex-headline'
           (rx (or
                ;; Case 1:
                ;;
                ;; foo-bar    or    foo-bar
                ;; =======          -------
                ;;
                ;; Ensure the line before the heading text consists of
                ;; only whitespaces to exclude front matter openers
                ;; (md-roam uses "\s" which actually stands for a
                ;; space; I'm guessing that's a mistake)
                (seq bol (zero-or-more whitespace) "\n"
                     (group (zero-or-more nonl) eol) "\n"
                     (group bol (one-or-more (any "=-")) eol))
                ;; Case 2: "# Heading" style
                (seq (group bol (one-or-more "#") " ")
                     (group (zero-or-more nonl) eol))))
           nil t)
      (list (or (match-string-no-properties 1)
                (match-string-no-properties 4))))))

(cl-defmethod org-roam--extract-titles-headline (&context (major-mode org-mode))
  "Extract the first headline as a title in Org mode."
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

(defun org-roam--extract-titles ()
  "Extract the titles from current buffer.

This extracts the aliases plus either the title or the first
headline."
  (org-with-wide-buffer
   (-uniq (append (or (org-roam--extract-titles-title)
                      (org-roam--extract-titles-headline))
                  (org-roam--extract-titles-alias)))))

;; TODO: use project root
(defun org-roam--extract-tags-all-directories (file)
  "Extract tags from using the directory path FILE.
All sub-directories relative to `org-directory' are used as tags."
  (when-let ((dir-relative (file-name-directory
                            (f-relative file (f-expand org-directory)))))
    (f-split dir-relative)))

(defun org-roam--extract-tags-last-directory (file)
  "Extract tags from using the directory path FILE.
The final directory component is used as a tag."
  (when-let ((dir-relative (file-name-directory
                            (f-relative file (f-expand org-directory)))))
    (last (f-split dir-relative))))

(defun org-roam--extract-tags-first-directory (file)
  "Extract tags from path FILE.
The first directory component after `org-directory' is used as a
tag."
  (when-let ((dir-relative (file-name-directory
                            (f-relative file (f-expand org-directory)))))
    (list (car (f-split dir-relative)))))

(defun org-roam--extract-tags-prop (_file)
  "Extract tags from the current buffer's \"#+roam_tags\" global property.

This also extracts from the #+tags[] property, which is what Hugo expects."
  (condition-case nil
      (append (org-roam--extract-prop-as-list "ROAM_TAGS")
              ;; Extracting hugo style #+tags[].
              ;; Concept from http://www.sidpatil.com/posts/org-roam-and-hugo-tags/
              ;; (The fact that you simply need to change the prop it uses.)
              (org-roam--extract-prop-as-list "TAGS[]"))
    (error
     (minaduki//warn
      :error
      "Failed to parse tags for buffer: %s. Skipping"
      (or minaduki//file-name
          (buffer-file-name))))))

(defun org-roam--extract-tags-vanilla (_file)
  "Extract vanilla `org-mode' tags.
This includes all tags used in the buffer."
  (org-set-regexps-and-options 'tags-only)
  (-flatten (org-get-buffer-tags)))

(defun org-roam--extract-tags (&optional file)
  "Extract tags from the current buffer.

If file-path FILE is non-nil, use it to determine the directory tags.

Tags are obtained via:

1. Directory tags: Relative to `org-directory': each folder
   path is considered a tag.
2. The key #+roam_tags."
  (let* ((file (or file (buffer-file-name (buffer-base-buffer))))
         (tags (->> minaduki/tag-sources
                    (mapcan (lambda (it) (funcall it file)))
                    -uniq)))
    (cond
     ((not org-roam-tag-sort)
      tags)
     ((listp org-roam-tag-sort)
      (apply #'cl-sort tags org-roam-tag-sort))
     (t
      (cl-sort tags #'string-lessp :key #'downcase)))))

;; Modified from md-roam's `org-roam--extract-tags-md-buffer'
(defun minaduki-extract/tags-hashtag (&optional _file)
  ;; This is referred to "Zettlr style" in `md-roam'; as I've never
  ;; used Zettlr, I should probably not claim that this is like Zettlr.
  "Extracts tags written with hashtags.

Tags are specified like this:

    #tag1 #tag-with-hyphen #tag_with_underscore"
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "\\([^/s]\\)\\([#][[:alnum:]_-]+\\)" nil t)
             when (match-string-no-properties 2)
             collect it)))

;; Modified from md-roam's `org-roam--extract-tags-md-frontmatter'
;;
;; Right now this doesn't actually read YAML because there is no YAML
;; parser in Emacs Lisp, apart from maybe
;; https://github.com/syohex/emacs-libyaml.
(defun minaduki-extract/tags-hashtag-frontmatter (&optional _file)
  "Extract hashtags in a YAML frontmatter.

Tags are specified like this at the beginning of the buffer:

    ---
    tags: #tag1 #tag-with-hyphen #tag_with_underscore
    ---"
  (save-excursion
    (goto-char (point-min))
    ;; FIXME: extract this into a `with-front-matter' macro
    (-when-let* ((start
                  ;; The beginning of the frontmatter, which has to be at the
                  ;; beginning of the buffer (before char position 4).
                  (re-search-forward "^---$" 4 t))
                 (end
                  ;; The end of the frontmatter
                  (re-search-forward "^---$" nil t)))
      (save-restriction
        (narrow-to-region start end)
        (minaduki-extract/tags-hashtag)))))

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

(defun minaduki-extract//process-ref (ref)
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

(cl-defgeneric minaduki-extract/refs ()
  "Extract all refs statements from the current buffer.

Return value: ((TYPE . KEY) (TYPE . KEY) ...)"
  nil)

(cl-defmethod minaduki-extract/refs (&context (major-mode org-mode))
  "Extract all refs statements in Org mode."
  (let (refs)
    (pcase-dolist
        (`(,_ . ,roam-key)
         (minaduki//org-props '("ROAM_KEY")))
      (pcase roam-key
        ('nil nil)
        ((pred string-empty-p)
         (user-error "Org property #+roam_key cannot be empty"))
        (ref
         (when-let ((r (minaduki-extract//process-ref ref)))
           (push r refs)))))
    refs))

(cl-defmethod minaduki-extract/refs (&context (major-mode markdown-mode))
  "Extract all refs statements in Markdown.

Refs are specified in the roam_key: prop in the front matter and
is always assumed to be a cite key. URL keys are not yet supported."
  (-some--> (minaduki-extract//markdown-props "roam_key")
    (list (cons "cite" it))))

(provide 'minaduki-extract)
;;; minaduki-extract.el ends here
