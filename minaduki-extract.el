;;; minaduki-extract.el --- Extraction functions  -*- lexical-binding: t; -*-

;;; Commentary:

;; These functions retrieves data from the current buffer.

;;; Code:

(require 'dash)
(require 'f)

(require 'yaml)

(require 'org-element)
(require 'org-id)
(require 'org)

(require 'minaduki-lit)
(require 'minaduki-utils)
(require 'minaduki-vault)
(require 'minaduki-vars)

;; Markdown
(defvar markdown-regex-link-inline)
(defvar markdown-regex-angle-uri)
(defvar markdown-regex-uri)
(defvar markdown-regex-email)
(declare-function markdown-inline-code-at-point-p "markdown-mode")
(declare-function markdown-code-block-at-point-p "markdown-mode")
;; Silence byte compiler on Org < 9.5. These are not present there and
;; aren't used.
(declare-function org-element-citation-reference-parser "org-element")
(defvar org-element-citation-prefix-re)

(defvar-local minaduki-extract::file-prop::cache nil
  "A cache for the current buffer's front matter.")
(defvar minaduki-extract::file-prop::use-cache nil
  "Whether the front matter cache should be used right now.

It should be used when the buffer will be thrown away (like when
creating the db for the first time), and not used when the buffer
is meant to be edited, so this should not be set to non-nil globally.")

(defun minaduki--get-file-prop (prop)
  "Return values of the file level property PROP as a list."
  (minaduki::file-type-case
    (:markdown
     (minaduki::with-front-matter
       ;; TODO: We might have to fold cases here ourselves;
       ;; `case-fold-search' obviously does not affect `equal'
       (if minaduki-extract::file-prop::use-cache
           ;; cached mode: set cache if available
           (unless minaduki-extract::file-prop::cache
             (setq minaduki-extract::file-prop::cache
                   (yaml-parse-string
                    (buffer-string)
                    :object-key-type 'string
                    :sequence-type 'list)))
         ;; uncached mode: clear cache
         (setq minaduki-extract::file-prop::cache nil))
       (let ((frontmatter (if minaduki-extract::file-prop::use-cache
                              minaduki-extract::file-prop::cache
                            (yaml-parse-string
                             (buffer-string)
                             :object-key-type 'string
                             :sequence-type 'list))))
         (ensure-list
          (map-elt frontmatter prop)))))
    (:org
     ;; Don't use `org-collect-keywords', which refuses to read the
     ;; "source" keyword for reasons. `org-element-at-point' also refuses.
     ;; `org-element-keyword-parser' accepts it though.
     (let* ((keyword-pairs (minaduki-org--collect-frontmatter-keywords))
            (values
             (->> keyword-pairs
                  (--filter (equal (upcase prop) (car it)))
                  (--map (cdr it)))))
       ;; Add level 0 property drawer values as well
       (-when-let (v (org-entry-get 1 prop))
         (push v values))
       values))))

(defun minaduki-org--collect-frontmatter-keywords ()
  "Collect all keywords at the top of the buffer.
This has the benefit over `org-collect-keywords' in that it doesn\\='t
care about what the keys are.

The return value is a list of cons cells (KEY . VALUE). If the same
keywords appears multiple times the return value will reflect that
without merging."
  (save-excursion
    (goto-char (point-min))
    (let (this-keyword keywords)
      ;; sometimes org-element-keyword-parser could just succeed in places where
      ;; it shouldn't (or that I don't expect it to). Just this regexp is far
      ;; more reliable at not getting stuck.
      (while (looking-at "^#\\+")
        (setq this-keyword
              (save-match-data
                (ignore-errors
                  (org-element-keyword-parser 0 nil))))
        (forward-line)
        (push (cons (org-element-property :key this-keyword)
                    (org-element-property :value this-keyword))
              keywords))
      keywords)))

(defun minaduki-extract//org-prop-as-list (prop)
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
  (--> (minaduki--get-file-prop prop)
       ;; so that the returned order is the same as in the buffer
       nreverse
       ;; '("a b" "c d")
       (mapcar #'split-string-and-unquote it)
       ;; We have a list of lists at this point. Join them.
       (apply #'append it)))

(defun minaduki-extract//org-citation (&optional file-from)
  "Extract Org 9.5+ citations.

Currently citations are treated as links of the `cite' type, from
FILE-FROM to the key."
  (setq file-from (minaduki::current-file-name (list file-from)))
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
                       (list :outline (minaduki::get-outline-path)
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

(defun minaduki-extract//org-links (&optional file-from)
  "Extract links in current buffer in Org mode format ([[target][desc]]).

Assume links come from FILE-FROM."
  (setq file-from (minaduki::current-file-name (list file-from)))
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
                                 (equal "KEY"
                                        (org-element-property :key elem-at-point)))))
              (goto-char (org-element-property :begin link))
              (let* ((type (minaduki::collate-types (org-element-property :type link)))
                     (path (org-element-property :path link))
                     (content (minaduki-extract//org-links-context))
                     (properties (list :outline (minaduki::get-outline-path)
                                       :point (point)
                                       :content content))
                     (names (pcase type
                              ("id"
                               ;; The cache is not available yet
                               (when-let ((file-from (minaduki-extract//id-file path)))
                                 (list file-from)))
                              ("cite" (list path))
                              ("website" (list path))
                              ("fuzzy" (list (minaduki-obsidian-path path)))
                              ("roam" (list path))
                              (_ (if (or (file-remote-p path)
                                         (minaduki::url? path))
                                     (list path)
                                   (let ((file-maybe (expand-file-name path (file-name-directory file-from))))
                                     (if (f-exists? file-maybe)
                                         (list file-maybe)
                                       (list path))))))))
                (dolist (name names)
                  (when name
                    (push (vector file-from name type properties) links))))))))
      links)))

(defun minaduki-extract//obsidian-links (&optional file-from)
  "Extract Obsidian links from current buffer.

Links are assumed to originate from FILE-FROM."
  (setq file-from (minaduki::current-file-name (list file-from)))
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     while (re-search-forward markdown-regex-wiki-link nil t)
     collect
     (let ((file-to (minaduki-obsidian-path (match-string 3))))
       (vector file-from
               file-to
               "file"
               `(:point ,(point)))))))

;; Modified from md-roam's `md-roam--extract-file-links'
(defun minaduki-extract//markdown-links (&optional file-from)
  "Extract Markdown links from current buffer.

Links are assumed to originate from FILE-FROM."
  (setq file-from (minaduki::current-file-name (list file-from)))
  (save-excursion
    (goto-char (point-min))
    ;; Adadpted from `markdown-get-used-uris'
    (cl-loop while (re-search-forward
                    (concat "\\(?:" markdown-regex-link-inline
                            "\\|" markdown-regex-angle-uri
                            "\\|" markdown-regex-uri
                            "\\|" markdown-regex-email
                            "\\)")
                    nil t)
             unless (or (markdown-inline-code-at-point-p)
                        (markdown-code-block-at-point-p))
             collect
             (let* ((file-to (or (match-string-no-properties 6)
                                 (match-string-no-properties 10)
                                 (match-string-no-properties 12)
                                 (match-string-no-properties 13)))
                    (link-type-raw (url-type
                                    (url-generic-parse-url file-to)))
                    (link-type (minaduki::collate-types
                                (or link-type-raw "file")))
                    end-of-block begin-of-block
                    content)
               ;; Not a file link
               (when link-type-raw
                 ;; https://... -> remove ^https: ->  //...
                 ;; http://... -> remove ^http: ->  //...
                 (setq file-to
                       (replace-regexp-in-string
                        (format "^%s:" (regexp-quote link-type-raw))
                        ""
                        file-to)))
               ;; Is a file link
               (when (equal link-type "file")
                 (setq file-to
                       (file-truename
                        (expand-file-name
                         (url-filename (url-generic-parse-url file-to))
                         (file-name-directory file-from))))
                 (save-excursion
                   ;; get the text block = content around the link as context
                   (unless (eobp)
                     (forward-sentence))
                   (setq end-of-block (point))
                   (backward-sentence)
                   (setq begin-of-block (point))
                   (setq content
                         (buffer-substring-no-properties begin-of-block
                                                         end-of-block))))
               (vector file-from
                       file-to
                       link-type
                       (list :content content
                             :point begin-of-block))))))

;; Modified from md-roam's `md-roam--extract-cite-links'
(defun minaduki-extract//pandoc-citation (&optional file-from)
  "Extract cite links defined like this: @bibkey.

Assume links come from FILE-FROM."
  (setq file-from (minaduki::current-file-name (list file-from)))
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
               (vector file-from
                       target
                       link-type
                       (list :content content :point begin-of-block))))))

(defun minaduki-extract/links (&optional file-from)
  "Extract all links within the current buffer.

Link items are of the form:

    [source dest type properties]

FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-FROM."
  (setq file-from (minaduki::current-file-name (list file-from)))
  (minaduki::file-type-case
    (:org
     (append
      (minaduki-extract//org-links file-from)
      ;; FIXME: citation references should not be tracked as links
      (when (featurep 'oc)
        (minaduki-extract//org-citation file-from))))
    (:markdown
     (append
      ;; I won't bother to support Org links in Markdown.
      (minaduki-extract//markdown-links file-from)
      (minaduki-extract//obsidian-links file-from)
      (minaduki-extract//pandoc-citation file-from)))))

(defun minaduki-extract//headings (&optional file-path)
  "Extract all headings within the current buffer.

If FILE-PATH is nil, use the current file.

Return a list of `minaduki-id' objects in the order they appeared
in the buffer.

Note that this is presently not used for DB caching. Only
headings with an ID are cached (extracted with
`minaduki-extract/ids')."
  ;; TODO: cache all headings, even those without IDs.
  ;;
  ;; This requires minaduki-open to not assume that when the ID is nil, it's a
  ;; file. We should grab the ID information from the ids table.
  (setq file-path (minaduki::current-file-name (list file-path)))
  (let (result)
    (minaduki::file-type-case
      (:markdown
       (goto-char (point-min))
       (while (re-search-forward markdown-regex-header nil t)
         (-let* (((id text level) (minaduki::markdown-matched-heading t)))
           (push (minaduki-id :id id
                              :file file-path
                              :level level
                              :title text
                              :point (point))
                 result))))
      (:org
       (org-map-region
        (lambda ()
          (push (minaduki-id :id (org-entry-get nil "ID")
                             :file file-path
                             :level (org-outline-level)
                             :title (org-entry-get nil "ITEM")
                             :point (point))
                result))
        (point-min) (point-max))))
    (nreverse result)))

(defun minaduki-extract/ids (&optional file-path)
  "Extract all IDs within the current buffer.
If FILE-PATH is nil, use the current file.
Return a list of `minaduki-id' objects."
  (setq file-path (minaduki::current-file-name (list file-path)))
  (let (result)
    (minaduki::file-type-case
      (:markdown
       (goto-char (point-min))
       (while (re-search-forward markdown-regex-header nil t)
         (-when-let* (((id text level) (minaduki::markdown-matched-heading t)))
           (push (minaduki-id :id id
                              :file file-path
                              :level level
                              :title text
                              :point (point))
                 result))))
      (:org
       ;; Handle the file property drawer (outline level 0)
       (goto-char (point-min))
       (when-let ((before-first-heading (= 0 (org-outline-level)))
                  (id (org-entry-get nil "ID")))
         (push (minaduki-id :id id
                            :file file-path
                            :level 0
                            :point (point))
               result))
       ;; Extract every other ID
       (org-map-region
        (lambda ()
          (when-let ((id (org-entry-get nil "ID")))
            (push (minaduki-id :id id
                               :file file-path
                               :level (org-outline-level)
                               :title (org-entry-get nil "ITEM")
                               :point (point))
                  result)))
        (point-min) (point-max))))
    result))

(defun minaduki-extract/main-title ()
  "Return a list containing the main title of the current buffer."
  ;; Outside of a vault, the title is always the file name.
  (if (not (minaduki-vault-in-vault?))
      (list (minaduki::apply-link-abbrev
             (buffer-file-name)))
    (minaduki::file-type-case
      (:org
       (-some-> (car (minaduki--get-file-prop "title"))
         list))
      (:markdown
       (ensure-list
        (car (minaduki--get-file-prop "title")))))))

(defun minaduki-extract/aliases ()
  "Return a list of aliases from the current buffer."
  (minaduki::file-type-case
    (:org
     (minaduki--get-file-prop "ALIAS"))
    (:markdown
     (minaduki--get-file-prop "alias"))))

(defun minaduki-extract/first-headline ()
  "Extract the first headline."
  (minaduki::file-type-case
    (:org
     (save-excursion
       (goto-char (point-min))
       ;; "What happens if a heading star was quoted
       ;; before the first heading?"
       ;; - `org-map-region' also does this
       ;; - Org already breaks badly when you do that;
       ;; precede the heading star with a ",".
       (re-search-forward org-outline-regexp-bol nil t)
       (-some-> (org-entry-get nil "ITEM")
         list)))
    (:markdown
     ;; from md-roam's `org-roam--extract-titles-mdheadline'
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
                   (match-string-no-properties 4))))))))

(defun minaduki-extract--modified ()
  "Extract the last modified timestamp from the current buffer.

This only considers the \"modified\", \"created\", or \"date\"
properties, and does not consider the modification time of the file."
  (when-let ((timestamps (or (minaduki--get-file-prop "modified")
                             (minaduki--get-file-prop "created")
                             (minaduki--get-file-prop "date"))))
    (car timestamps)))

(defun minaduki-extract/titles ()
  "Extract the titles from current buffer.

This extracts the aliases plus either the title or the first
headline."
  (save-excursion
    (save-restriction
      (widen)
      (-uniq
       (append
        (or (minaduki-extract/main-title)
            (minaduki-extract/first-headline)
            (list
             (--> (buffer-file-name)
                  (f-relative it (f-expand (minaduki-vault-main)))
                  f-no-ext)))
        (minaduki-extract/aliases))))))

(defun minaduki-extract//tags/nested-vault (path)
  "If PATH is in a nested vault, return the vault's name as a tag."
  (-some-> (minaduki-vault-closest path)
    f-filename
    list))
(defun minaduki-extract//tags/all-directories (path)
  "Return PATH's relative path in the vault in segments."
  (-some-> (file-name-directory
            (minaduki-vault-path-relative path))
    f-split))
(defun minaduki-extract//tags/first-directory (path)
  "Return PATH's first directory in the vault."
  (-some->> path
    f-parent
    minaduki-vault-path-relative
    f-split
    car
    list
    ;; Files at root
    (remove ".")
    ;; HACK: Currently path-relative will still return an absolute path for
    ;; anything that's not in the main vault, so f-split will put the path
    ;; separator into the results. Just remove it for now.
    (remove (f-path-separator))))
(defun minaduki-extract//tags/last-directory (path)
  "Return PATH's last directory in the vault."
  (-some-> (minaduki-vault-path-relative path)
    f-split
    last))
(defun minaduki-extract//tags/org-tags ()
  "Return all Org tags in current buffer.

Org tags are fetched with `org-get-buffer-tags'."
  (org-set-regexps-and-options 'tags-only)
  (-flatten (org-get-buffer-tags)))
(defun minaduki-extract//tags/org-prop ()
  "Return tags from the #+roam_tags and #+tags[] properties."
  (append (minaduki-extract//org-prop-as-list "ROAM_TAGS")
          ;; Extracting hugo style #+tags[].
          ;;
          ;; The concept that you simply need to change the prop it
          ;; uses is from
          ;; http://www.sidpatil.com/posts/org-roam-and-hugo-tags/
          (minaduki-extract//org-prop-as-list "TAGS[]")))
(defun minaduki-extract//tags/hashtag ()
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
;; Right now this doesn't actually read YAML, I still have to
;; incorporate https://github.com/zkry/yaml.el/.
(defun minaduki-extract//tags/hashtag-frontmatter ()
  "Extract hashtags in a YAML frontmatter.

Tags are specified like this at the beginning of the buffer:

    ---
    tags: #tag1 #tag-with-hyphen #tag_with_underscore
    ---"
  (minaduki::with-front-matter
    (minaduki-extract//tags/hashtag)))

;; TODO: markdown frontmatter "tags: ["abc", "def"]". Just hook up //file-prop.
(defun minaduki-extract/tags (&optional file)
  "Extract file tags from the current buffer.

If file-path FILE is non-nil, use it to determine the directory tags."
  (let* ((file (minaduki::current-file-name (list file)))
         tags)
    (when (memq 'nested-vault minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/nested-vault file) tags)))
    (when (memq 'all-directores minaduki-tag-sources)
      t
      (setq tags (append (minaduki-extract//tags/all-directories file) tags)))
    (when (memq 'first-directory minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/first-directory file) tags)))
    (when (memq 'last-directory minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/last-directory file) tags)))
    (when (memq 'org-tags minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/org-tags) tags)))
    (when (memq 'org-prop minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/org-prop) tags)))
    (when (memq 'hashtag minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/hashtag) tags)))
    (when (memq 'hashtag-frontmatter minaduki-tag-sources)
      (setq tags (append (minaduki-extract//tags/hashtag-frontmatter) tags)))
    (setq tags (cl-remove-duplicates tags))
    (cond
     ((not minaduki-tag-sort)
      tags)
     ((listp minaduki-tag-sort)
      (apply #'cl-sort tags minaduki-tag-sort))
     (t
      (cl-sort tags #'string-lessp :key #'downcase)))))

(defun minaduki-extract//process-ref (ref)
  "Processes REF into its type and path.

Returns a cons cell (TYPE . PATH) if ref is a valid ref.

REF is either a plain link or a plain string. If it's a link, the
protocol is treated as the TYPE (after processing through
`minaduki::collate-types'). Otherwise, REF is assumed to be a cite ref.

The \"cite:\" prefix is removed."
  (let ((type "cite")
        (ref ref))
    (save-match-data
      (when (string-match org-link-plain-re ref)
        (setq type (minaduki::collate-types (match-string 1 ref))
              ref (match-string 2 ref))))
    (cons type
          (s-replace-regexp (rx bos "cite:") "" ref))))

(defun minaduki-extract/note-lit-entry ()
  "Extract one lit entry from the current file.
This is for allowing declaring lit entries within notes directly without
registering them in a bibliography.

If the current file doesn\\='t have both title (obtained as the main
title) and author (declared as the \"author\" property), no lit entry is
created.

Return a `minaduki-lit/entry' object."
  (let ((title (car (minaduki-extract/main-title)))
        (author (car (minaduki--get-file-prop "author")))
        sources)
    (when (and title author)
      (setq sources (mapcan #'minaduki--get-file-prop minaduki--source-keys))
      (minaduki-lit/entry
       :author author
       :date (car (minaduki--get-file-prop "published"))
       :title title
       :key (or (car (minaduki--get-file-prop "key"))
                (car sources))
       :sources sources
       :tags (append
              (minaduki--get-file-prop "tags")
              (minaduki-extract//tags/org-prop))))))

(defun minaduki-extract/lit-entries ()
  "Extract literature entries from this bibliography file.

Return a list of cons cells: (POINT . PROPS), where PROPS look
like `minaduki-lit/entry' objects.

If this file is not in `minaduki-lit/bibliography', this does
nothing and returns nil."
  (pcase major-mode
    (`bibtex-mode (minaduki-lit/parse-entries/bibtex))
    (`json-mode (minaduki-lit/parse-entries/csl-json))
    (_ (minaduki-lit/parse-entries))))

(defun minaduki-extract/refs ()
  "Extract the citekeys this buffer corresponds with.

Return value: ((TYPE . KEY) (TYPE . KEY) ...)

In Org mode, the keys are specified with the #+KEY keyword."
  (minaduki::file-type-case
    (:org
     (let (refs)
       (dolist (key (minaduki--get-file-prop "key"))
         (pcase key
           ('nil nil)
           ((pred string-empty-p)
            (minaduki::warn :warning
              "Org property #+key should not be empty (%s)"
              (f-filename
               (minaduki::current-file-name))))
           (ref
            (when-let ((r (minaduki-extract//process-ref ref)))
              (push r refs)))))
       refs))
    (:markdown
     (-some--> (minaduki--get-file-prop "key")
       car
       (list (cons "cite" it))))))

;;; Links -> common link repr -> Links

(cl-defstruct (minaduki-link (:copier nil)
                             (:constructor minaduki-link))
  desc to)

(defun minaduki::link::write (link type)
  "Write a `minaduki-link' LINK according to TYPE."
  (pcase type
    (:obsidian
     (if (oref link desc)
         (if markdown-wiki-link-alias-first
             (format "[[%s|%s]]" (oref link desc) (oref link to))
           (format "[[%s|%s]]" (oref link to) (oref link desc)))
       (format "[[%s]]" (oref link to))))
    (:org
     (if (oref link desc)
         (format "[[%s][%s]]" (oref link to) (oref link desc))
       (format "[[%s]]" (oref link to))))
    (:markdown
     (if (oref link desc)
         (format "[%s](%s)" (oref link desc) (oref link to))
       (format "<%s>" (oref link to))))))
(defun minaduki::link-markdown::parse ()
  "Return a representation of the native Markdown link at point."
  ;; Adadpted from `markdown-get-used-uris'
  (when (looking-at (concat "\\(?:" markdown-regex-link-inline
                            "\\|" markdown-regex-angle-uri
                            "\\|" markdown-regex-uri
                            "\\|" markdown-regex-email
                            "\\)"))
    (let ((to (or (match-string-no-properties 6)
                  (match-string-no-properties 10)
                  (match-string-no-properties 12)
                  (match-string-no-properties 13)))
          (desc (match-string-no-properties 3)))
      (minaduki-link :to to :desc desc))))
(defun minaduki::link-obsidian::parse ()
  "Return a representation of the obsidian style link at point."
  (when (looking-at minaduki--wikilink-regexp)
    (let* ((first (match-string-no-properties 3))
           (second (match-string-no-properties 5))
           (desc
            (if markdown-wiki-link-alias-first
                first
              second))
           (to
            ;; markdown-wiki-link-alias-first -> to = second || first
            ;; !markdown-wiki-link-alias-first -> to = first
            ;; "alias" = "desc", "PageName" = "target"
            (if markdown-wiki-link-alias-first
                (or second first)
              first)))
      (minaduki-link :to (minaduki-obsidian-path to) :desc desc))))
(defun minaduki::link-org::parse ()
  "Return a simple representation of an Org link at point."
  (when (looking-at org-link-bracket-re)
    (let* ((first (match-string-no-properties 1))
           (second (match-string-no-properties 2))
           (to first)
           (desc second))
      (minaduki-link :to (org-link-expand-abbrev to) :desc desc))))

(provide 'minaduki-extract)
;;; minaduki-extract.el ends here
