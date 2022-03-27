;;; minaduki-lit.el --- Literature note management -*- lexical-binding: t -*-

;;; Commentary:

;; This has roughly the same goal as org-roam-bibtex and
;; bibtex-completion, but with more of a focus on managing and easily
;; adding sources. Sources are articles, books, etc., or just anything
;; you'd like to have a literature note for.
;;
;; Literature entries (sources) are extracted from the bibliography
;; (in minaduki-extract.el).
;;
;; The bibliography can currently only be Org files. They should look
;; like this:
;;
;;   * title
;;   :PROPERTIES:
;;   :custom_id:  abc123
;;   :author:  some author
;;   :END:
;;
;; Which defines an entry with the key "abc123", title "title", and
;; author "some author".
;;
;; Every heading with the custom_id prop (customizable with
;; `minaduki-lit/key-prop') in a file listed in
;; `minaduki-lit/bibliography' is considered a literature entry.
;;
;; TODO: support saving entries in JSON, maybe even in CSL-JSON
;; TODO: support extracting entries from bibtex. This would truly
;; replace bibtex-completion.
;; TODO: command to add sources, modify a source, or remove a source

;;; Code:

(require 'dash)

(require 'minaduki-utils)
(require 'minaduki-vars)

;; (declare-function parsebib-find-next-item "parsebib")
;; (declare-function parsebib-read-entry "parsebib")

;;;; Type definition

(defvar minaduki-lit//cache nil)

(cl-defun minaduki-lit/source (&key author type title key sources tags others)
  "Return a hash table representing a source.

AUTHOR: the main entity responsible for the source.
TYPE: the type of the source, like \"book\".
TITLE: the name or title of the source.
KEY: the identifying string, like bibtex's ID.
SOURCES: a list of paths or urls that can be used to visit the
source.
TAGS: a list of tags.

OTHERS: other key -> value pairs."
  (let ((obj (make-hash-table :test #'equal)))
    (when author
      (puthash "author" author obj))
    (when type
      (puthash "type" type obj))
    (when title
      (puthash "title" title obj))
    (when key
      (puthash "key" key obj))
    (when sources
      (puthash "sources" (cl-coerce sources 'vector) obj))
    (when tags
      (puthash "tags" (cl-coerce tags 'vector) obj))
    (map-do
     (lambda (k v)
       (puthash (symbol-name k) v obj))
     others)
    obj))

;;;; JSON <-> vector of hash tables
;; (defun minaduki-lit/read-sources (file)
;;   "Parse FILE, a JSON file, into a list of sources."
;;   (catch 'ret
;;     (unless (f-exists? file)
;;       (throw 'ret nil))
;;     (with-temp-buffer
;;       (let ((format-alist nil)
;;             (after-insert-file-functions nil))
;;         (insert-file-contents file))
;;       (json-parse-buffer))))

;; (defun minaduki-lit/write-sources (source-list file)
;;   "Write SOURCE-LIST into FILE as JSON."
;;   (with-temp-file file
;;     (insert (json-serialize source-list))
;;     ;; This is re-parsing and re-writing it again, but (a) I want it
;;     ;; pretty printed because Emacs chokes on long lines and (b) this
;;     ;; function should not on a hot path anyways. I think.
;;     (json-pretty-print-buffer)))

;;;; Reading from Org
(defun minaduki-lit/key-at-point ()
  "Return the key of the literature entry at point."
  (when (eq major-mode 'org-mode)
    (let ((value (org-entry-get nil minaduki-lit/key-prop t)))
      (when (and value
                 (not (string= "" value)))
        value))))

(defun minaduki-lit/generate-key-at-point ()
  "Generate a key for the headline at point."
  (unless (org-entry-get nil minaduki-lit/key-prop)
    (let* ((author
            (-some->> (org-entry-get nil "author")
              (s-replace-all '((" " . "")
                               ("," . "")
                               ("/" . "")
                               ("?" . "")))
              downcase))
           (date
            (-some->> (or (org-entry-get nil "date")
                          (org-entry-get nil "year"))
              (s-replace "--" "–")
              (s-replace "-" "")
              (s-replace "–" "--")
              ;; this should handle ISO 8601 timestamps
              (s-replace-regexp "T[[:digit:]].*" "")))
           (new-id
            (concat author (or date ""))))
      (unless (and author date)
        (setq new-id (read-string "The currently generated ID is too general. Make it more specific: " new-id)))
      (org-entry-put nil minaduki-lit/key-prop new-id))))

(defun minaduki-lit/insert-new-entry-from-url (url)
  "Fetch information from URL and insert a new literature entry.

The entry is an Org heading.

If the author or publish date cannot be determined, ask the user
to fill them in."
  (let (dom)
    (message "Retrieving entry from %s..." url)
    (when-let ((buf (url-retrieve-synchronously url :silent)))
      ;; Extract the DOM first
      (with-current-buffer buf
        (decode-coding-region (point-min) (point-max) 'utf-8)
        (goto-char (point-min))
        ;; Move cursor after the headers
        (eww-parse-headers)
        (setq dom (libxml-parse-html-region (point) (point-max))))
      (let (title author publishdate)
        ;; Parse information out of it
        (setq title (or
                     ;; <meta name="title" content="...">
                     ;; Youtube puts "- YouTube" in <title>, so I want
                     ;; this first.
                     (-some--> (dom-by-tag dom 'meta)
                       (--first (equal (dom-attr it 'name) "title") it)
                       (dom-attr it 'content))
                     ;; Open Graph
                     ;; Also often without the site suffix
                     (-some--> (dom-by-tag dom 'meta)
                       (--first (equal (dom-attr it 'property) "og:title") it)
                       (dom-attr it 'content))
                     ;; <title>
                     (dom-texts (car (dom-by-tag dom 'title))))
              author (or
                      ;; <meta name="author" content="...">
                      (-some--> (dom-by-tag dom 'meta)
                        (--first (equal "author" (dom-attr it 'name)) it)
                        (dom-attr it 'content))
                      ;; <meta name="cXenseParse:author" content="...">
                      ;; nippon.com uses this
                      (-some--> (dom-by-tag dom 'meta)
                        (--first (equal "cXenseParse:author" (dom-attr it 'name)) it)
                        (dom-attr it 'content))
                      ;; WordPress entry header
                      ;; <body><div class="entry-meta">...<a rel="author" href=...>
                      (-some--> (dom-by-class dom "entry-meta")
                        (dom-by-tag it 'a)
                        (--first (equal "author" (dom-attr it 'rel)) it)
                        (dom-text it))
                      ;; YouTube stores this in a <div> in <body>...
                      ;; And yes, it's in a <span> for some reason
                      (-some--> (dom-by-tag dom 'span)
                        (--first (equal "author" (dom-attr it 'itemprop)) it)
                        dom-children
                        (--first (equal "name" (dom-attr it 'itemprop)) it)
                        (dom-attr it 'content)))
              publishdate (or
                           ;; Open Graph
                           ;; <meta property="article:published_time"
                           ;;       content="2019-08-29T09:54:00-04:00" />
                           (-some--> (dom-by-tag dom 'meta)
                             (--first (equal "article:published_time"
                                             (dom-attr it 'property))
                                      it)
                             (dom-attr it 'content))
                           ;; WordPress entry header
                           ;; <body><div class="entry-meta">...<time class="entry-date"...>
                           (-some--> (dom-by-class dom "entry-meta")
                             (dom-by-tag it 'time)
                             (--first (equal "entry-date" (dom-attr it 'class)) it)
                             (dom-text it))
                           ;; YouTube
                           (-some--> (dom-by-tag dom 'meta)
                             (--first (equal "datePublished" (dom-attr it 'itemprop)) it)
                             (dom-attr it 'content))))
        (unless (eq ?\n (char-before))
          (insert "\n"))
        (insert (format "%s %s\n"
                        (make-string (1+ (or (org-current-level)
                                             0))
                                     ?*)
                        title))
        (org-entry-put nil "url" url)
        (org-entry-put nil "author" author)
        (org-entry-put nil "date" publishdate)
        (message "Retrieving entry from %s...done" url)
        (dolist (prop '("url" "author" "date"))
          (let ((value (org-read-property-value prop)))
            (unless (or (null value)
                        (string= value ""))
              (org-entry-put nil prop value))))
        (minaduki-lit/generate-key-at-point)))))

;;;; Migration
;; (defun minaduki-lit/read-sources-from-bibtex (bibtex-file)
;;   "Parse a BIBTEX-FILE into our format."
;;   (with-temp-buffer
;;     (insert-file-contents bibtex-file)
;;     (goto-char (point-min))
;;     (cl-loop for entry-type = (parsebib-find-next-item)
;;              while entry-type
;;              collect (let ((entry
;;                             (--map (cons (intern (car it))
;;                                          (cdr it))
;;                                    (parsebib-read-entry entry-type (point)))))
;;                        (let-alist entry
;;                          (minaduki-lit/source
;;                           :author (minaduki//remove-curly .author)
;;                           :type .=type=
;;                           :key .=key=
;;                           :title (minaduki//remove-curly .title)
;;                           :tags
;;                           (-some->> .keywords
;;                             minaduki//remove-curly
;;                             (s-split ",")
;;                             (-map #'s-trim))
;;                           :sources (-non-nil
;;                                     (mapcar #'minaduki//remove-curly
;;                                             (list .link .url)))
;;                           :others
;;                           (cl-loop for (k . v) in entry
;;                                    unless (member k '(author =type= =key= title keywords url link))
;;                                    collect (cons k (minaduki//remove-curly v)))))))))

;; (defun minaduki-lit/migrate-from-bibtex ()
;;   "Migrate from .bib files."
;;   (minaduki-lit/write-sources
;;    (cl-loop for (_ . bib) in bibtex-completion-bibliography
;;             vconcat (minaduki-lit/read-sources-from-bibtex bib))
;;    minaduki-lit/source-json))

;;;; The search interface

(defun minaduki-lit/format-source (source)
  "Format SOURCE for display."
  (s-format "${date:4} ${todo}@${type} ${author} - ${title:100} ${tags}"
            (lambda (key table)
              (let* ((split (s-split ":" key))
                     (width (cadr split))
                     (real-key (car split)))
                (let ((value (gethash real-key table "")))
                  (cond
                   ((equal key "todo")
                    (unless (equal value "")
                      (setq value (concat value " "))))
                   ((equal key "tags")
                    (setq value (->> value
                                     (--map (concat "#" it))
                                     (s-join " "))))
                   (t
                    (setq value (format "%s" value))))
                  (when width
                    (setq width (string-to-number width))
                    (setq value (concat (minaduki//truncate-string width value "")
                                        (make-string
                                         (max 0
                                              (- width
                                                 (string-width value)))
                                         ?\s))))
                  value)))
            source))

(provide 'minaduki-lit)

;;; minaduki-lit.el ends here
