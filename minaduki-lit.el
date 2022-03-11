;;; minaduki-lit.el --- Literature note management -*- lexical-binding: t -*-

;;; Commentary:

;; This has roughly the same goal as org-roam-bibtex and
;; bibtex-completion, but with more of a focus on managing and easily
;; adding sources. Sources are articles, books, etc., or just anything
;; you'd like to have a literature note for.

;; - sources.json
;;   - parsed into something
;;   - stored in a local cache?
;; - list sources
;; - add sources
;; - modify sources
;; - remove sources
;;
;; - command to browse sources
;; - actions on sources
;;   - visiting the source's corresponding note
;;     - creating one if one doesn't exist
;;   - open the source's url or file
;;   - show the source's entry in the JSON
;;   - modify the source

;; We can probably use CSL-JSON later, but I'll make up my own format
;; for now.
;;
;; A source has:
;;
;; - a type (string)
;; - a list of "sources" (like file paths and urls)
;; - an ID (called "key"); if this is a url, it's also considered a source.

;;; Code:

(require 'dash)

(require 'kisaragi-notes-utils)

(defcustom minaduki-lit/source-json (f-join org-directory "sources.json")
  "Path to the JSON file that stores sources."
  :group 'minaduki
  :type 'string)

(defcustom minaduki-lit/extra-source-json nil
  "A list of extra JSON files storing sources.

This is useful for having both sources managed in Org mode and
sources managed by Minaduki."
  :group 'minaduki
  :type '(repeat string))

;;;; Type definition

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
(defun minaduki-lit/read-sources (file)
  "Parse FILE, a JSON file, into a list of sources."
  (catch 'ret
    (unless (f-exists? file)
      (throw 'ret nil))
    (with-temp-buffer
      (let ((format-alist nil)
            (after-insert-file-functions nil))
        (insert-file-contents file))
      (json-parse-buffer))))

(defun minaduki-lit/write-sources (source-list file)
  "Write SOURCE-LIST into FILE as JSON."
  (with-temp-file file
    (insert (json-serialize source-list))
    ;; This is re-parsing and re-writing it again, but (a) I want it
    ;; pretty printed because Emacs chokes on long lines and (b) this
    ;; function should not on a hot path anyways. I think.
    (json-pretty-print-buffer)))

(defun minaduki-lit/read-sources-from-org (org-file)
  "Read sources from an ORG-FILE."
  (org-roam-with-file org-file nil
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (re-search-forward "^:bibtex_id:" nil t)
                 collect
                 (let ((props (org-entry-properties)))
                   (setq props
                         (--remove
                          (member (car it)
                                  (->> org-special-properties
                                       (remove "ITEM")
                                       (remove "TODO")))
                          props)
                         props
                         (--map (cons (downcase (car it))
                                      (cdr it))
                                props)
                         props
                         (--map (cons (or (cdr
                                           ;; Key replacements
                                           ;; (ORG_PROP . KEY)
                                           (assoc (car it)
                                                  '(("category" . "type")
                                                    ("bibtex_id" . "key")
                                                    ("item" . "title"))))
                                          (car it))
                                      (cdr it))
                                props))
                   (map-into props '(hash-table :test equal))))))))

;;;; Migration
(defun minaduki-lit/read-sources-from-bibtex (bibtex-file)
  "Parse a BIBTEX-FILE into our format."
  (with-temp-buffer
    (insert-file-contents bibtex-file)
    (goto-char (point-min))
    (cl-loop for entry-type = (parsebib-find-next-item)
             while entry-type
             collect (let ((entry
                            (--map (cons (intern (car it))
                                         (cdr it))
                                   (parsebib-read-entry entry-type (point)))))
                       (let-alist entry
                         (minaduki-lit/source
                          :author (minaduki//remove-curly .author)
                          :type .=type=
                          :key .=key=
                          :title (minaduki//remove-curly .title)
                          :tags
                          (-some->> .keywords
                            minaduki//remove-curly
                            (s-split ",")
                            (-map #'s-trim))
                          :sources (-non-nil
                                    (mapcar #'minaduki//remove-curly
                                            (list .link .url)))
                          :others
                          (cl-loop for (k . v) in entry
                                   unless (member k '(author =type= =key= title keywords url link))
                                   collect (cons k (minaduki//remove-curly v)))))))))

(defun minaduki-lit/migrate-from-bibtex ()
  "Migrate from .bib files."
  (minaduki-lit/write-sources
   (cl-loop for (_ . bib) in bibtex-completion-bibliography
            vconcat (minaduki-lit/read-sources-from-bibtex bib))
   minaduki-lit/source-json))

;;;; The search interface

(defun minaduki-lit/format-source (source)
  "Format SOURCE for display."
  (s-format "${todo:4}${title:100}\t(${type}) ${author} ${tags}"
            (lambda (key table)
              (let* ((split (s-split ":" key))
                     (width (cadr split))
                     (real-key (car split)))
                (let ((value (format "%s" (gethash real-key table ""))))
                  (when width
                    (setq width (string-to-number width))
                    (setq value (concat (minaduki//truncate-string width value)
                                        (make-string
                                         (max 0
                                              (- width
                                                 (string-width value)))
                                         ?\s))))
                  value)))
            source))

(provide 'minaduki-lit)

;;; minaduki-lit.el ends here
