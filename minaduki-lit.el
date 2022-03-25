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
