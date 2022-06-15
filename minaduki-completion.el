;;; minaduki-completion.el --- Completion utilities -*- coding: utf-8; lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Completion utilities.
;;
;;; Code:

(require 'dash)
(require 'f)

(require 'minaduki-vars)
(require 'minaduki-utils)
(require 'minaduki-db)
(require 'minaduki-lit)

;;;; Completion utils
(defun minaduki//get-title-path-completions ()
  ;; TODO: include headlines with IDs. Might have to think about how a
  ;; headline entry or file entry would be represented.
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
  (let* ((file-nodes (minaduki-db/query [:select [file titles tags meta]
                                         :from files]))
         (id-nodes (minaduki-db/query
                    [:select [ids:id ids:title files:titles files:meta]
                     :from ids
                     :left :join files
                     :on (= files:file ids:file)]))
         rows)
    (cl-loop for x in file-nodes
             do (dolist (title (elt x 1))
                  (push (list :path (elt x 0)
                              :title title
                              :tags (elt x 2)
                              :meta (elt x 3))
                        rows)))
    (cl-loop for x in id-nodes
             do (push (list :path (elt x 0)
                            :title (format "* %s - %s"
                                           (car (elt x 2))
                                           (elt x 1))
                            :tags nil
                            :meta (elt x 3)
                            :id? t)
                      rows))
    (setq rows (sort rows
                     (lambda (a b)
                       (time-less-p
                        (plist-get (plist-get a :meta) :mtime)
                        (plist-get (plist-get b :meta) :mtime)))))
    (cl-loop for row in rows
             collect (let ((path (plist-get row :path))
                           (title (plist-get row :title))
                           (tags (plist-get row :tags)))
                       (cons (-> (minaduki//add-tag-string title tags)
                                 (propertize :metadata `((path . ,path)
                                                         (title . ,title)
                                                         (tags . ,tags))))
                             row)))))

(defun minaduki//get-ref-path-completions (&optional arg filter)
  "Return an alist of refs to absolute path of Org-roam files.

When called interactively (i.e. when ARG is 1), formats the car
of the completion-candidates with extra information: title, tags,
and type \(when `org-roam-include-type-in-ref-path-completions'
is non-nil).

When called with a `C-u' prefix (i.e. when ARG is 4), forces the
default format without the formatting.

FILTER can either be a string or a function:

- If it is a string, it should be the type of refs to include as
  candidates \(e.g. \"cite\", \"website\", etc.)

- If it is a function, it should be the name of a function that
  takes three arguments: the type, the ref, and the file of the
  current candidate. It should return t if that candidate is to
  be included as a candidate."
  (let ((rows (minaduki-db/query
               [:select [refs:type refs:ref refs:file files:title files:tags]
                :from files
                :left :join refs :on (= files:file refs:file)
                :where refs:file :is :not :null]))
        completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,type ,ref ,file-path ,title ,tags) row))
        (when (pcase filter
                ('nil t)
                ((pred stringp) (string= type filter))
                ((pred functionp) (funcall filter type ref file-path))
                (wrong-type (signal 'wrong-type-argument
                                    `((stringp functionp)
                                      ,wrong-type))))
          (let ((k (if (eq arg 1)
                       (concat
                        (when org-roam-include-type-in-ref-path-completions
                          (format "{%s} " type))
                        (minaduki//add-tag-string (format "%s (%s)" title ref)
                                                  tags))
                     ref))
                (v (list :path file-path :type type :ref ref)))
            (push (cons k v) completions)))))))

;; `orb--get-non-ref-path-completions'
(defun minaduki-completion//get-non-literature ()
  "Return a list of non-literature notes for completion.

Each note is a list (STR :path PATH :title TITLE), where STR is
displayed in `completing-read'."
  (let* ((rows (minaduki-db/query
                [:select [file title tags]
                 :from files
                 :left :join refs :on (= files:file refs:file)
                 :where refs:file :is :null]))
         completions)
    (dolist (row rows)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (concat
                  (when tags
                    (format "(%s) " (s-join org-roam-tag-separator tags)))
                  title))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))
    completions))

;;;; `completing-read' completions
(defun minaduki-completion//mark-category (seq category)
  "Mark SEQ as being in CATEGORY.

Return a collection (as defined by `completing-read') that is the
same as passing SEQ to `completing-read' except with category
information attached.

See Info node `(elisp) Programmed Completion' for the official
documentation on this system.

Category information is conveyed by having collection functions
respond metadata when Emacs asks for it. This has begun being
utilized by packages such as Marginalia to display different
information for different types of completion entries, or by
Embark to create what are in effect context menus."
  (lambda (str pred action)
    (pcase action
      ('metadata
       `(metadata (category . ,category)))
      (_
       (all-completions str seq pred)))))

(cl-defun minaduki-completion//read-note
    (&key initial-input completions filter-fn (prompt "Note: "))
  "Read a note from the repository.

INITIAL-INPUT: passed to `completing-read'.

COMPLETIONS: if non-nil a list of note entries in the format
returned by `minaduki//get-title-path-completions'. When nil,
`minaduki//get-title-path-completions' will be queried.

FILTER-FN: completions will pass through this function first
before being prompted for selection.

PROMPT: the prompt to use during completion. Default: \"Note: \""
  (let* ((completions (--> (or completions (minaduki//get-title-path-completions))
                           (if filter-fn
                               (funcall filter-fn it)
                             it)))
         (selection (completing-read prompt
                                     (minaduki-completion//mark-category
                                      completions 'note)
                                     nil nil initial-input)))
    (or (cdr (assoc selection completions))
        ;; When there is no existing match, the entered text is both
        ;; the title and the path.
        ;;
        ;; TODO: the path should be resolved relative to `org-directory'
        ;;       (unless it's a url or an absolute path)
        `(:title ,selection :path ,(s-trim selection) :new? t))))

(defvar minaduki-completion//read-list-entry//citekey nil
  "Let-bind this variable to use `org-cite-insert' on a particular citekey.

For example:

  (let ((minaduki-completion//read-list-entry//citekey \"iso20041201\"))
    (org-cite-insert nil))")

(defun minaduki--format-lit-entry (entry)
  "Format ENTRY for display."
  (concat
   (minaduki--ensure-width
    (min 400 (* 0.3 (frame-pixel-width)))
    (format "%-4.4s %s%s %s"
            (or (gethash "date" entry)
                (gethash "year" entry)
                "")
            (or (-some-> (gethash "todo" entry)
                  (concat " "))
                "")
            (or (-some--> (gethash "type" entry)
                  (concat "@" it)
                  (propertize it 'face 'minaduki-type))
                "")
            (gethash "author" entry "")))
   (format "  %s %s"
           (gethash "title" entry "")
           (or (--> (gethash "tags" entry)
                    (--map (concat "#" it) it)
                    (s-join " " it)
                    (propertize it 'face 'minaduki-tag))
               ""))))

(cl-defun minaduki-completion//read-lit-entry
    (multiple &key (prompt "Entry: "))
  "Read a literature entry and return its citekey.)))

If `minaduki-completion//read-list-entry//citekey' is non-nil,
return that instead. This allows us to call `org-cite-insert'
without prompting.

MULTIPLE: if non-nil, try to read multiple values with
`completing-read-multiple'. This allows this function to be used
for the `minaduki' org-cite insert processor.

PROMPT: the text shown in the prompt."
  (when minaduki-completion//read-list-entry//citekey
    (cl-return-from minaduki-completion//read-lit-entry
      ;; org-cite expects a list if it asked for one
      (if multiple
          (list minaduki-completion//read-list-entry//citekey)
        minaduki-completion//read-list-entry//citekey)))
  (let* ((entries (->> (minaduki-db/query [:select [props] :from keys])
                       (mapcar #'car)))
         (alist (--map (cons (map-elt it "key")
                             (minaduki--format-lit-entry it))
                       entries))
         (completions (map-values alist)))
    (-some->> (if multiple
                  (completing-read-multiple prompt completions)
                (completing-read prompt completions))
      (--map (car (rassoc it alist))))))

;;;; `completion-at-point' completions

(defun minaduki-completion/everywhere ()
  "`completion-at-point' function for word at point.
This is active when `org-roam-completion-everywhere' is non-nil."
  (when (and org-roam-completion-everywhere
             (thing-at-point 'word))
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties start end)))
      (list start end
            (--> (completion-table-dynamic
                  ;; Get our own completion request string
                  (lambda (_)
                    (->> (minaduki-db/query
                          [:select [titles] :from files])
                         -flatten
                         (--remove (string= prefix it)))))
                 (completion-table-case-fold it (not org-roam-completion-ignore-case)))
            :exit-function (lambda (str _status)
                             (delete-char (- (length str)))
                             (insert "[[roam:" str "]]"))))))

(defun minaduki-completion/tags-at-point ()
  "`completion-at-point' function for Org-roam tags."
  (let ((end (point))
        (start (point)))
    (when (looking-back "^#\\+roam_tags:.*" (line-beginning-position))
      (when (looking-at "\\>")
        (setq start (save-excursion
                      (skip-syntax-backward "w")
                      (point))
              end (point)))
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (--> (completion-table-dynamic
                    ;; Get our own completion request string
                    (lambda (_)
                      (->> (minaduki-db//fetch-all-tags)
                           (--remove (string= prefix it)))))
                   (completion-table-case-fold it (not org-roam-completion-ignore-case)))
              :exit-function (lambda (str _status)
                               (delete-char (- (length str)))
                               (insert "\"" str "\"")))))))

(provide 'minaduki-completion)

;;; minaduki-completion.el ends here
