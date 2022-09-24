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

(require 'marginalia nil t)
;; Keeping the `require' at top level allows the byte compiler to
;; see the definition of `marginalia--fields' so that it doesn't
;; report errors for `minaduki-completion--annotate-note'.
(when (featurep 'marginalia)
  (defvar marginalia-annotator-registry)
  (add-to-list 'marginalia-annotator-registry
               '(note minaduki-completion--annotate-note none)))

(defun minaduki-completion--annotate-note (cand)
  "Marginalia annotation for note entries.

CAND is the entry in the completion. Metadata is passed through
text properties to distinguish between entries with the same
title."
  (when (featurep 'marginalia)
    (-when-let (metadata (get-text-property 0 :metadata cand))
      (let-alist metadata
        (marginalia--fields
         ((when .tags
            (format "(%s)" (s-join "," .tags)))
          :width 30 :face 'marginalia-list)
         ((f-relative .path org-directory)
          :truncate 40 :face 'marginalia-file-name))))))

;;;; Completion utils
(defun minaduki//get-title-path-completions ()
  ;; TODO: include headlines with IDs. Might have to think about how a
  ;; headline entry or file entry would be represented.
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
  (let* ((file-nodes (minaduki-db/query
                      [:select [files:file
                                files:titles
                                files:tags
                                files:meta
                                ;; FIXME: this gives us one entry per
                                ;; key for a file with multiple keys
                                refs:ref
                                refs:type]
                       :from files
                       :left :join refs
                       :on (= refs:file files:file)]))
         (id-nodes (minaduki-db/query
                    [:select
                     [ids:id ids:title files:titles files:meta ids:file]
                     :from ids
                     :left :join files
                     :on (= files:file ids:file)]))
         rows)
    (cl-loop for x in file-nodes
             do (dolist (title (elt x 1))
                  (push
                   (minaduki-node
                    :path (elt x 0)
                    :title title
                    :tags (elt x 2)
                    :meta (elt x 3)
                    :key (elt x 4)
                    :key-type (elt x 5))
                   rows)))
    (cl-loop for x in id-nodes
             do (push
                 (minaduki-node
                  :id (elt x 0)
                  :path (elt x 4)
                  :title (format "* %s - %s"
                                 (car (elt x 2))
                                 (elt x 1))
                  :tags nil
                  :meta (elt x 3))
                 rows))
    (setq rows (sort rows
                     (lambda (a b)
                       (time-less-p
                        (plist-get (oref a meta) :mtime)
                        (plist-get (oref b meta) :mtime)))))
    rows))

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

(defun minaduki--format-lit-entry (entry)
  "Format ENTRY for display."
  (concat
   (minaduki--ensure-display-width
    (min 400 (* 0.3 (frame-pixel-width)))
    (format "%-4.4s %s%s %s"
            (or (gethash "date" entry)
                (gethash "year" entry)
                "")
            (or (-some-> (gethash "todo" entry)
                  (concat " "))
                "")
            (or (-some--> (gethash "type" entry)
                  (concat "" it)
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

(defun minaduki--format-node (node)
  "Format NODE for use in a completion interface."
  (concat
   (minaduki--ensure-display-width
    (min 800 (* 0.5 (frame-pixel-width)))
    (oref node title))
   (or (and (equal (oref node key-type) "cite")
            (--> (oref node key)
                 (concat "@" it)
                 (propertize it 'face 'minaduki-key)
                 (concat it " ")))
       "")
   (or (->> (oref node tags)
            ;; I want a trailing space here
            (--map (-> (concat "#" it)
                       (propertize 'face 'minaduki-tag)
                       (concat " ")))
            string-join)
       "")
   (--> (or (oref node id)
            (f-relative (oref node path) org-directory))
        (propertize it 'face 'minaduki-path))))

(cl-defun minaduki-completion//read-note
    (&key initial-input (prompt "Note: "))
  "Read a note from the repository.

Return the `minaduki-node' object.

INITIAL-INPUT: passed to `completing-read'.

PROMPT: the prompt to use during completion. Default: \"Note: \""
  (minaduki--with-comp-setup
      ((ivy-sort-matches-functions-alist . #'ivy--flx-sort))
    (let* ((entries (minaduki//get-title-path-completions))
           (alist
            (let (ret)
              (dolist (entry entries)
                (push (cons (minaduki--format-node entry) entry) ret))
              (nreverse ret)))
           (completions (map-keys alist))
           (selection
            (completing-read prompt completions nil nil initial-input)))
      (or (cdr (assoc selection alist))
          ;; When there is no existing match, the entered text is both
          ;; the title and the path.
          ;;
          ;; TODO: the path should be resolved relative to `org-directory'
          ;;       (unless it's a url or an absolute path)
          (minaduki-node
           :title selection
           :path (s-trim selection)
           :new? t)))))

(defvar minaduki-completion//read-lit-entry//citekey nil
  "Let-bind this variable to use `org-cite-insert' on a particular citekey.

For example:

  (let ((minaduki-completion//read-lit-entry//citekey \"iso20041201\"))
    (org-cite-insert nil))")

(cl-defun minaduki-completion//read-lit-entry
    (multiple &key (prompt "Entry: "))
  "Read a literature entry and return its citekey.

Always return a list of citekeys.

If `minaduki-completion//read-lit-entry//citekey' is non-nil,
return that instead. This allows us to call `org-cite-insert'
without prompting.

MULTIPLE: if non-nil, try to read multiple values with
`completing-read-multiple'. This allows this function to be used
for the `minaduki' org-cite insert processor.

PROMPT: the text shown in the prompt."
  (when minaduki-completion//read-lit-entry//citekey
    (cl-return-from minaduki-completion//read-lit-entry
      ;; org-cite expects a list if it asked for one
      (if (listp minaduki-completion//read-lit-entry//citekey)
          minaduki-completion//read-lit-entry//citekey
        (list minaduki-completion//read-lit-entry//citekey))))
  (minaduki--with-comp-setup
      ((ivy-sort-functions-alist . nil)
       (ivy-sort-matches-functions-alist . #'ivy--shorter-matches-first))
    (let* ((entries (->> (minaduki-db/query [:select [props] :from keys])
                         (mapcar #'car)))
           (alist (--map (cons (minaduki--format-lit-entry it)
                               (map-elt it "key"))
                         entries))
           (completions (map-keys alist)))
      (-when-let (answer (if multiple
                             (completing-read-multiple prompt completions)
                           (completing-read prompt completions)))
        (unless (listp answer)
          (setq answer (list answer)))
        (--map (cdr (assoc it alist)) answer)))))

;;;; `completion-at-point' completions

(defun minaduki-completion/everywhere ()
  "`completion-at-point' function for word at point.
This is active when `minaduki:completion-everywhere' is non-nil."
  (when (and minaduki:completion-everywhere
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
                 (completion-table-case-fold it (not minaduki:ignore-case-during-completion)))
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
                   (completion-table-case-fold it (not minaduki:ignore-case-during-completion)))
              :exit-function (lambda (str _status)
                               (delete-char (- (length str)))
                               (insert "\"" str "\"")))))))

(provide 'minaduki-completion)

;;; minaduki-completion.el ends here
