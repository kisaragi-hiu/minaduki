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
(require 's)
(require 'f)

(require 'minaduki-vars)
(require 'minaduki-vault)
(require 'minaduki-utils)
(require 'minaduki-db)
(require 'minaduki-lit)

(declare-function ivy--flx-sort "ivy")
(declare-function ivy--shorter-matches-first "ivy")

(cl-defun minaduki-read:author (&key def (prompt "Author: "))
  "Ask the user using PROMPT to select an author.
DEF is passed to `completing-read'. Pass the current author value
in with this to mimick `org-read-property-value'\\='s behavior
when a value is already present."
  ;; Mimick `org-read-property-value' on its prompt display for the default
  ;; value.
  (when (and def (string-match (rx (group (* any))
                                   ":" (opt " ") eol)
                               prompt))
    ;; We don't want to mutate the input, but this just changes the local
    ;; reference.
    (setq prompt
          ;; `replace-match' returns a new string
          (replace-match
           (format "\\& [%s]" def)
           nil nil prompt 1)))
  (completing-read prompt (minaduki-db::fetch-lit-authors)
                   nil nil nil nil
                   def))

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
   (minaduki::ensure-display-width
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
  (let ((title (concat
                (oref node title)
                (propertize
                 " "
                 'display
                 `(space :align-to (,(min 800 (* 0.5 (frame-pixel-width))))))))
        (cite (and (equal (oref node key-type) "cite")
                   (--> (oref node key)
                        (concat "@" it)
                        (propertize it 'face 'minaduki-key))))
        (tags (->> (oref node tags)
                   (--map (-> (concat "#" it)
                              (propertize 'face 'minaduki-tag)))
                   (s-join " ")))
        (path (-> (minaduki-vault-path-abbrev (oref node path) t)
                  (propertize 'face 'minaduki-path))))
    (s-join " " (list title cite tags path))))

(cl-defun minaduki-read:note
    (&key initial-input (prompt "Note: ") under-path)
  "Read a note that is indexed by Minaduki.

Return the `minaduki-node' object.

INITIAL-INPUT: passed to `completing-read'.
PROMPT: the prompt to use during completion. Default: \"Note: \"
UNDER-PATH: only list nodes under this path for completion."
  (minaduki::with-comp-setup
      ((ivy-sort-matches-functions-alist . #'ivy--flx-sort))
    (minaduki::message "Fetching nodes...")
    (let* ((entries (minaduki-db--fetch-nodes
                     :under-path under-path))
           (alist
            (let (ret)
              (minaduki::loading "Formating nodes..."
                (dolist (entry entries)
                  (push (cons (minaduki--format-node entry) entry) ret)))
              (nreverse ret)))
           (completions (map-keys alist))
           (selection
            (completing-read prompt completions nil nil initial-input)))
      (or (cdr (assoc selection alist))
          ;; When there is no existing match, the entered text is both
          ;; the title and the path.
          ;;
          ;; TODO: the path should be resolved relative to the main vault
          ;;       (unless it's a url or an absolute path)
          (minaduki-node
           :title selection
           :path (s-trim selection)
           :new? t)))))

(defvar minaduki-read:lit-entry::citekey nil
  "Let-bind this variable to use `org-cite-insert' on a particular citekey.

For example:

  (let ((minaduki-read:lit-entry::citekey \"iso20041201\"))
    (org-cite-insert nil))")

(cl-defun minaduki-read:lit-entry
    (multiple &key (prompt "Entry: "))
  "Read a literature entry and return its citekey.

Always return a list of citekeys.

If `minaduki-read:lit-entry::citekey' is non-nil,
return that instead. This allows us to call `org-cite-insert'
without prompting.

MULTIPLE: if non-nil, try to read multiple values with
`completing-read-multiple'. This allows this function to be used
for the `minaduki' org-cite insert processor.

PROMPT: the text shown in the prompt."
  (when minaduki-read:lit-entry::citekey
    (cl-return-from minaduki-read:lit-entry
      ;; org-cite expects a list if it asked for one
      (if (listp minaduki-read:lit-entry::citekey)
          minaduki-read:lit-entry::citekey
        (list minaduki-read:lit-entry::citekey))))
  (minaduki::with-comp-setup
      ((ivy-sort-functions-alist . nil)
       (ivy-sort-matches-functions-alist . #'ivy--shorter-matches-first))
    (let* ((entries (->> (minaduki-db-select "select props from keys")
                         (mapcar #'car)
                         (mapcar #'minaduki-db::parse-value)))
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

(defun minaduki::completing-read-annotation (prompt alist &optional cdr)
  "Prompt to select from a list of options, each with an annotation.

Return the selected option itself. If CDR is non-nil, return the
annotation of the selected option.

The user is prompted with PROMPT.
ALIST maps each option to their annotation string."
  (let (mapping strings selected-cell)
    ;; Map from the actual strings presented to the original values
    (setq mapping (--map
                   (cons (format "%s %s"
                                 (car it)
                                 (propertize
                                  (format "(%s)" (cdr it))
                                  'face 'font-lock-doc-face))
                         it)
                   alist))
    (setq strings (-map #'car mapping))
    (setq selected-cell
          ;; The original (str . doc) mapping
          (cdr (assoc (completing-read prompt strings nil t)
                      mapping)))
    (if cdr
        (cdr selected-cell)
      (car selected-cell))))

;;;; `completion-at-point' completions

(defun minaduki-completion/tags-at-point ()
  "`completion-at-point' function for in-buffer tags."
  (let ((end (point))
        (start (point)))
    (when (looking-back (rx bol
                            "#+" (or "roam_tags" "tags[]") ":"
                            (zero-or-more nonl))
                        (line-beginning-position))
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
                      (->> (minaduki-db::fetch-all-tags)
                           (--remove (string= prefix it)))))
                   (completion-table-case-fold it (not minaduki:ignore-case-during-completion)))
              :exit-function (lambda (str _status)
                               (delete-char (- (length str)))
                               (insert "\"" str "\"")))))))


(provide 'minaduki-completion)

;;; minaduki-completion.el ends here
