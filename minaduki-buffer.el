;;; minaduki-buffer.el --- Metadata buffer -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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

;; The backlinks buffer.
;;
;; minaduki-buffer/activate shows the buffer; if the buffer doesn't
;; exist beforehand, it will create it and start the right major mode
;; and minor mode (org-mode and minaduki-buffer/mode (for keybinds)).
;;
;; The content is rendered by minaduki-buffer/update (through
;; minaduki-buffer/update-maybe), which is triggered in
;; `post-command-hook' (registered by find-file-hook)

;;; Code:
;;;; Library Requires
(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ol)
(require 'org-element)

(require 'org-roam-db)
(require 'kisaragi-notes-utils)
(require 'minaduki-vars)
(require 'org-roam-extract)

(defvar org-link-frame-setup)
(defvar org-return-follows-link)

(declare-function minaduki//find-file         "org-roam")
(declare-function org-roam-link-get-path      "kisaragi-notes-wikilink")

(defcustom minaduki-buffer/position 'right
  "Where the metadata buffer should be placed.

Valid values are

- `left',
- `right',
- `top',
- `bottom',

or a function returning one of the above."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom)
                 function)
  :group 'minaduki)

(defcustom minaduki-buffer/width 0.33
  "Width of the metadata buffer.

Has an effect if and only if `minaduki-buffer/position' is `left' or `right'."
  :type 'number
  :group 'minaduki)

(defcustom minaduki-buffer/height 0.27
  "Height of the metadata buffer.

Has an effect if and only if `minaduki-buffer/position' is `top' or `bottom'."
  :type 'number
  :group 'minaduki)

(defcustom minaduki-buffer/hidden-tags nil
  "Tags that should not be inserted into the metadata buffer."
  :type '(repeat string)
  :group 'minaduki)

(defcustom minaduki-buffer/name "*minaduki*"
  "Name of the metadata buffer."
  :type 'string
  :group 'minaduki)

(defcustom minaduki-buffer/before-insert-hook nil
  "Hook run in the metadata buffer before new content is inserted."
  :type 'hook
  :group 'minaduki)

(defcustom minaduki-buffer/after-insert-hook
  '(minaduki-buffer//restore-point)
  "Hook run in the metadata buffer after new content is inserted."
  :type 'hook
  :group 'minaduki)

(defcustom minaduki-buffer/window-parameters nil
  "Additional window parameters for the `minaduki-buffer' side window.
For example: (setq minaduki-buffer/window-parameters '((no-other-window . t)))"
  :type '(alist)
  :group 'minaduki)

(defvar minaduki-buffer//current nil
  "Currently displayed file in `org-roam' buffer.")

(defun minaduki-buffer//find-file (file)
  "Open FILE in the other window."
  (setq file (expand-file-name file))
  (let ((last-window org-roam-last-window))
    (if (window-valid-p last-window)
        (progn (with-selected-window last-window
                 (minaduki//find-file file))
               (select-window last-window))
      (minaduki//find-file file))))

(defun minaduki-buffer//insert-title ()
  "Insert the minaduki-buffer title."
  (insert (propertize (minaduki-db//fetch-title
                       (buffer-file-name minaduki-buffer//current))
                      'font-lock-face
                      'org-document-title)))

(defun minaduki-buffer//open-at-point ()
  "Open an Org-roam link or visit the text previewed at point.

When point is on an Org-roam link, open the link in the Org-roam window.
When point is on the Org-roam preview text, open the link in the Org-roam
window, and navigate to the point.
This function hooks into `org-open-at-point' via `org-open-at-point-functions'."
  (cond
   ;; Org-roam link
   ((let* ((context (org-element-context))
           (path (org-element-property :path context)))
      (when (and (eq (org-element-type context) 'link)
                 (org-roam--org-roam-file-p path))
        (minaduki-buffer//find-file path)
        (org-show-context)
        t)))
   ;; Org-roam preview text
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (minaduki-buffer//find-file file-from)
      (goto-char p)
      (org-show-context)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(define-minor-mode minaduki-buffer/mode
  "Minor mode for a local keymap in the backlinks buffer.

\\{minaduki-buffer/mode-map}"
  :lighter " Backlinks"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [mouse-1] 'org-open-at-point)
            (define-key map (kbd "RET") 'org-open-at-point)
            map)
  ;; We use a minor mode just for the keymap. It's not meant to be
  ;; turned off.
  ;;
  ;; Content is rendered in `minaduki-buffer/update'.
  (setq-local org-return-follows-link t)
  (add-hook 'post-command-hook
            #'minaduki-buffer//save-point
            nil :local)
  (add-hook 'org-open-at-point-functions
            #'minaduki-buffer//open-at-point
            nil :local))

;;;; Saving cursor position

(defvar minaduki-buffer//point-map (make-hash-table :test #'equal)
  "A hash table storing cursor position in the backlinks buffer.")

(defun minaduki-buffer//save-point ()
  "Save cursor position in backlinks buffer for the current file."
  (with-current-buffer minaduki-buffer/name
    (puthash (buffer-file-name minaduki-buffer//current)
             (point)
             minaduki-buffer//point-map)))

(defun minaduki-buffer//restore-point ()
  "Restore last visited point in backlinks buffer for this file."
  (with-selected-window (get-buffer-window minaduki-buffer/name)
    (let ((saved-point (gethash (buffer-file-name minaduki-buffer//current)
                                minaduki-buffer//point-map)))
      (when (and (integerp saved-point)
                 (<= (point-min) saved-point (point-max)))
        (goto-char saved-point)))))

;;;; Inserting backlinks

(cl-defun minaduki-buffer//insert-backlinks (&key cite? filter (heading "Backlink"))
  "Insert the minaduki-buffer backlinks string for the current buffer.

Customized:

1. If FILTER is nil, only display backlinks that, when passed to
FILTER, returns non-nil.

Each backlink is a list: (LINK-FILE TO-FILE PLIST), where
LINK-FILE is the link origin,
TO-FILE is the target (in this case always the current file),
and PLIST contains some properties about the link's context.

2. HEADING is the \"* 3 Backlinks\" string in the backlinks buffer.

It should be an English noun because it'll be turned into a
plural automatically.

3. CITE? controls whether to get backlinks to the current file,
or to this file's ROAM_KEY.

4. Nothing is inserted when there are no backlinks.

5. Tags are shown for each entry, except for those in
   `minaduki-buffer/hidden-tags'.

6. Links in titles are removed."
  (let (props file-from)
    (when-let* ((file-path (buffer-file-name minaduki-buffer//current))
                (titles-and-refs
                 (with-current-buffer minaduki-buffer//current
                   (cons (org-roam--extract-titles)
                         (minaduki-extract/refs))))
                (backlinks
                 (if cite?
                     (mapcan #'minaduki-db//fetch-backlinks
                             (-map #'cdr (cdr titles-and-refs)))
                   (minaduki-db//fetch-backlinks
                    (push file-path (car titles-and-refs)))))
                (backlinks (if filter (-filter filter backlinks) backlinks))
                (backlink-groups (--group-by (nth 0 it) backlinks)))
      ;; The heading
      (insert (let ((l (length backlinks)))
                (format "\n\n* %d %s"
                        l (minaduki-buffer//pluralize heading l))))
      ;; Backlinks
      ;; Links from the same group originate from the same file
      (dolist (group backlink-groups)
        ;; Each group looks like:
        ;; (GROUP (FILE-FROM FILE-TO PROPS))
        ;; Where PROPS is a plist of props.
        (setq file-from (car group))
        (setq props (mapcar (lambda (row) (nth 2 row)) (cdr group)))
        (setq props (seq-sort-by (lambda (p) (plist-get p :point)) #'< props))
        (insert "\n\n** "
                ;; title link
                (minaduki/format-link :target file-from
                                      :desc (minaduki//remove-org-links
                                             (minaduki-db//fetch-title file-from)))
                ;; tags
                (or (-some->> (minaduki-db/query [:select tags :from tags
                                                  :where (= file $s1)]
                                                 file-from)
                      caar
                      (--remove (member it minaduki-buffer/hidden-tags))
                      (--map (s-replace " " "_" (downcase it)))
                      (s-join ":")
                      (format "  :%s:"))
                    ""))
        (dolist (prop props)
          (insert
           (format "\n\n/%s/\n\n"
                   (or (-some--> (plist-get prop :outline)
                         (string-join it " › ")
                         (minaduki-buffer/expand-links it file-from)
                         (format "Top › %s" it))
                       "Top")))
          (when-let ((content (plist-get prop :content)))
            (insert
             (--> (minaduki-buffer/expand-links content file-from)
                  s-trim
                  (if (= ?* (elt it 0))
                      (concat "  " it)
                    it)
                  (propertize
                   it
                   'help-echo "mouse-1: visit backlinked note"
                   'file-from file-from
                   'file-from-point (plist-get prop :point))))))))))

(defun minaduki-buffer//insert-cite-backlinks ()
  "Insert ref backlinks.

I'm calling it \"Citation Backlinks\" because I want to
distinguish between Org-roam's ref backlinks and backlinks that
come from Reflections."
  (minaduki-buffer//insert-backlinks
   :heading "Citation Backlink"
   :cite? t))

(defun minaduki-buffer//insert-reflection-backlinks ()
  "Insert backlinks from the \"reflections\" directory."
  (minaduki-buffer//insert-backlinks
   :filter (pcase-lambda (`(,from ,_to ,_plist))
             (string-match-p "reflection/" from))
   :heading "Reflection Backlink"))

(defun minaduki-buffer//insert-diary-backlinks ()
  "Insert backlinks from the \"diary\" directory."
  (minaduki-buffer//insert-backlinks
   :filter (pcase-lambda (`(,from ,_to ,_plist))
             (string-match-p "diary/" from))
   :heading "Diary Backlink"))

(defun minaduki-buffer//insert-other-backlinks ()
  "Insert backlinks that are not from reflections or diary entries."
  (minaduki-buffer//insert-backlinks
   :filter (pcase-lambda (`(,from ,_to ,_plist))
             (not (or (string-match-p "reflection/" from)
                      (string-match-p "diary/" from))))
   :heading "Internal Backlink"))

(defun minaduki-buffer//pluralize (string number)
  "Conditionally pluralize STRING if NUMBER is above 1."
  (let ((l (pcase number
             ((pred (listp)) (length number))
             ((pred (integerp)) number)
             (wrong-type (signal 'wrong-type-argument
                                 `((listp integerp)
                                   ,wrong-type))))))
    (concat string (when (> l 1) "s"))))

(defun minaduki-buffer/expand-links (content orig-path)
  "Crawl CONTENT for relative links and corrects them to be correctly displayed.
ORIG-PATH is the path where the CONTENT originated."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let (link link-type)
      (while (re-search-forward org-roam--org-link-bracket-typed-re (point-max) t)
        (setq link-type (match-string 1)
              link (match-string 2))
        (when (and (string-equal link-type "file")
                   (f-relative-p link))
          (replace-match (org-roam-link-get-path (expand-file-name link (file-name-directory orig-path)))
                         nil t nil 2))))
    (buffer-string)))

(defun minaduki-buffer/update ()
  "Render the backlinks buffer."
  (interactive)
  (minaduki-db//ensure-built)
  (let* ((source-org-directory org-directory))
    (with-current-buffer minaduki-buffer/name
      ;; When dir-locals.el is used to override org-directory,
      ;; minaduki-buffer should have a different local org-directory and
      ;; default-directory, as relative links are relative from the overridden
      ;; org-directory.
      (setq-local org-directory source-org-directory)
      (setq-local default-directory source-org-directory)
      ;; Locally overwrite the file opening function to re-use the
      ;; last window org-roam was called from
      (setq-local org-link-frame-setup
                  (cons '(file . minaduki//find-file) org-link-frame-setup))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (run-hooks 'minaduki-buffer/before-insert-hook)
        (minaduki-buffer//insert-title)
        (minaduki-buffer//insert-cite-backlinks)
        (minaduki-buffer//insert-reflection-backlinks)
        (minaduki-buffer//insert-diary-backlinks)
        (minaduki-buffer//insert-other-backlinks)
        ;; HACK: we should figure out we have no backlinks directly
        (unless (< 2 (s-count-matches "\n" (buffer-string)))
          (insert "\n\n/No backlinks/"))
        (minaduki-buffer//restore-point)
        (run-hooks 'minaduki-buffer/after-insert-hook)
        (read-only-mode 1)))))

(cl-defun minaduki-buffer//update-maybe (&key redisplay)
  "Reconstruct the backlinks buffer.

This needs to be quick or infrequent, because this is run at
`post-command-hook'. If REDISPLAY, force an update no matter
what."
  (let ((buffer (window-buffer)))
    (when (and (or redisplay
                   (not (eq minaduki-buffer//current buffer)))
               (minaduki-buffer/visible?)
               (buffer-file-name buffer)
               (minaduki-db//file-present? (buffer-file-name buffer)))
      (setq minaduki-buffer//current buffer)
      (minaduki-buffer/update))))

;;;; Toggling the org-roam buffer
(defun minaduki-buffer/visible? ()
  "Is the metadata buffer currently visible?"
  (declare (side-effect-free t))
  (get-buffer-window minaduki-buffer/name))

(defun minaduki-buffer//set-width (width)
  "Set the width of the current window to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun minaduki-buffer//set-height (height)
  "Set the height of the current window to HEIGHT."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (h (max height window-min-height)))
      (cond
       ((> (window-height) h)
        (shrink-window  (- (window-height) h)))
       ((< (window-height) h)
        (enlarge-window (- h (window-height))))))))

(defun minaduki-buffer/activate ()
  "Activate display of the `minaduki-buffer'."
  (interactive)
  (setq org-roam-last-window (get-buffer-window))
  ;; Set up the window
  (let ((position (if (functionp minaduki-buffer/position)
                      (funcall minaduki-buffer/position)
                    minaduki-buffer/position)))
    (save-selected-window
      (-> (get-buffer-create minaduki-buffer/name)
          (display-buffer-in-side-window
           `((side . ,position)
             (window-parameters . ,minaduki-buffer/window-parameters)))
          (select-window))
      (pcase position
        ((or 'right 'left)
         (minaduki-buffer//set-width
          (round (* (frame-width)  minaduki-buffer/width))))
        ((or 'top  'bottom)
         (minaduki-buffer//set-height
          (round (* (frame-height) minaduki-buffer/height)))))))
  ;; Set up the buffer
  (with-current-buffer minaduki-buffer/name
    (org-mode)
    (minaduki-buffer/mode)))

(defun minaduki-buffer/deactivate ()
  "Deactivate display of the `minaduki-buffer'."
  (interactive)
  (setq org-roam-last-window (get-buffer-window))
  (delete-window (get-buffer-window minaduki-buffer/name)))

(defun minaduki-buffer/toggle-display ()
  "Toggle display of the `minaduki-buffer'."
  (interactive)
  (if (minaduki-buffer/visible?)
      (minaduki-buffer/deactivate)
    (minaduki-buffer/activate)))

(provide 'minaduki-buffer)

;;; minaduki-buffer.el ends here
