;;; minaduki-buffer.el --- Metadata buffer -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, convenience
;; Version: 1.2.3

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

(require 'text-property-search)

(require 'minaduki-db)
(require 'minaduki-utils)
(require 'minaduki-vault)
(require 'minaduki-vars)
(require 'minaduki-extract)

(defvar org-link-frame-setup)
(defvar org-return-follows-link)

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
For example: (setq minaduki-buffer/window-parameters \\='((no-other-window . t)))"
  :type '(alist)
  :group 'minaduki)

(defvar minaduki-buffer//current nil
  "Currently displayed file in `org-roam' buffer.")

(defun minaduki-buffer//find-file (file)
  "Open FILE in the other window."
  (setq file (expand-file-name file))
  (let ((last-window minaduki::last-window))
    (if (window-valid-p last-window)
        (progn (with-selected-window last-window
                 (minaduki::find-file file))
               (select-window last-window))
      (minaduki::find-file file))))

(defun minaduki-buffer//insert-title ()
  "Insert the minaduki-buffer title."
  (-> (minaduki-db::fetch-title
       (buffer-file-name minaduki-buffer//current))
      (propertize 'font-lock-face 'org-document-title)
      insert))

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
                 (minaduki-vault:in-vault? path))
        (minaduki-buffer//find-file path)
        t)))
   ;; Backlinks context
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (minaduki-buffer//find-file file-from)
      (goto-char p)
      t))
   ;; Unlinked references context
   ((-when-let* ((file-from (get-text-property (point) 'file-from))
                 ((line . col) (get-text-property (point) 'file-from-line/col)))
      (minaduki-buffer//find-file file-from)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char col)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(define-minor-mode minaduki-buffer/mode
  "Minor mode for a local keymap in the backlinks buffer.

\\{minaduki-buffer/mode-map}"
  :lighter " Backlinks"
  :interactive nil
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

(defun minaduki//backlinks (&optional type)
  "Return backlinks of TYPE to the current buffer.

TYPE can be:

- `titles': titles and aliases from `minaduki-extract/titles'
- `refs': references to keys from `minaduki-extract/refs', or
- anything else: return both."
  (pcase type
    (`titles (minaduki-db::fetch-backlinks
              (cons (buffer-file-name)
                    (minaduki-extract/titles))))
    (`refs (mapcan
            #'minaduki-db::fetch-backlinks
            (mapcar #'cdr (minaduki-extract/refs))))
    (_ (append (minaduki//backlinks 'titles)
               (minaduki//backlinks 'refs)))))

(defun minaduki//unlinked-references ()
  "Return unlinked references (from the main vault) to the current buffer.

Only references from files without links to the current buffer
are returned."
  (let ((file-path (buffer-file-name))
        (titles (minaduki-extract/titles))
        (refs (mapcar #'cdr (minaduki-extract/refs)))
        (files-linking-here
         (-uniq (mapcar #'car (minaduki//backlinks))))
        (rg-cmd (or (and (boundp 'rg-executable) rg-executable)
                    (executable-find "rg"))))
    (when (and rg-cmd
               file-path
               (or titles refs))
      (with-temp-buffer
        (let ((default-directory (minaduki-vault:main)))
          (apply
           #'call-process
           rg-cmd
           nil '(t nil) nil
           `("--color=ansi"
             "--colors=match:none"
             ;; These colors are used below to mark sections
             "--colors=path:fg:green"
             "--colors=line:fg:blue"
             "--colors=column:fg:yellow"
             "-i"
             "-n"
             "--column"
             ,@(--map
                (format "--glob=!%s" (f-relative it))
                (cons file-path files-linking-here))
             "--heading"
             "--no-config"
             "--fixed-strings"
             ,@(cl-loop for it in (append titles refs)
                        append (list "-e" it))))
          (unless (equal "" (s-trim (buffer-string)))
            ;; Step 1: insert the output
            (minaduki::set-buffer-substring
                (point-min) (point-max)
              (--> (buffer-string)
                   ;; Trim all starting and ending newlines for
                   ;; consistency. All newlines will be replaced with two
                   ;; close parens to close off the (:path) lines.
                   s-trim
                   ;; One is for the `search-forward' (which is me
                   ;; attempting to go faster than forward-line +
                   ;; line-{beginning,end}-position), the other is to be
                   ;; replaced with the close parens.
                   (concat it "\n\n")))
            (goto-char (point-min))
            ;; Step 2: convert Ripgrep's output into `read'able sexp
            (cl-loop
             with start = (point)
             while (search-forward "\n" nil t)
             do
             (let* ((end (match-beginning 0))
                    (line (buffer-substring start end)))
               (pcase line
                 ;; path line
                 ((rx bos
                      "[0m[32m"
                      (let relpath (* any))
                      "[0m")
                  (let ((path (f-expand relpath)))
                    (minaduki::set-buffer-substring start end
                      (format
                       ;; We intentionally do not close
                       ;; it, only doing so when the
                       ;; block is over.
                       "(:path %S :title %S :matches ("
                       path
                       (or (minaduki-db::fetch-title path)
                           relpath)))))
                 ;; matches lines
                 ((rx bos
                      ;; \x1b is ESC; we could write it literally but
                      ;; good luck reading it anywhere else other than
                      ;; Emacs as most editors / viewers do not render
                      ;; control chars in a meaningful way.
                      ;;
                      ;; Case in point: look at the diff for these
                      ;; lines with a tool that isn't Emacs.
                      "\x1b[0m\x1b[34m"
                      (let ln (*? any))
                      "\x1b[0m" ":"
                      "\x1b[0m\x1b[33m"
                      (let col (*? any))
                      "\x1b[0m" ":"
                      (let context (* any)))
                  (minaduki::set-buffer-substring start end
                    (prin1-to-string
                     (list :line ln :column col
                           :context context))))
                 ;; Empty lines between files
                 (_
                  (minaduki::set-buffer-substring start end
                    "))"))))
             do (setq start (point))))
          ;; Step 3: wrap the whole buffer in a set of parens so we
          ;; can read it in one go
          (progn
            (goto-char (point-min))
            (insert "(")
            (goto-char (point-max))
            (insert ")"))
          (goto-char (point-min))
          ;; Step 4: Read!
          (->> (read (current-buffer))
               (--remove (memq 'ignore (plist-get it :matches)))))))))

(cl-defun minaduki-buffer//insert-backlinks (backlinks &key (heading "Backlink"))
  "Insert BACKLINKS.

Get the backlinks with this:

  (with-current-buffer minaduki-buffer//current
    (minaduki//backlinks \\='refs) ; only cite backlinks
    (minaduki//backlinks \\='titles) ; only non-cite backlinks
    (minaduki//backlinks)) ; both

HEADING is the \"* 3 Backlinks\" string in the backlinks buffer.

It should be an English noun because it'll be turned into a
plural automatically.

Nothing is inserted when there are no backlinks.

Tags are shown for each entry, except for those in
`minaduki-buffer/hidden-tags'.

Links in titles are removed."
  (let (props file-from)
    (when-let* ((backlink-groups (--group-by (nth 0 it) backlinks)))
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
                (minaduki::format-link :target file-from
                                      :desc (minaduki::remove-org-links
                                             (minaduki-db::fetch-title file-from)))
                ;; tags
                (or (-some->> (minaduki-db::fetch-tags file-from)
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

(defun minaduki-buffer//insert-unlinked-references (references)
  "Insert unlinked REFERENCES."
  (when references
    (insert "\n\n* Unlinked references\n")
    (cl-loop
     for file in references
     do
     (progn (insert (format "** [[%s][%s]]\n\n"
                            ;; If there is only one match, make this
                            ;; link go point to it
                            (if (= 1 (length (plist-get file :matches)))
                                (format "%s::%s"
                                        (plist-get file :path)
                                        (-> file
                                            (plist-get :matches)
                                            car
                                            (plist-get :line)))
                              (plist-get file :path))
                            (plist-get file :title)))
            (cl-loop for ind in (plist-get file :matches)
                     do (insert
                         (let* ((file file)
                                (ind ind)
                                (path (plist-get file :path))
                                (line (string-to-number
                                       (plist-get ind :line)))
                                (col (string-to-number
                                      (plist-get ind :column))))
                           (-> (format "  %s\n\n"
                                       (plist-get ind :context))
                               (propertize
                                'face 'fixed-pitch
                                'file-from path
                                'file-from-line/col (cons line col))))))))))

(defun minaduki-buffer//insert-tag-references (tag)
  "Insert links to files tagged with TAG."
  (when tag
    (-when-let (references (minaduki-db::fetch-tag-references tag))
      (insert (format "\n\n* Files tagged with /%s/\n" tag))
      (->> (cl-loop for file in references
                    collect (concat "** "
                                    (minaduki::format-link
                                     :target file
                                     :desc (minaduki::remove-org-links
                                            (minaduki-db::fetch-title file)))))
           (s-join "\n")
           insert))))

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
      (while (re-search-forward minaduki::org-link-bracket-typed-re (point-max) t)
        (setq link-type (match-string 1)
              link (match-string 2))
        (when (and (string-equal link-type "file")
                   (f-relative-p link))
          (replace-match (minaduki::convert-path-format (expand-file-name link (file-name-directory orig-path)))
                         nil t nil 2))))
    (buffer-string)))

(defun minaduki-buffer/update ()
  "Render the backlinks buffer."
  (interactive)
  (minaduki-db::ensure-built)
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
                  (cons '(file . minaduki::find-file) org-link-frame-setup))
      (let ((inhibit-read-only t)
            backlinks cite-backlinks
            unlinked-references)
        (with-current-buffer minaduki-buffer//current
          (setq cite-backlinks (minaduki//backlinks 'refs)
                backlinks (minaduki//backlinks 'titles))
          (when (minaduki-vault:in-vault?)
            (setq unlinked-references (minaduki//unlinked-references))))
        (erase-buffer)
        (run-hooks 'minaduki-buffer/before-insert-hook)
        (minaduki-buffer//insert-title)
        (minaduki-buffer//insert-tag-references
         (--> (buffer-file-name minaduki-buffer//current)
              minaduki-db::fetch-title
              downcase))
        (minaduki-buffer//insert-backlinks
         cite-backlinks
         :heading "Citation Backlink")
        (minaduki-buffer//insert-backlinks
         backlinks
         :heading "Backlink")
        (when unlinked-references
          (minaduki-buffer//insert-unlinked-references unlinked-references))
        (unless (or backlinks cite-backlinks unlinked-references)
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
               (minaduki::current-file-name buffer)
               (minaduki-db::file-present? (minaduki::current-file-name buffer)))
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
  (setq minaduki::last-window (get-buffer-window))
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
  (setq minaduki::last-window (get-buffer-window))
  (delete-window (get-buffer-window minaduki-buffer/name)))

(defun minaduki-buffer/toggle-display ()
  "Toggle display of the `minaduki-buffer'."
  (interactive)
  (if (minaduki-buffer/visible?)
      (minaduki-buffer/deactivate)
    (minaduki-buffer/activate)))

(provide 'minaduki-buffer)

;;; minaduki-buffer.el ends here
