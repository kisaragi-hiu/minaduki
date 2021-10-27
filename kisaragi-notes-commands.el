;;; kisaragi-notes-commands.el --- Commands -*- lexical-binding: t -*-

;;; Commentary:

;; Editing commands.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dom)
(require 'dash)
(require 's)

(require 'kisaragi-notes-completion)
(require 'kisaragi-notes-utils)
(require 'org-roam-extract)

(defvar org-roam-mode)
(declare-function org-roam-mode "org-roam")

;;;; Local commands

;;;###autoload
(defun kisaragi-notes/remove-markup (&optional beg end)
  "Remove markup between BEG and END."
  (interactive
   (when (region-active-p)
     (list (region-beginning)
           (region-end))))
  (unless (and beg end)
    (user-error "Please select text to remove markup from"))
  (save-restriction
    (narrow-to-region beg end)
    (let ((parsed (org-element-parse-buffer)))
      (delete-region beg end)
      (insert (->> parsed
                ;; Yes, this works.
                dom-texts
                (s-replace-regexp "[ \t]+" " "))))))

;;;###autoload
(defun org-roam-alias-add ()
  "Add an alias to Org-roam file.

Return added alias."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((alias (read-string "Alias: ")))
    (when (string-empty-p alias)
      (user-error "Alias can't be empty"))
    (org-roam--set-global-prop
     "roam_alias"
     (combine-and-quote-strings
      (seq-uniq (cons alias
                      (org-roam--extract-titles-alias)))))
    (org-roam-db--update-file (buffer-file-name (buffer-base-buffer)))
    alias))

;;;###autoload
(defun org-roam-alias-delete ()
  "Delete an alias from Org-roam file."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (if-let ((aliases (org-roam--extract-titles-alias)))
      (let ((alias (completing-read "Alias: " aliases nil 'require-match)))
        (org-roam--set-global-prop
         "roam_alias"
         (combine-and-quote-strings (delete alias aliases)))
        (org-roam-db--update-file (buffer-file-name (buffer-base-buffer))))
    (user-error "No aliases to delete")))

;;;###autoload
(defun org-roam-tag-add ()
  "Add a tag to Org-roam file.

Return added tag."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((all-tags (kisaragi-notes-db//fetch-all-tags))
         (tag (completing-read "Tag: " all-tags))
         (file (buffer-file-name (buffer-base-buffer)))
         (existing-tags (org-roam--extract-tags-prop file)))
    (when (string-empty-p tag)
      (user-error "Tag can't be empty"))
    (org-roam--set-global-prop
     "roam_tags"
     (combine-and-quote-strings (seq-uniq (cons tag existing-tags))))
    (org-roam-db--insert-tags 'update)
    tag))

;;;###autoload
(defun org-roam-tag-delete ()
  "Delete a tag from Org-roam file."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (if-let* ((file (buffer-file-name (buffer-base-buffer)))
            (tags (org-roam--extract-tags-prop file)))
      (let ((tag (completing-read "Tag: " tags nil 'require-match)))
        (org-roam--set-global-prop
         "roam_tags"
         (combine-and-quote-strings (delete tag tags)))
        (org-roam-db--insert-tags 'update))
    (user-error "No tag to delete")))

;;;; Global commands

;;;###autoload
(defun org-roam-switch-to-buffer ()
  "Switch to an existing Org-roam buffer."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (kisaragi-notes-db//fetch-title
                                                 (buffer-file-name buffer))
                                                (buffer-name buffer))
                                            buffer))
                                    roam-buffers)))
    (unless roam-buffers
      (user-error "No roam buffers"))
    (when-let ((name (completing-read "Buffer: " names-and-buffers
                                      nil t)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

;;;###autoload
(defun kisaragi-notes/find-directory ()
  "Open `org-directory'."
  (interactive)
  (find-file org-directory))

;;;###autoload
(defun kisaragi-notes/open-literature-note (interactive? &optional filter)
  ;; Originally `org-roam-find-ref'
  "Open a literature note, allowing search for their ROAM_KEYs.

INTERACTIVE? is passed to `org-roam--get-ref-path-completions'.

FILTER is used to filter results, and can either be a string or a function:

- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)

- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate."
  (let* ((completions (org-roam--get-ref-path-completions interactive? filter))
         (ref (completing-read "Literature note: " completions nil t))
         (file (-> (cdr (assoc ref completions))
                 (plist-get :path))))
    (org-roam--find-file file)))

;;;###autoload
(defun kisaragi-notes/open-random-note ()
  ;; Originally `org-roam-random-note'
  "Open a random note."
  (interactive)
  (find-file (seq-random-elt (org-roam--list-all-files))))

;;;###autoload
(defun kisaragi-notes/open-index ()
  ;; originally `org-roam-jump-to-index'
  "Open the index file.

The index file is specified in this order:

- `org-roam-index-file' (a string or function, see its docstring)
- A note with a title of \"Index\" in `org-directory'"
  (interactive)
  (let ((index (cond
                ((functionp org-roam-index-file)
                 (f-expand (funcall org-roam-index-file)
                           org-directory))
                ((stringp org-roam-index-file)
                 (f-expand org-roam-index-file))
                (t
                 (car (kisaragi-notes-db//fetch-files-by-title "Index"))))))
    (if (and index (f-exists? index))
        (org-roam--find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (kisaragi-notes/open "Index")))))

;;;###autoload
(defun kisaragi-notes/open (&optional entry)
  ;; Some usages:
  ;; (kisaragi-notes/open title)
  ;; (kisaragi-notes/open
  ;;   (kisaragi-notes-completion//read-note initial-input))
  "Find and open the note ENTRY.

ENTRY is a plist (:path PATH :title TITLE). It can also be a
string, in which case it refers to a (maybe non-existent) note
with it as the title.

Interactively, provide a list of notes to search and select from.
If a note with the entered title does not exist, create a new
one."
  (interactive
   (list (kisaragi-notes-completion//read-note)))
  (unless org-roam-mode (org-roam-mode))
  (when (stringp entry)
    (setq entry
          (list :path (car (kisaragi-notes-db//fetch-files-by-title entry))
                :title entry)))
  (let ((file-path (plist-get entry :path))
        (title (plist-get entry :title)))
    (if file-path
        (org-roam--find-file file-path)
      ;; FIXME: Hardcodes choice of Org
      (with-current-buffer (find-file-noselect
                            (-> (kisaragi-notes//title-to-slug title)
                              (f-expand org-directory)
                              (concat ".org")))
        (insert "#+TITLE: " title "\n")
        (pop-to-buffer-same-window (current-buffer))))))

(provide 'kisaragi-notes-commands)

;;; kisaragi-notes-commands.el ends here
