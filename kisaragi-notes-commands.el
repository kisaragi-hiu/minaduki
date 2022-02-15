;;; kisaragi-notes-commands.el --- Commands -*- lexical-binding: t -*-

;;; Commentary:

;; Editing commands.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dom)
(require 'dash)
(require 's)

(require 'transient)

(require 'bibtex-completion)

(require 'kisaragi-diary)
(require 'kisaragi-notes-completion)
(require 'kisaragi-notes-utils)
(require 'kisaragi-notes-templates)


(require 'org-roam-extract)
(require 'org-roam-capture)

(defvar org-roam-mode)
(declare-function org-roam-mode "org-roam")

;;;; Local commands

;;;###autoload
(defun minaduki/remove-markup (&optional beg end)
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

(defun minaduki/org-heading-to-file//suffix (&optional dir full? visit?)
  "Write the current heading to a file under DIR.

DIR defaults to current directory (`default-directory').

The name of the created file is based on the heading. By default,
this is the first WORD of the heading; if FULL? is non-nil, this
happens:

- take the entire heading
- dashes and colons are removed,
- then spaces are replaced with dashes,
- and everything is turned into lowercase (except the T in a timestamp).

For example, given a heading \"2020-05-29T00:00:00+0800 my heading\",
when FULL? is non-nil the file name will be
\"20200529T000000+0800-my-heading.org\", otherwise it will be
\"20200529T000000+0800.org\".

When VISIT? is non-nil, visit the new file after creating it.

Interactively, please use the transient command instead."
  (interactive (let ((args (transient-args 'minaduki/org-heading-to-file)))
                 (transient-save)
                 (list (transient-arg-value "--dir=" args)
                       (transient-arg-value "--full" args)
                       (transient-arg-value "--open" args))))
  (let* ((dir (or dir default-directory))
         (title (org-entry-get nil "ITEM"))
         (filename (->> (if full?
                            title
                          (car (s-split " " title)))
                        (s-replace-regexp (rx (any "-/,:?\"!'\\")) "")
                        (s-replace-regexp " +" "-")
                        downcase
                        (s-replace-regexp (rx (group digit) "t" (group digit))
                                          "\\1T\\2")
                        (format "%s.org")))
         (path (f-join dir filename))
         (content (save-mark-and-excursion
                    (org-mark-subtree)
                    (buffer-substring-no-properties
                     (region-beginning)
                     (region-end)))))
    (with-temp-file path
      (insert content))
    (when visit?
      (find-file path))))

(transient-define-prefix minaduki/org-heading-to-file ()
  "Export heading at point to a file."
  ["Options"
   ("-d" "Directory to export to" "--dir=" transient-read-directory)
   ("-f" "Use the entire heading instead of just the first WORD" "--full")
   ("-v" "Open the exported file" "--open")]
  ["Command"
   ("e" "Export" minaduki/org-heading-to-file//suffix)])

;;;###autoload
(defun org-roam-insert (&optional lowercase completions filter-fn description type)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
TYPE is the type of link to be created. It defaults to \"file\".
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (completions (--> (or completions
                                     (org-roam--get-title-path-completions))
                                 (if filter-fn
                                     (funcall filter-fn it)
                                   it)))
               (title-with-tags (completing-read "File: " completions
                                                 nil nil region-text))
               (res (cdr (assoc title-with-tags completions)))
               (title (or (plist-get res :title)
                          title-with-tags))
               (target-file-path (plist-get res :path))
               (description (or description region-text title))
               (description (if lowercase
                                (downcase description)
                              description)))
          (cond ((and target-file-path
                      (file-exists-p target-file-path))
                 (when region-text
                   (delete-region beg end)
                   (set-marker beg nil)
                   (set-marker end nil))
                 (insert (org-roam-format-link target-file-path description type)))
                (t
                 (let ((minaduki-capture//info `((title . ,title-with-tags)
                                                 (slug . ,(minaduki//title-to-slug title-with-tags))))
                       (minaduki-capture//context 'title))
                   (setq minaduki-capture/additional-template-props (list :region (org-roam-shield-region beg end)
                                                                          :insert-at (point-marker)
                                                                          :link-type type
                                                                          :link-description description
                                                                          :finalize 'insert-link))
                   (minaduki-capture//capture))))
          res))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-insert-immediate (arg &rest args)
  "Find an Org-roam file, and insert a relative org link to it at point.
This variant of `org-roam-insert' inserts the link immediately by
using the template in `minaduki-capture/immediate-template'. The
interactive ARG and ARGS are passed to `org-roam-insert'.
See `org-roam-insert' for details."
  (interactive "P")
  (let ((args (push arg args))
        (minaduki-capture/templates (list minaduki-capture/immediate-template)))
    (apply #'org-roam-insert args)))

;;;###autoload
(defun org-roam-unlinked-references ()
  "Check for unlinked references in the current buffer.

The check here is naive: it uses a regex that detects for
strict (case-insensitive) occurrences of possible titles (see
`org-roam--extract-titles'), and shows them in a buffer. This
means that the results can be noisy, and may not truly indicate
an unlinked reference.

Users are encouraged to think hard about whether items should be
linked, lest the network graph get too crowded.

Requires a version of Ripgrep with PCRE2 support installed, with
the executable 'rg' in variable `exec-path'."
  (interactive)
  (unless (org-roam--org-roam-file-p)
    (user-error "Not in org-roam file"))
  (if (not (executable-find "rg"))
      (error "Cannot find the ripgrep executable \"rg\". Check that it is installed and available on `exec-path'")
    (when (string-match "PCRE2 is not available" (shell-command-to-string "rg --pcre2-version"))
      (error "\"rg\" must be compiled with PCRE2 support"))
    (let* ((titles (org-roam--extract-titles))
           (rg-command (concat "rg -o --vimgrep -P -i "
                               (s-join
                                " "
                                (--map (concat "-g " (s-wrap it "\""))
                                       (org-roam--list-files-search-globs
                                        org-roam-file-extensions)))
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-directory))
           (file-loc (buffer-file-name))
           (buf (get-buffer-create "*org-roam unlinked references*"))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           (result-regex (rx (group (one-or-more anything))
                             ":"
                             (group (one-or-more digit))
                             ":"
                             (group (one-or-more digit))
                             ":"
                             (group (zero-or-more anything)))))
      (pop-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (propertize (car titles) 'font-lock-face 'org-document-title) "\n\n"
                "* Unlinked References\n")
        (dolist (line results)
          (save-match-data
            (when (string-match result-regex line)
              (let ((file (match-string 1 line))
                    (row (match-string 2 line))
                    (col (match-string 3 line))
                    (match (match-string 4 line)))
                (when (and match
                           (member (downcase match) (mapcar #'downcase titles))
                           (not (f-equal-p (expand-file-name file org-directory)
                                           file-loc)))
                  (let ((rowcol (concat row ":" col)))
                    (insert "- "
                            (org-link-make-string (concat "file:" file "::" rowcol)
                                                  (format "[%s] %s" rowcol (or (minaduki-db//fetch-title file)
                                                                               file))))
                    (when (executable-find "sed") ; insert line contents when sed is available
                      (insert " :: "
                              (shell-command-to-string
                               (concat "sed -n "
                                       row
                                       "p "
                                       "\""
                                       file
                                       "\""))))
                    (insert "\n")))))))
        (read-only-mode +1)
        (dolist (title titles)
          (highlight-phrase (downcase title) 'bold-italic))
        (goto-char (point-min))))))

;;;###autoload
(defun org-roam-alias-add ()
  "Add an alias.

Return added alias."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((alias (read-string "Alias: ")))
    (when (string-empty-p alias)
      (user-error "Alias can't be empty"))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward "^\\(#\\+alias:.*\\)" (point-max) t)
            (replace-match (format "#+alias: %s\n\\1" alias)
                           'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+alias: " alias "\n"))))
    (minaduki-db//update-file (buffer-file-name (buffer-base-buffer)))
    alias))

;;;###autoload
(defun org-roam-alias-delete ()
  "Delete an alias from Org-roam file."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (if-let ((aliases (org-roam--extract-titles-alias)))
      (let ((alias (completing-read "Alias: " aliases nil 'require-match)))
        (org-with-point-at 1
          (let ((case-fold-search t))
            (when (search-forward (concat "#+alias: " alias) (point-max) t)
              (delete-region (line-beginning-position)
                             (1+ (line-end-position))))))
        (minaduki-db//update-file (buffer-file-name (buffer-base-buffer))))
    (user-error "No aliases to delete")))

;;;###autoload
(defun org-roam-tag-add ()
  "Add a tag to Org-roam file.

Return added tag."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((all-tags (minaduki-db//fetch-all-tags))
         (tag (completing-read "Tag: " all-tags))
         (file (buffer-file-name (buffer-base-buffer)))
         (existing-tags (org-roam--extract-tags-prop file)))
    (when (string-empty-p tag)
      (user-error "Tag can't be empty"))
    (org-roam--set-global-prop
     "roam_tags"
     (combine-and-quote-strings (seq-uniq (cons tag existing-tags))))
    (minaduki-db//insert-tags 'update)
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
        (minaduki-db//insert-tags 'update))
    (user-error "No tag to delete")))

;;;; Global commands

;;;###autoload
(defun org-roam-switch-to-buffer ()
  "Switch to an existing Org-roam buffer."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (minaduki-db//fetch-title
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
(defun minaduki/new-daily-note (&optional day)
  "Create a new daily note on DAY.

This will create diary/20211129.org on the day 2021-11-29, then
fill it in with the \"daily\" template."
  (interactive)
  (let* ((day (or day (minaduki//today)))
         (now (pcase-let ((`(,y ,m ,d)
                           (mapcar
                            #'string-to-number
                            (cdr (s-match (rx (group (= 4 digit)) "-"
                                              (group (= 2 digit)) "-"
                                              (group (= 2 digit)))
                                          day)))))
                (encode-time `(0 0 0 ,d ,m ,y nil nil nil))))
         (filename (s-replace "-" "" day))
         (ext "org"))
    (find-file (f-join minaduki/diary-directory
                       (concat filename "." ext)))
    (let (;; This is how you pass arguments to org-capture-fill-templates
          ;; It's either this or `org-capture-put'; this is
          ;; less ugly.
          (org-capture-plist (list :default-time now))
          ;; Since we're creating a daily note, this
          ;; variable should not be used.
          (org-extend-today-until 0))
      (insert
       (minaduki-templates//make-note "daily")))))

;;;###autoload
(defun minaduki/new-diary-entry (&optional time)
  "Create a new diary entry in `minaduki/diary-directory'.

The entry will be stored as a file named after the current time
under `minaduki/diary-directory'. Example:

    diary/20211019T233513+0900.org

When TIME is non-nil, create an entry for TIME instead of
`current-time'."
  (interactive)
  (let* ((now (or time (current-time)))
         (filename (format-time-string "%Y%m%dT%H%M%S%z" now))
         (title (format-time-string "%FT%T%z" now))
         ;; Put this here so if we allow different templates later
         ;; it's easier to change
         (ext "org"))
    (find-file (f-join minaduki/diary-directory
                       (concat filename "." ext)))
    (insert (concat "#+title: " title "\n"))))

;;;###autoload
(defun minaduki/open-diary-entry ()
  "Open a diary entry.

By default, open one from today. With a \\[universal-argument],
prompt to select a day first.

When there are multiple diary entries, prompt for selection.

Diary entries are files in `minaduki/diary-directory' that
are named with a YYYYMMDD prefix (optionally with dashes)."
  (declare (interactive-only kisaragi-diary//visit-entry-for-day))
  (interactive)
  (let ((day
         ;; Why not `cond': if we're in the calendar buffer but our cursor
         ;; is not on a date (so `calendar-cursor-to-date' is nil), we want
         ;; to fall back to the next case. `cond' doesn't do that.
         (or (and (derived-mode-p 'calendar-mode)
                  (-some-> (calendar-cursor-to-date)
                    minaduki//date/calendar.el->ymd))

             (and current-prefix-arg
                  (minaduki//read-date "Visit diary entry from day:"))

             (minaduki//today))))
    (if-let ((file (minaduki//find-entry-for-day day)))
        (find-file file)
      (and (y-or-n-p (format "No entry from %s. Create one? " day))
           (minaduki/new-daily-note day)))))

;;;###autoload
(defun minaduki/open-diary-entry-yesterday ()
  "Open a diary entry from yesterday."
  (interactive)
  (let ((day (minaduki//today -1)))
    (if-let ((file (minaduki//find-entry-for-day day)))
        (find-file file)
      (and (y-or-n-p (format "No entry from %s. Create one? " day))
           (minaduki/new-daily-note day)))))

;;;###autoload
(defun minaduki/open-template ()
  "Open a template in `minaduki/templates-directory' for edit."
  (interactive)
  ;; Setting `default-directory' to (a) skip passing the directory to
  ;; `f-relative' and `f-expand', and (b) make sure each entry points
  ;; to the right file as relative links. Without this, we have to
  ;; settle for not setting the category correctly.
  (minaduki//find-file
   (minaduki-templates//read-template "Open template: ")))

(defun minaduki/capture-template ()
  "Capture a template."
  (interactive)
  (-> (minaduki-templates//read-template "Template: ")
      minaduki-templates//capture
      minaduki//find-file))

;;;###autoload
(defun minaduki/open-non-literature-note (&optional initial-prompt)
  ;; `orb-find-non-ref-file'
  "Open a note that isn't a literature note.

INITIAL-PROMPT is the initial title prompt. See
`org-roam-find-files' and `minaduki-completion//get-non-literature' for
details."
  (interactive)
  (minaduki/open
   (minaduki-completion//read-note
    initial-prompt
    (car (minaduki-completion//get-non-literature)))))

;;;###autoload
(defun minaduki/open-directory ()
  "Open `org-directory'."
  (interactive)
  (find-file org-directory))

;;;###autoload
(defun minaduki/open-literature-note (interactive? &optional filter)
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
  (interactive (list t))
  (let* ((completions (org-roam--get-ref-path-completions interactive? filter))
         (ref (completing-read "Literature note: " completions nil t))
         (file (-> (cdr (assoc ref completions))
                   (plist-get :path))))
    (minaduki//find-file file)))

;;;###autoload
(defun minaduki/open-random-note ()
  ;; Originally `org-roam-random-note'
  "Open a random note."
  (interactive)
  (find-file (seq-random-elt (org-roam--list-all-files))))

;;;###autoload
(defun minaduki/open-index ()
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
                 (car (minaduki-db//query-title "Index"))))))
    (if (and index (f-exists? index))
        (minaduki//find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (minaduki/open "Index")))))

;;;###autoload
(defun minaduki/open (&optional entry)
  ;; Some usages:
  ;; (minaduki/open title)
  ;; (minaduki/open
  ;;   (minaduki-completion//read-note initial-input))
  "Find and open the note ENTRY.

ENTRY is a plist (:path PATH :title TITLE). It can also be a
string, in which case it refers to a (maybe non-existent) note
with it as the title.

Interactively, provide a list of notes to search and select from.
If a note with the entered title does not exist, create a new
one."
  (interactive
   (list (minaduki-completion//read-note)))
  (unless org-roam-mode (org-roam-mode))
  (when (stringp entry)
    (setq entry
          (list :path (car (minaduki-db//query-title entry))
                :title entry)))
  (let ((file-path (plist-get entry :path))
        (title (plist-get entry :title)))
    (if file-path
        (minaduki//find-file file-path)
      ;; FIXME: Hardcodes choice of Org
      (with-current-buffer (find-file-noselect
                            (-> (minaduki//title-to-slug title)
                                (f-expand org-directory)
                                (concat ".org")))
        (insert "#+TITLE: " title "\n")
        (pop-to-buffer-same-window (current-buffer))))))

;;;; Literature note actions

(defun orb-note-actions-copy-citekey (citekey)
  "Save note's citation key to `kill-ring' and copy it to clipboard.
CITEKEY is a list whose car is a citation key."
  (with-temp-buffer
    (insert (car citekey))
    (copy-region-as-kill (point-min) (point-max))))

;;;; Actions

(defvar minaduki/global-commands
  '(("Open or create a note"              . minaduki/open)
    ("Open notes directory"               . minaduki/open-directory)
    ("Open or create a template"          . minaduki/open-template)
    ("Create a new diary entry"           . minaduki/new-diary-entry)
    ("Create a new note from a template" . minaduki/capture-template)
    ("Create a new note with the \"daily\" template" . minaduki/new-daily-note)
    ("Open the index file"                . minaduki/open-index)
    ("Open a literature note"             . minaduki/open-literature-note)
    ("Open a non-literature note"         . minaduki/open-non-literature-note)
    ("Open a random note"                 . minaduki/open-random-note)
    ("Switch to a buffer visiting a note" . org-roam-switch-to-buffer)
    ("Refresh cache"                      . minaduki-db/build-cache))
  "Global commands shown in `minaduki/command-palette'.

List of (DISPLAY-NAME . COMMAND) pairs.")

(defun minaduki/command-palette ()
  "Command palette."
  (declare (interactive-only command-execute))
  (interactive)
  (let* ((candidates minaduki/global-commands)
         (selection (completing-read "Minaduki Global Command: " candidates))
         (func (cdr (assoc selection candidates)))
         (prefix-arg current-prefix-arg))
    (command-execute func)))

(defvar minaduki/literature-note-actions
  '(("Open PDF file(s)" . bibtex-completion-open-pdf)
    ("Add PDF to library" . bibtex-completion-add-pdf-to-library)
    ("Open URL or DOI in browser" . bibtex-completion-open-url-or-doi)
    ("Show record in the bibtex file" . bibtex-completion-show-entry)
    ("Edit notes" . orb-edit-notes)
    ("Save citekey to kill-ring and clipboard" . orb-note-actions-copy-citekey))
  "Commands useful inside a literature note.

List of (DISPLAY-NAME . FUNCTION) pairs. Each function receives
one argument, a list of cite keys.

Equivalent to `orb-note-actions-default'.")

(defun minaduki/literature-note-actions (&optional citekey)
  ;; `orb-note-actions'
  "Prompt for note-related actions on CITEKEY.

CITEKEY is, by default, the first ROAM_KEY in the buffer.

Actions are defined in `minaduki/literature-note-actions'."
  (interactive)
  (-if-let* ((citekey (or citekey (cdar (minaduki-extract/refs)))))
      (let* ((prompt (let ((bibtex-completion-display-formats
                            '((t . "Act on ${author} - ${title} (${year}): ")))
                           ;; This should be local to the `let'.
                           bibtex-completion-display-formats-internal)
                       (bibtex-completion-init)
                       (bibtex-completion-format-entry
                        (bibtex-completion-get-entry citekey)
                        0)))
             (candidates minaduki/literature-note-actions)
             (selection (completing-read prompt candidates))
             (func (cdr (assoc selection candidates))))
        (funcall func (list citekey)))
    (user-error "Could not retrieve the citekey, is ROAM_KEY specified?")))

(provide 'kisaragi-notes-commands)

;;; kisaragi-notes-commands.el ends here
