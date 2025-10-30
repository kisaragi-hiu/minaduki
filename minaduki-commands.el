;;; minaduki-commands.el --- Commands -*- lexical-binding: t -*-

;;; Commentary:

;; Editing commands.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dom)
(require 'dash)
(require 's)

(require 'transient)

(require 'minaduki-diary)
(require 'minaduki-completion)
(require 'minaduki-lit)
(require 'minaduki-btn)
(require 'minaduki-buffer)

(require 'minaduki-utils)
(require 'minaduki-vault)
(require 'minaduki-templates)

(require 'minaduki-extract)
(require 'minaduki-db)
(require 'minaduki-bibtex)

(defvar ivy-sort-functions-alist)
(defvar selectrum-should-sort)

;;;; Org-specific local commands

(defun minaduki--cut-commit-metadata (beg end)
  "Cut commit metadata written between BEG and END.
The text will be deleted.
Return ((commit-id . ID) (created-time . ENCODED-TIME) (created-zone . ZONE))."
  (let ((text
         (s-trim (buffer-substring-no-properties beg end)))
        commit-id commit-date)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (setq commit-id (buffer-substring-no-properties
                       (pos-bol) (pos-eol)))
      (when (re-search-forward "CommitDate: " nil t)
        (setq commit-date (buffer-substring-no-properties
                           (point) (pos-eol)))))
    (delete-region beg end)
    (let* ((decoded (parse-time-string commit-date))
           (zone (car (last decoded)))
           (encoded (encode-time decoded)))
      `((commit-id . ,commit-id)
        (created-time . ,encoded)
        (created-zone . ,zone)))))
(defun minaduki-convert-commit-metadata-to-properties (beg end)
  "Convert commit metadata written between BEG and END to Org properties.

Usage:

1. Find the first commit creating this file in
   \\[universal-argument]
   \\[magit-log-buffer-file] (`magit-log-buffer-file' with a
   `universal-argument' in order to turn on \"--follow\"
2. Enter it and copy the commit ID ~ CommitDate lines
3. Paste the lines into this file
4. Line-select these lines
4. Run this command.

The result is that the created time and commit ID for the current entry
are set via `org-entry-put'."
  (interactive "r")
  (let-alist (minaduki--cut-commit-metadata beg end)
    (org-entry-put (point) "commit" .commit-id)
    (org-entry-put (point) "created" (format-time-string
                                      "%FT%T%z"
                                      .created-time
                                      .created-zone))))
(defun minaduki-convert-commit-metadata-to-keywords (beg end)
  "Convert commit metadata written between BEG and END to Org keywords.

Usage:

1. Find the first commit creating this file in
   \\[universal-argument]
   \\[magit-log-buffer-file] (`magit-log-buffer-file' with a
   `universal-argument' in order to turn on \"--follow\"
2. Enter it and copy the commit ID ~ CommitDate lines
3. Paste the lines into this file
4. Line-select these lines
4. Run this command."
  (interactive "r")
  (let-alist (minaduki--cut-commit-metadata beg end)
    (minaduki::set-file-prop "commit" .commit-id)
    (minaduki--set-created-prop .created-time .created-zone)))

(defun minaduki-org//id-new-advice (&rest _args)
  "Update the database if a new Org ID is created."
  (when (and (minaduki-vault-in-vault?)
             (not (eq minaduki-db/update-method 'immediate))
             (not (minaduki-capture/p)))
    (minaduki-db::incremental-update)))

(defun minaduki-org//move-to-row-col (s)
  "Move to row:col if S match the row:col syntax.

To be used with `org-execute-file-search-functions'."
  (when (string-match (rx (group (1+ digit))
                          ":"
                          (group (1+ digit))) s)
    (let ((row (string-to-number (match-string 1 s)))
          (col (string-to-number (match-string 2 s))))
      (org-goto-line row)
      (move-to-column (- col 1))
      t)))

(defun minaduki-cite//follow (datum _)
  "The follow function for Minaduki's Org-cite processor.

This will extract the citation key from DATUM and ask the user
what they want to do with it."
  (let ((key
         ;; Taken from the `basic' processor's follow function
         (if (eq 'citation-reference (org-element-type datum))
             (org-element-property :key datum)
           (pcase (org-cite-get-references datum t)
             (`(,key) key)
             (keys
              (or (completing-read "Select citation key: " keys nil t)
                  (user-error "Aborted")))))))
    (minaduki:local-commands key)))

(defun minaduki-org-set-startup ()
  "Set the STARTUP option for the current buffer."
  (interactive)
  (minaduki::set-file-prop
   "startup"
   (minaduki::completing-read-annotation
    "Startup option: "
    `(("overview" . "fold everything")
      ("nofold" . "show all")
      ("content" . "all headlines")
      ("show2levels" . "2 levels of headlines")
      ("show3levels" . "3 levels of headlines")
      ("show4levels" . "4 levels of headlines")
      ("show5levels" . "5 levels of headlines")
      ("showeverything" . "show everything, even drawers")))))

;;;; Markdown-specific local commands

(defun minaduki-markdown-follow (&optional other)
  "Follow thing at point.

Like `markdown-follow-thing-at-point', but has support for:

- Obsidian links,
- ID links (written as [text](#<ID>), ie. a path starting with a hash)

When OTHER is non-nil (with a \\[universal-argument]),
open in another window instead of in the current one."
  (interactive "P")
  (if (bound-and-true-p minaduki-local-mode)
      (let ((markdown-enable-wiki-links t))
        (when other (other-window 1))
        (cond ((markdown-wiki-link-p)
               (minaduki::find-file (minaduki-obsidian-path (match-string 3))))
              ((markdown-link-p)
               (let ((url (markdown-link-url))
                     expanded)
                 (cond
                  ((s-prefix? "#" url)
                   (minaduki/open-id (substring url 1)))
                  ;; Support link expansion; right now just read from
                  ;; `org-link-abbrev-alist' directly.
                  ((not (equal url
                               (setq expanded (org-link-expand-abbrev url))))
                   (markdown--browse-url expanded))
                  (t
                   (markdown-follow-thing-at-point other)))))
              (t (markdown-follow-thing-at-point other))))
    (markdown-follow-thing-at-point other)))


;;;; Local commands

;;;###autoload
(defalias 'minaduki:toggle-sidebar
  #'minaduki-buffer/toggle-display)

(defun minaduki:id-get-create (&optional new-id)
  "Return the ID to the current heading.

If it doesn't have one yet, assign a generated one.

If NEW-ID is non-nil, assign NEW-ID to be the ID of the heading,
overwriting the existing one if necessary."
  (interactive
   (when current-prefix-arg
     (list (read-string "New ID: "))))
  (when (and new-id (minaduki-db::has-id? new-id))
    (error "ID `%s' already exists in the database" new-id))
  (if new-id
      (minaduki::set-heading-id new-id)
    (or (minaduki::get-heading-id)
        (minaduki::set-heading-id (org-id-new)))))

(defun minaduki-insert-link (target &optional label)
  "Insert a link to TARGET with LABEL.
This dispatches based on the current file type."
  (insert
   (minaduki::format-link :target target :desc label)))

(defun minaduki-insert-file-link (path)
  "Insert a link to PATH."
  (interactive "fInsert link to file: ")
  (minaduki-insert-link path))

(cl-defun minaduki-insert (&key entry lowercase? replace-region?)
  "Insert a link to a note.

This only lists notes from the current vault for selection.

If region is active, the new link uses the selected text as the
description. For example, if the text \"hello world\" is
selected, and the user chooses to insert a link to
./programming.org, the region would be replaced with
\"[[file:programming.org][hello world]]\".

If the note with the provided title does not exist, a new one is
created.

ENTRY: the note entry, a `minaduki-node' object.
LOWERCASE?: if non-nil, the link description will be downcased.
REPLACE-REGION?: whether to replace selected text."
  (interactive
   (list
    :entry nil
    :lowercase? current-prefix-arg
    :replace-region? t))
  ;; 1. Fetch region into desc if active
  ;; 2. Ask for an entry
  ;; 3. Set title, id, path, desc (if applicable)
  ;; 4. Delete region if active (done after asking to be less jarring)
  ;; 5. Create a new note if the entry doesn't exist
  ;; 6. Downcase desc if we should
  ;; 7. Format entry and insert!
  (let (title id path desc)
    (when (and replace-region?
               (region-active-p))
      (setq desc (-> (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))
                     s-trim)))
    (unless entry
      (setq entry (minaduki-read:note
                   :initial-input desc
                   :prompt "Insert link to note: "
                   :under-path (minaduki-vault-closest))))
    (setq title (oref entry title)
          id (oref entry id)
          path (minaduki::ensure-not-file://
                (oref entry path)))
    (if desc
        (delete-active-region)
      (setq desc title))
    ;; We avoid creating a new note if the path is a URL or it refers
    ;; to an existing file.
    ;;
    ;; This also allows inserting references to existing notes whose
    ;; title happens to be a URL without issue.
    (when (and (oref entry new?)
               (not (minaduki::url? path))
               (not (f-exists? path)))
      (setq path
            ;; FIXME: support creating notes in the current vault, not just in
            ;; the main vault. Perhaps this needs support for template paths.
            ;; Maybe this should be called the "default" template. Maybe if the
            ;; entered text is foo:bar we should attempt to create a file based
            ;; on the "foo" template.
            (minaduki/new-concept-note
             :title title
             :visit? nil))
      (minaduki::message "Created new note \"%s\"" title))
    (when lowercase?
      (setq desc (downcase desc)))
    (insert (minaduki::format-link
             :target (or id path)
             :desc desc
             :type (cond (id 'id)
                         (t nil))))))

(defun minaduki:move-file-to-directory ()
  "Move the current file to a new directory."
  (interactive)
  (-when-let* ((file (minaduki::current-file-name))
               (vault (minaduki-vault-closest)))
    (let* ((newdir (read-directory-name
                    "Move current file to directory: "
                    vault nil t))
           (newpath (f-join newdir (f-filename file))))
      (cl-assert (f-descendant-of? newpath vault))
      (rename-file file newpath)
      (setq buffer-file-name newpath)
      (revert-buffer))))

(defun minaduki--format-id (id)
  "Format ID, a `minaduki-id' object, for completion."
  (let ((str (minaduki-id-title id)))
    (-when-let (lvl (minaduki-id-level id))
      (setq str (format "%s %s" (make-string lvl ?*) str)))
    ;; Unfortunately passing data to the caller of `completing-read'
    ;; with a text property can only be done with Ivy.
    (concat str (propertize (format "%S" id) 'invisible t))))

(defun minaduki-insert-local ()
  "Insert a link to a heading in the same file."
  (interactive)
  (-when-let (headings (mapcar #'minaduki--format-id
                               (minaduki-extract//headings)))
    (let (selection
          selected-heading)
      (minaduki::with-comp-setup
          ((ivy-sort-functions-alist . nil)
           (ivy-sort-matches-functions-alist . nil))
        (setq selection (completing-read
                         "Insert link to heading: " headings
                         nil t))
        (with-temp-buffer
          (insert selection)
          (goto-char (point-min))
          (text-property-search-forward 'invisible t)
          (setq selected-heading
                ;; HACK: I wish there's a better way.
                (read
                 (buffer-substring-no-properties (point) (point-max))))))
      (let ((id (save-excursion
                  (goto-char (minaduki-id-point selected-heading))
                  (prog1 (minaduki:id-get-create)
                    ;; Save it into the file & the DB so that the
                    ;; newly inserted link will not be highlighted as
                    ;; an invalid link.
                    (save-buffer)))))
        (minaduki-insert
         :replace-region? t
         :entry (minaduki-node
                 :id id
                 :title (minaduki::remove-org-links
                         (minaduki-id-title selected-heading))))))))

;;;###autoload
(defun minaduki-add-alias ()
  "Add an alias."
  (interactive)
  (let ((alias (read-string "Alias: ")))
    (when (string-empty-p alias)
      (user-error "Alias can't be empty"))
    (org-with-wide-buffer
     (goto-char 1)
     (let ((case-fold-search t))
       (if (re-search-forward "^#\\+alias: .*" nil t)
           (insert "\n")
         ;; Skip past the first block of keywords and property drawer
         (while (and (not (eobp))
                     (looking-at "^[#:]"))
           (if (> (line-end-position) (1- (buffer-size)))
               (progn
                 (end-of-line)
                 (insert "\n"))
             (forward-line)
             (beginning-of-line))))
       (insert "#+alias: " alias)))
    (when (minaduki-vault-in-vault?)
      (minaduki-db::insert-meta 'update))
    alias))

;;;###autoload
(defun minaduki-delete-alias ()
  "Delete an alias."
  (interactive)
  (if-let ((aliases (minaduki-extract/aliases)))
      (let ((alias (completing-read "Alias: " aliases nil 'require-match)))
        (org-with-wide-buffer
         (goto-char 1)
         (let ((case-fold-search t))
           (when (search-forward (concat "#+alias: " alias) (point-max) t)
             (delete-region (line-beginning-position)
                            (1+ (line-end-position))))))
        (when (minaduki-vault-in-vault?)
          (minaduki-db::insert-meta 'update)))
    (user-error "No aliases to delete")))

;;;###autoload
(defun minaduki-add-tag ()
  "Add a tag."
  (interactive)
  (let* ((all-tags (minaduki-db::fetch-all-tags))
         (tag (completing-read "Tag: " all-tags))
         (existing-tags (minaduki-extract//tags/org-prop)))
    (when (string-empty-p tag)
      (user-error "Tag can't be empty"))
    (minaduki::set-file-prop
     "tags[]"
     (combine-and-quote-strings (seq-uniq (cons tag existing-tags))))
    (when (minaduki-vault-in-vault?)
      (minaduki-db::insert-meta 'update))
    tag))

;;;###autoload
(defun minaduki-delete-tag ()
  "Delete a tag from Org-roam file."
  (interactive)
  (if-let* ((tags (minaduki-extract//tags/org-prop)))
      (let ((tag (completing-read "Tag: " tags nil 'require-match)))
        (minaduki::set-file-prop
         "tags[]"
         (combine-and-quote-strings (delete tag tags)))
        (when (minaduki-vault-in-vault?)
          (minaduki-db::insert-meta 'update)))
    (user-error "No tag to delete")))

;;;; Global commands

;;;###autoload
(defun minaduki/fix-broken-links ()
  "List all broken links in a new buffer."
  (interactive)
  (let ((list-buffer (get-buffer-create "*minaduki broken links*"))
        errors)
    ;; Set up the display buffer
    (with-current-buffer list-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (kill-all-local-variables)
        (setq-local buffer-read-only t
                    revert-buffer-function (lambda (&rest _)
                                             (minaduki/fix-broken-links)))))
    ;; Collect missing links
    (let* ((all-files (minaduki-vault-all-files))
           (i 0)
           (length (length all-files)))
      (cl-loop
       for f in all-files
       do
       (cl-incf i)
       (minaduki::message "(%s/%s) Looking for broken links in %s"
                          i length f)
       (minaduki::with-temp-buffer f
         (save-excursion
           (goto-char (point-min))
           (let ((ast (org-element-parse-buffer)))
             (org-element-map ast 'link
               (lambda (l)
                 (let ((file (org-element-property :path l)))
                   (when (and (equal "file" (org-element-property :type l))
                              (not (or (file-exists-p file)
                                       (file-remote-p file))))
                     (push
                      `(,f
                        ,(org-element-property :begin l)
                        ,(format
                          (if (org-element-lineage l '(link))
                              "\"%s\" (image in description) does not exist"
                            "\"%s\" does not exist")
                          file))
                      errors))))))))))
    ;; Insert them into the buffer
    (with-current-buffer list-buffer
      (let ((inhibit-read-only t)
            (count-bounds '(nil . nil)))
        (insert "Click the file names to visit the error.\n"
                "Checkboxes are available for keeping track of which ones are fixed.\n\n")
        ;; "100 broken links (100 to go)\n\n"
        ;; We need to capture the second number's bounds.
        (insert (format "%s broken links ("
                        (length errors)))
        (setf (car count-bounds) (point))
        (insert (format "%s" (length errors)))
        (setf (cdr count-bounds) (point))
        (insert " to go):\n\n")
        (insert
         (cl-loop
          for (file point message) in errors
          concat
          (format
           "%s %s: %s\n"
           (let ((enabled nil))
             (make-text-button
              "[ ]" nil
              'face 'button
              'follow-link t
              'action (minaduki::lambda-self (&rest _)
                        (let ((inhibit-read-only t)
                              (bounds
                               (unless (member (char-after) '(?\[ ?\s ?\]))
                                 (error
                                  "This action can only be run on a button"))))
                          (setq enabled (not enabled))
                          ;; Update the count on top first
                          (save-excursion
                            (let (current)
                              (goto-char (car count-bounds))
                              (setq current (number-at-point))
                              (delete-region (car count-bounds)
                                             (cdr count-bounds))
                              (if enabled
                                  (insert (format "%s" (1- current)))
                                (insert (format "%s" (1+ current))))
                              (setf (cdr count-bounds) (point))))
                          ;; Then update the bounds now.
                          ;; `save-excursion' knows to take the
                          ;; insertion into account, but we don't.
                          (setq bounds
                                (cl-case (char-after)
                                  (?\[ (cons (point) (+ (point) 2)))
                                  (?\s (cons (1- (point)) (1+ (point))))
                                  (?\] (cons (- (point) 2) (point)))))
                          (minaduki::set-buffer-substring
                              (car bounds) (1+ (cdr bounds))
                            (make-text-button
                             (if enabled "[X]" "[ ]") nil
                             'face 'button
                             'follow-link t
                             'action self))))))
           ;; This ensures the lambda below gets its own instance of
           ;; `file', instead of sharing with all the other
           ;; iterations. Without this, all instances of this button
           ;; would open the same file.
           (let ((file file))
             (make-text-button
              (format "%s::C%s"
                      (if (f-descendant-of? file (minaduki-vault-main))
                          (f-relative file (minaduki-vault-main))
                        file)
                      point)
              nil
              'face '(font-lock-constant-face underline)
              'follow-link t
              'action (lambda (&rest _)
                        (find-file-other-window file)
                        (goto-char point))))
           message))))
      (goto-char (point-min)))
    (display-buffer list-buffer)))

;;;###autoload
(defun minaduki/literature-entries ()
  "List all sources for browsing interactively."
  (interactive)
  (let ((key (car
              (minaduki-read:lit-entry nil :prompt "Entry: "))))
    (minaduki:local-commands key)))

;;;###autoload
(cl-defun minaduki/new-concept-note (&key title dir (visit? t))
  "Create a new concept note with TITLE.

Return the path of the newly created note.

If TITLE is nil, prompt the user for it.
If DIR is non-nil, put the file under DIR instead.
If VISIT? is non-nil (default), go to the newly created note."
  ;; VISIT? needs to be non-nil by default for this function to itself be usable
  ;; in `org-capture-templates' (when using org-capture just as a dispatcher).
  (interactive
   (list :title (read-string "Title: ")
         :visit? t))
  (let* ((title (or title (read-string "Title: ")))
         (file (--> (minaduki::to-slug title)
                    (f-join (minaduki-vault-main)
                            (or dir "")
                            it)
                    (concat it ".org")))
         (now (current-time))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (minaduki-templates--insert
       "concept"
       now
       :title title)
      (minaduki--set-created-prop now))
    (when visit? (pop-to-buffer-same-window buf))
    file))

;;;###autoload
(cl-defun minaduki/diary-next (&optional (n 1))
  "Go to the Nth next diary entry."
  (interactive "p")
  (let* ((current-file (minaduki::current-file-name))
         (siblings (directory-files (f-dirname current-file))))
    (--> (cl-position (f-filename current-file)
                      siblings
                      :test #'equal)
         (+ it n)
         (% it (length siblings))
         (nth it siblings)
         minaduki::find-file)))

;;;###autoload
(cl-defun minaduki/diary-prev (&optional (n 1))
  "Go to the Nth previous diary entry."
  (interactive "p")
  (minaduki/diary-next (- n)))

;;;###autoload
(defun minaduki/new-daily-note (&optional day)
  "Create a new daily note on DAY.

This will create diary/20211129.org on the day 2021-11-29, then
fill it in with the \"daily\" template.

If the file already exists, this will just visit it.

With a \\[universal-argument], prompt which day to create a diary
entry for."
  (interactive
   (list (and current-prefix-arg
              (minaduki//read-date "Create diary entry for day: "))))
  (let* ((day (or day (minaduki::today)))
         (actual-now (current-time))
         (moment (pcase-let ((`(,y ,m ,d)
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
    (when (equal 0 (buffer-size))
      (let (;; Since we're creating a daily note, this
            ;; variable should not be used.
            (org-extend-today-until 0))
        (minaduki-templates--insert "daily" moment)
        (minaduki--set-created-prop actual-now)))))

;;;###autoload
(cl-defun minaduki/new-fleeting-note (&optional moment dir (visit? t))
  "Create a new fleeting note for MOMENT in DIR.

MOMENT is the current time by default.
DIR is `minaduki/diary-directory' by default.
If VISIT? is non-nil (default), visit the resulting buffer afterwards.
Return the buffer created for the note regardless of VISIT?.

A fleeting note is one whose title is a timestamp, down to the second;
its filename is also based on the current time.

The entry will be stored as a file named after the current time
under DIR. For example, assuming DIR is \"/path/to/diary\", the
file will be located at something like:

    /path/to/diary/20211019T233513+0900.org

The content of the fleeting note uses the \"fleeting\" template."
  (interactive
   (list
    (and current-prefix-arg
         (parse-iso8601-time-string
          (read-string "Create new diary entry at (yyyymmddThhmmssz): ")))
    minaduki/diary-directory))
  (let* ((now (or moment (current-time)))
         (filename (format-time-string "%Y%m%dT%H%M%S%z" now))
         ;; Put this here so if we allow different templates later
         ;; it's easier to change
         (ext "org")
         (buf (find-file-noselect (f-join (or dir minaduki/diary-directory)
                                          (concat filename "." ext)))))
    (with-current-buffer buf
      (minaduki-templates--insert "fleeting" now))
    (when visit?
      (pop-to-buffer-same-window buf))
    buf))

;;;###autoload
(defun minaduki/open-diary-entry (&optional noprompt)
  "Open a diary entry.

Interactively, by default, NOPROMPT is t, and an entry from today
is opened. With a \\[universal-argument], prompt to select a day
first, as if NOPROMPT is nil.

When there are multiple diary entries, prompt for selection.

Diary entries are files in `minaduki/diary-directory' that
are named with a YYYYMMDD prefix (optionally with dashes)."
  (interactive (list t))
  (let ((day
         ;; Why not `cond': if we're in the calendar buffer but our cursor
         ;; is not on a date (so `calendar-cursor-to-date' is nil), we want
         ;; to fall back to the next case. `cond' doesn't do that.
         (or (and (derived-mode-p 'calendar-mode)
                  (-some-> (calendar-cursor-to-date)
                    minaduki-date::calendar.el->ymd))

             (and (or current-prefix-arg minaduki-btn::pressed (not noprompt))
                  (minaduki//read-date "Visit diary entry from day:"))
             (minaduki::today nil t))))
    (if-let ((file (minaduki//find-entry-for-day day)))
        (find-file file)
      (and (y-or-n-p (format "No entry from %s. Create one? " day))
           (minaduki/new-daily-note day)))))

;;;###autoload
(defun minaduki/open-diary-entry-yesterday (&optional ignore-current-file)
  "Open a diary entry from the previous day.
If the date for the current file can be figured out (by
`minaduki--file-date'), then try to open for the day before the
current file's day. Otherwise, try to files from yesterday.

If IGNORE-CURRENT-FILE is non-nil, always open files from
yesterday instead."
  (interactive "P")
  (let ((day (or (and (not ignore-current-file)
                      (minaduki--file-date))
                 (minaduki::today -1))))
    (if-let ((file (minaduki//find-entry-for-day day)))
        (find-file file)
      (and (y-or-n-p (format "No entry from %s. Create one? " day))
           (minaduki/new-daily-note day)))))

;;;###autoload
(defun minaduki/open-template ()
  "Open a template in `minaduki/templates-directory' for edit."
  (interactive)
  (minaduki::find-file
   (minaduki-templates:read "Open template file: " :all 'files)))

;;;###autoload
(defun minaduki/open-directory ()
  "Open the main vault."
  (interactive)
  (find-file (minaduki-vault-main)))

;;;###autoload
(defun minaduki/open-random-note ()
  ;; Originally `org-roam-random-note'
  "Open a random note."
  (interactive)
  (find-file (seq-random-elt (minaduki-vault-all-files))))

;;;###autoload
(defun minaduki/open-index ()
  ;; originally `org-roam-jump-to-index'
  "Open the index file.

The index file is specified in this order:

- `minaduki:index-file' (a string or function, see its docstring)
- A note with a title of \"Index\" in the main vault"
  (interactive)
  (let ((index (cond
                ((functionp minaduki:index-file)
                 (f-expand (funcall minaduki:index-file)
                           (minaduki-vault-main)))
                ((stringp minaduki:index-file)
                 (f-expand minaduki:index-file
                           (minaduki-vault-main)))
                (t
                 (car (minaduki-db::fetch-file :title "Index"))))))
    (if (and index (f-exists? index))
        (minaduki::find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (minaduki-open "Index")))))

;;;###autoload
(defun minaduki-open (&optional entry)
  ;; Some usages:
  ;; (minaduki-open title)
  ;; (minaduki-open
  ;;   (minaduki-read:note :initial-input initial-input))
  "Find and open the note ENTRY.

ENTRY is a plist (:path PATH :title TITLE). It can also be a
string, in which case it refers to a (maybe non-existent) note
with it as the title.

Interactively, provide a list of notes to search and select from.
If a note with the entered title does not exist, create a new
one."
  (interactive
   (list (minaduki-read:note)))
  (when (stringp entry)
    (setq entry
          (minaduki-node
           :path (car (minaduki-db::fetch-file :title entry))
           :title entry)))
  (let ((path (oref entry path))
        (title (oref entry title)))
    (cond ((oref entry new?)
           (minaduki/new-concept-note
            :title title
            :visit? t))
          ((oref entry id)
           (minaduki/open-id (oref entry id)))
          (t
           (minaduki::find-file path)))))

(defun minaduki/open-id (id &optional other?)
  "Open an ID.

This assumes ID is present in the cache database.

If OTHER? is non-nil, open it in another window, otherwise in the
current window."
  (-when-let (id (minaduki-db::fetch-id id))
    (minaduki::find-file (minaduki-id-file id) other?)
    (goto-char (minaduki-id-point id))))

(defun minaduki/open-id-at-point ()
  "Open the ID link at point.

This function hooks into `org-open-at-point' via
`org-open-at-point-functions'."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      ;; `org-open-at-point-functions' expects member functions to
      ;; return t if we visited a link, and nil if we haven't (to move
      ;; onto the next method or onto the default).
      (or (and (minaduki/open-id id)
               t)
          ;; No = stop here = return t
          (and (not (y-or-n-p "ID not found in the cache. Search with `org-id-files' (may be slow)? "))
               t)))))

;;;; Literature note actions

;; CITEKEY: string
(defun minaduki/new-for-citekey (citekey)
  "Create a new note for CITEKEY based on the \"lit\" template."
  (cl-block nil
    (let ((title nil)
          (props (or (-some-> (minaduki-db::fetch-lit-entry citekey)
                       minaduki-lit-entry-props)
                     (minaduki::warn :warning
                       "Could not find the literature entry %s" citekey)
                     (make-hash-table :test #'equal))))
      (when-let (key (gethash "key" props))
        (puthash "=key=" key props)
        (remhash "key" props))
      (when-let (type (gethash "type" props))
        (puthash "=type=" type props)
        (remhash "type" props))
      (setq props (map-into props 'alist))
      (setq title (or (cdr (assoc "title" props))
                      (minaduki::warn :warning "Title not found for this entry")
                      ;; this is not critical, the user may input their own
                      ;; title
                      "Title not found"))
      (unless title
        (cl-return
         (minaduki::warn :warning "Something went wrong while creating a new literature note")))
      (let ((slug (minaduki::to-slug
                   (pcase minaduki-lit:slug-source
                     (`citekey citekey)
                     (`title title)
                     (_ (user-error "`minaduki-lit:slug-source' can only be `citekey' or `title'")))))
            (now (current-time)))
        ;; Create the note
        (minaduki-open
         (minaduki-node
          :path
          (f-join minaduki/literature-notes-directory (format "%s.org" slug))))
        (apply #'minaduki-templates--insert
               minaduki-lit-template
               now
               :title title
               :ref citekey
               :slug slug
               (let ((extra-props nil))
                 (pcase-dolist (`(,key . ,value) props)
                   (when (and (eq 'string (type-of value))
                              (not (equal key "title")))
                     (when (member key '("=type=" "=key="))
                       (setq key (substring key 1 -1)))
                     (push (intern (format ":%s" key)) extra-props)
                     (push value extra-props)))
                 (nreverse extra-props)))
        (minaduki--set-created-prop now)))))

(defun minaduki/edit-citekey-notes (citekey)
  "Open a note associated with the CITEKEY.
If there isn\\='t one, create it.

CITEKEY's information is extracted from files listed in
`minaduki-lit/bibliography' during Minaduki's cache build
process.

If the note doesn\\='t exist, it is created with the \"lit\"
template (or whatever `minaduki-lit-template' is set to), with
the following arguments:

- %:title: title of the entry
- %:ref: the CITEKEY
- %:now: (common to all templates) the current moment."
  (let* ((file (minaduki-db::fetch-file :key citekey))
         (_title (minaduki-db::fetch-title file))
         title)
    (cl-block nil
      (cond
       ;; If a corresponding file exists, just visit it
       (file (minaduki::find-file file))
       ;; Otherwise create a file for it
       (t (minaduki/new-for-citekey citekey))))))

(defun minaduki-insert-citation (citekey)
  "Insert a citation to CITEKEY."
  (minaduki::file-type-case
    (:org
     (let ((minaduki-read:lit-entry::citekey citekey))
       (org-cite-insert nil)))
    (_ (insert "@" citekey))))

(defun minaduki:copy-citekey (citekey)
  "Save note's citation key to `kill-ring' and copy it to clipboard.
CITEKEY is a list whose car is a citation key."
  (with-temp-buffer
    (insert citekey)
    (copy-region-as-kill (point-min) (point-max)))
  (message "Copied \"%s\"" citekey))

(defun minaduki:visit-citekey-source (citekey)
  "Visit the source (URL, file path, DOI...) of CITEKEY."
  (let ((entry (minaduki-db::fetch-lit-entry citekey))
        sources)
    (setq sources (minaduki::resolve-org-links
                   (gethash "sources" (minaduki-lit-entry-props entry))))
    (setq minaduki-lit//cache nil)
    (cl-case (length sources)
      (0 (message "%s has no associated source" citekey))
      (1 (browse-url (car sources)))
      (t (browse-url
          (completing-read "Which one: " sources nil t))))))

(defun minaduki:citekey-show-entry (citekey)
  "Go to where CITEKEY is defined."
  (let ((entry (minaduki-db::fetch-lit-entry citekey)))
    (minaduki::find-file (minaduki-lit-entry-file entry))
    (goto-char (minaduki-lit-entry-point entry))))

(defun minaduki-insert-note-to-citekey (citekey)
  "Insert a link to the note associated with CITEKEY."
  (-if-let* ((path (minaduki-db::fetch-file :key citekey))
             (title (minaduki-db::fetch-title path)))
      ;; A corresponding note already exists. Insert a link to it.
      (minaduki-insert :entry (minaduki-node :path path :title title))
    ;; There is no corresponding note. Barf about it for now. Ideally
    ;; we'd create a note as usual, and insert a link after that's
    ;; done. But I don't know how to do that with the current
    ;; templates system.
    (message
     "@%s does not have an associated note file. Please create one first."
     citekey)))

;;;; Managing literature entries
;; Literature entries are like entries in a .bib file.
;; TODO: a generic function for creating a new entry (title, author, date)

(defun minaduki-lit::literature-key-at-point ()
  "Return the key of the literature entry at point."
  (minaduki::file-type-case
    (:org
     (let ((value (org-entry-get nil minaduki-lit/key-prop t)))
       (when (and value
                  (not (string= "" value)))
         value)))
    (:json
     (save-excursion
       (let (here doc)
         (setq here (point))
         (goto-char (point-min))
         (setq doc (json-read))
         (->> (-> (json-path-to-position here)
                  (plist-get :path)
                  car)
              (elt doc)
              (alist-get 'id)))))))

(defun minaduki-lit:fill-entry-info ()
  "Fill in information for the current heading, turning it into a literature entry."
  (interactive)
  (let ((key (minaduki-lit::literature-key-at-point)))
    (unless key
      (minaduki::file-type-case
        (:org
         (dolist (prop '("url" "author" "date"))
           (let ((value (pcase prop
                          ("author" (minaduki-read:author))
                          (_ (org-read-property-value prop)))))
             (unless (or (null value)
                         (string= value ""))
               (org-entry-put nil prop value))))
         (setq key
               (minaduki-lit::generate-key-from
                ;; :title (org-entry-get nil "ITEM")
                :author (org-entry-get nil "author")
                :date (or (org-entry-get nil "date")
                          (org-entry-get nil "year"))))
         (org-entry-put nil minaduki-lit/key-prop key))
        (:json
         (error "Support for setting the key of the CSL-JSON entry at point has not yet been implemented"))))
    key))
(defun minaduki-lit:literature-key-get-create ()
  "Assign a literature key to the current heading if it doesn't have one yet.

Return the key."
  (interactive)
  (let ((key (minaduki-lit::literature-key-at-point)))
    (unless key
      (minaduki::file-type-case
        (:org
         (setq key
               (minaduki-lit::generate-key-from
                ;; :title (org-entry-get nil "ITEM")
                :author (org-entry-get nil "author")
                :date (or (org-entry-get nil "date")
                          (org-entry-get nil "year"))))
         (org-entry-put nil minaduki-lit/key-prop key))
        (:json
         (error "Support for setting the key of the CSL-JSON entry at point has not yet been implemented"))))
    key))

;;;###autoload
(defun minaduki/new-literature-note-from-url ()
  "Create a new literature note from a URL.

This first adds an entry for it into a file in
`minaduki-lit/bibliography'."
  (interactive)
  (let* ((bibliographies (minaduki-lit:bibliography))
         (target-biblio
          (cond
           ((= 1 (length bibliographies))
            (car bibliographies))
           (t
            (let ((default-directory (minaduki-vault-main))
                  (maybe-relative
                   (cl-loop
                    for f in bibliographies
                    collect (if (f-descendant-of? f (minaduki-vault-main))
                                (f-relative f (minaduki-vault-main))
                              f))))
              (-->
               maybe-relative
               (minaduki-completion//mark-category it 'file)
               (completing-read "Which bibliography? " it nil t)
               f-expand)))))
         (info (minaduki-lit/fetch-new-entry-from-url
                (read-string "Create new literature entry for URL: "))))
    ;; Use find-file to ensure we save into it
    (find-file target-biblio)
    (minaduki::file-type-case
      (:org
       ;; Go to just before the first heading
       (goto-char (point-min))
       (outline-next-heading)
       (forward-char -1)
       (unless (eq ?\n (char-before))
         (insert "\n"))
       (insert (format "%s %s\n"
                       (make-string (1+ (or (org-current-level)
                                            0))
                                    ?*)
                       (plist-get info :title)))
       (org-entry-put nil "url"    (plist-get info :url))
       (org-entry-put nil "author" (plist-get info :author))
       (org-entry-put nil "date"   (plist-get info :date))
       (dolist (prop '("url" "author" "date"))
         (let ((value (pcase prop
                        ("author" (minaduki-read:author
                                   :def (plist-get info :author)))
                        (_ (org-read-property-value prop)))))
           (unless (or (null value)
                       (string= value ""))
             (org-entry-put nil prop value)
             (setq info (plist-put info prop value)))))
       (setq info (plist-put info
                             :citekey (minaduki-lit:literature-key-get-create))))
      (:json
       (goto-char (point-min))
       (let ((v (json-read)))
         (dolist (prop '(:author :date))
           (let ((value (read-string (substring (format "%s: " prop) 1)
                                     (plist-get info prop))))
             (unless (or (null value)
                         (string= value ""))
               (setq info (plist-put info prop value)))))
         (setq info (plist-put info :citekey (minaduki-lit::generate-key-from
                                              :author (plist-get info :author)
                                              :date (plist-get info :date))))
         (replace-region-contents
          (point-min) (point-max)
          (lambda ()
            (let ((json-encoding-pretty-print t))
              (json-encode
               (vconcat
                (list `((author . ,(->> (plist-get info :author)
                                        (s-split " and ")
                                        (--map `((literal . ,it)))
                                        vconcat))
                        (date . ,(plist-get info :date))
                        (url . ,(plist-get info :url))
                        (type . ,(f-base target-biblio))
                        (id . ,(plist-get info :citekey))
                        (title . ,(plist-get info :title))))
                v))))))))
    ;; Save the buffer
    (basic-save-buffer)
    (-when-let (citekey (plist-get info :citekey))
      (minaduki/edit-citekey-notes citekey))))

;;;; Actions

;;;###autoload
(defun minaduki:global-commands ()
  "Command palette."
  (declare (interactive-only command-execute))
  (interactive)
  (let* ((prefix-arg current-prefix-arg))
    (command-execute
     (minaduki::completing-read-annotation
      "Minaduki Global Command: "
      minaduki::global-commands
      t))))

;;;###autoload
(defun minaduki:local-commands (&optional citekey)
  "Prompt for note-related actions.

CITEKEY defaults to the entry at point. If there is no entry at
point, the KEY specified by the buffer is used. If there are
multiple keys, the user is asked to select one.

Actions are defined in `minaduki::local-commands'. If CITEKEY is
given or can be retrieved, actions from
`minaduki::local-commands::lit' are also used."
  (interactive)
  (let* ((citekey (or citekey
                      (minaduki-lit::literature-key-at-point)
                      (let ((keys (minaduki-extract/refs)))
                        (if (= 1 (length keys))
                            (cdar keys)
                          (mapcar #'cdr keys)))))
         (prompt (format "Actions for %s: "
                         (cond
                          ((stringp citekey) citekey)
                          (t (car (minaduki-extract/main-title))))))
         (candidates (-sort
                      (-on #'string< #'car)
                      (append
                       minaduki::local-commands
                       (when citekey
                         minaduki::local-commands::lit)
                       (when (member (buffer-file-name)
                                     (minaduki-lit:bibliography))
                         minaduki::local-commands::biblio)
                       (when (derived-mode-p 'org-mode)
                         minaduki::local-commands::org))))
         (func (minaduki::completing-read-annotation
                prompt candidates t)))
    (if (/= 1 (car (func-arity func)))
        (funcall func)
      (when (and citekey
                 (listp citekey))
        (setq citekey
              (completing-read "Which cite key to run action with: "
                               citekey
                               nil t)))
      (funcall func citekey))))

(provide 'minaduki-commands)

;;; minaduki-commands.el ends here
