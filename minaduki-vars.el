;;; minaduki-vars.el --- Variable declarations -*- lexical-binding: t -*-

;;; Commentary:

;; Variable declarations intended to make extra `defvar's unnecessary.

;;; Code:

(require 'f)
(require 'org)
(require 'eieio)
(require 'map)

;;;; Type definitions

(defclass minaduki-node ()
  ((path :initarg :path :initform nil)
   (title :initarg :title :initform nil)
   (tags :initarg :tags :initform nil)
   (id :initarg :id :initform nil)
   (meta :initarg :meta :initform nil)
   (key :initarg :key :initform nil)
   (key-type :initarg :key-type :initform nil)
   (new? :initarg :new? :initform nil)))

;; Note that I take the shortcut of creating instances of these
;; classes with `record' instead of through a constructor. So it is
;; absolutely crucial that the order does not change.
(cl-defstruct (minaduki-lit-entry
               (:copier nil)
               (:constructor nil))
  key file point props)
(cl-defstruct (minaduki-id
               (:copier nil)
               (:constructor minaduki-id))
  id file point level title)

;;;; Groups
(defgroup minaduki nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "minaduki-"
  :link '(url-link :tag "GitHub" "https://github.com/kisaragi-hiu/minaduki"))

(defgroup minaduki-bibtex nil
  "Bibtex-completion integration for Org-roam."
  :group 'minaduki
  :prefix "orb-")

(defgroup minaduki-faces nil
  "Faces used by Org-roam."
  :group 'minaduki
  :group 'faces)

;;;; User Options

(defcustom minaduki:track-files-outside-vaults t
  "Whether to keep track of files outside of any vault."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki:note-title-in-frame-title nil
  "Use the note's declared title instead of the buffer name as frame title.

When this is non-nil, in files tracked by Minaduki, occurances of
\"%b\" in `frame-title-format' are replaced with a call to
`minaduki:buffer-name-for-display'.

For example, in a note stored as \"home.org\" that specified its
title to be \"Index\", the frame title is:

- \"home.org - GNU Emacs\" with this off,
- \"Index - GNU Emacs\" with this on."
  :type 'boolean
  :group 'minaduki)

(defcustom minaduki-mode:command-prefix (kbd "C-c (")
  "The prefix for bindings in files managed by Minaduki.

This should be set before `minaduki-mode' is loaded in order to
take effect."
  :type 'key-sequence
  :group 'minaduki)

(defcustom minaduki/db-location (expand-file-name "minaduki.db" user-emacs-directory)
  "Full path to the cache database.

All cache will be saved here regardless of which project a note
file might belong to, and there is no need to change this
per-project."
  :type 'string
  :group 'minaduki)

(defcustom minaduki-db/gc-threshold gc-cons-threshold
  "The value to temporarily set the `gc-cons-threshold' threshold to.
During large, heavy operations like `minaduki-db/build-cache',
many GC operations happen because of the large number of
temporary structures generated (e.g. parsed ASTs). Temporarily
increasing `gc-cons-threshold' will help reduce the number of GC
operations, at the cost of temporary memory usage.

This defaults to the original value of `gc-cons-threshold', but
tweaking this number may lead to better overall performance. For
example, to reduce the number of GCs, one may set it to a large
value like `most-positive-fixnum'."
  :type 'int
  :group 'minaduki)

(defcustom minaduki-db/update-method 'idle-timer
  "Method to update the Org-roam database.

`immediate'
  Update the database immediately upon file changes.

`idle-timer'
  Updates the database if dirty, if Emacs idles for
  `minaduki-db/update-idle-seconds'."
  :type '(choice (const :tag "idle-timer" idle-timer)
                 (const :tag "immediate" immediate))
  :group 'minaduki)

(defcustom minaduki-db/update-idle-seconds 2
  "Number of idle seconds before triggering a database update."
  :type 'integer
  :group 'minaduki)

;; TODO: relative = relative to main vault
(defcustom minaduki/templates-directory (f-slash
                                         (f-join
                                          org-directory "templates"))
  "Where to look for templates."
  :group 'minaduki
  :type 'string)

(defcustom minaduki-templates-alist
  '(("daily.org" . "#+title: %<%F>")
    ("literature.org" . "#+title: %:author - %:title
#+key: %:ref
#+key: %:url"))
  "An alist for templates.

Templates can be files in the template directory (which will
eventually be vault-specific). This alist provides a way to set
defaults, however."
  :group 'minaduki
  :type '(alist :key-type string :value-type string))

(defcustom minaduki/diary-directory (f-slash
                                     (f-join
                                      org-directory "diary"))
  "Where to store diary entries."
  :group 'minaduki
  :type 'string)

(defcustom minaduki/literature-notes-directory (f-slash
                                                (f-join
                                                 org-directory "lit"))
  "Where to store diary entries."
  :group 'minaduki
  :type 'string)

(defcustom minaduki-lit/key-prop "custom_id"
  "The Org property that defines the keys of literature entries."
  :group 'group
  :type 'type)

(defcustom minaduki-lit/bibliography (list (f-join org-directory "bibliography.org"))
  "Path to the Org file that stores literature entries."
  :group 'minaduki
  :type 'string)

(defcustom minaduki-file-extension-type-alist
  '(("org" . org)
    ("md" . markdown)
    ("markdown" . markdown)
    ("bib" . bibtex)
    ("json" . json)
    ("edn" . edn)
    ("info" . info))
  "Alist mapping extensions to file types."
  :type '(alist :key-type string :value-type symbol)
  :group 'minaduki)
(defvar minaduki-file-extensions
  (map-keys minaduki-file-extension-type-alist))

;; TODO: this should be a vault setting
;; Perhaps in <vault>/.minaduki/ignore as a gitignore-style file
(defcustom minaduki-file-exclude-regexp nil
  "Files matching this regular expression are not indexed."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'minaduki)

(defcustom minaduki:index-file "index.org"
  "Path to the index file.
The path can be a string or a function.

If it is a string, it should be the path (absolute, or relative
to `org-directory') to the index file.

If it is is a function, the function should return the path to
the index file.

Otherwise, the index is assumed to be a note in `org-directory'
whose title is 'Index'."
  :type '(choice
          (string :tag "Path to index" "%s")
          (function :tag "Function to generate the path"))
  :group 'minaduki)

(defcustom minaduki:completion-everywhere nil
  "If non-nil, provide completions from the current word at point."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki/slug-replacements
  '(;; Domains
    ("https://www.nicovideo.jp/watch/" . "niconico-")
    ("https://nico.ms/" . "niconico-")
    ("^http[s]://" . "")
    ;; etc.
    ("[^[:alnum:][:digit:]]" . "-") ; convert anything not alphanumeric
    ("--*" . "-") ; remove sequential dashes (perhaps from above)
    ("^-" . "") ; remove starting dash
    ("-$" . "")) ; remove trailing dash
  "Extra replacements used to convert a title to a filename-suitable slug.

Replacements are applied in order. For example, by default, two
spaces (\" \") will be replaced with two dashes (\"--\"),
which will then be replaced with a single dash (\"-\")."
  :type '(alist
          :key-type (string :tag "From (regexp)")
          :value-type (string :tag "To"))
  :group 'minaduki)

(defcustom minaduki:use-custom-link-faces t
  "Whether to use custom link faces.

Valide values are:

t            Use custom faces in tracked files
everywhere   Apply custom faces everywhere.

When custom faces are being used, different faces are applied to
different links:

- invalid links use the face `minaduki-link-invalid',
- links to the current file use the face `minaduki-link-current', and
- links to other files tracked by Minaduki use the face `minaduki-link'."
  :type '(choice
          (const :tag "Use custom faces in tracked files" t)
          (const :tag "Apply custom faces everywhere" everywhere)
          (const :tag "Do not apply custom faces" nil))
  :group 'minaduki)

(defcustom minaduki-tag-sources
  '(org-prop nested-vault)
  "Sources to obtain tags from.

This should be a list of functions that will extract tags from a buffer.

Currently available sources:


`hashtag':
 All occurances of hashtags like \"#tag\".
`hashtag-frontmatter':
 All occurances of hashtags in the Markdown frontmatter.
`org-prop':
 The #+roam_tags and #+tags[] properties.
 Tags are space delimited. Tags may contain spaces if they are double-quoted.
 e.g. #+roam_tags: TAG \"tag with spaces\"
`org-tags':
 Vanilla `org-mode' tags, including #+FILETAGS and inherited tags.
`nested-vault'
 If the file is in a nested vault, the name of the nested vault.
 <vault>/a/b/.obsidian ; assuming this file exists,
                       ; marking it as a nested vault
 <vault>/a/b/c/d.org -> \"b\"
`all-directories'
 All directories in the vault path.
 <vault>/a/b/c/d.org -> \"a\", \"b\", \"c\"
`last-directory'
 The last directory in the vault path.
 <vault>/a/b/c/d.org -> \"d\"
`first-directory'
 The first directory in the vault path.
 <vault>/a/b/c/d.org -> \"a\""
  :group 'minaduki
  :type '(set
          (const :tag "Hashtags" hashtag)
          (const :tag "Hashtags in frontmatter" hashtag-frontmatter)
          (const :tag "Org props" org-prop)
          (const :tag "Vanilla Org tags" org-tags)
          (const :tag "Nested vault name" nested-vault)
          (const :tag "All directories" all-directories)
          (const :tag "Last directory" last-directory)
          (const :tag "First directory" first-directory)))

(defcustom minaduki-tag-sort nil
  "When non-nil, sort tags in completions.

When t, sort the tags alphabetically, regardless of case.

This can also be a list like '(string-less-p :key downcase), in
which case the list is passed to `cl-sort' as arguments."
  :type '(choice
          (boolean)
          (list :tag "Arguments to cl-loop"))
  :group 'minaduki)

(defcustom minaduki:ignore-case-during-completion t
  "Whether to ignore case in Org-roam `completion-at-point' completions."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'minaduki)

(defcustom minaduki-wikilink-auto-replace t
  "When non-nil, replace Org-roam's roam links with file/id equivalents."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki:link-insertion-format 'relative
  "How a new link should be inserted.

Options:

- `absolute-in-vault': Absolute path in the containing vault
- `relative': Relative to the current path
- `absolute': Absolute path starting from root

This can be set in a directory local variable."
  ;; - [ ] Shortest path possible
  ;; - [X] Relative path
  ;; - [X] Absolute path in vault
  ;; - [X] Absolute path
  :group 'minaduki
  :type '(choice
          (const absolute-in-vault :tag "Absolute path in vault")
          (const relative :tag "Relative path")
          (const absolute :tag "Absolute path"))
  :safe (lambda (x)
          (memq x '(relative absolute absolute-in-vault))))

;;;;; org-roam-bibtex

(defcustom orb-citekey-format "%s"
  "Format string for the citekey when capturing new ref notes."
  :type 'string
  :group 'minaduki-bibtex)

(defcustom orb-slug-source 'citekey
  "What should be used as a source for creating the note's slug.
Supported values are symbols `citekey' and `title'.

A special variable `%:slug` in the \"literature\" template is
used as a placeholder for an automatically generated string which
is meant to be used in filenames. This can be based on the
citekey or the title. `minaduki::title-to-slug' is used to create
the slug."
  :type '(choice
          (const citekey)
          (const title))
  :group 'minaduki-bibtex)

;;;; Internal Variables

(defvar-local minaduki::file-name nil
  "The corresponding file for a temp buffer.
This is set by `minaduki::with-temp-buffer', to allow throwing of
descriptive warnings when certain operations fail (e.g. parsing).")

(defvar minaduki::last-window nil
  "Last window `org-roam' was called from.")

(defvar minaduki::org-link-bracket-typed-re
  (rx (seq "[["
           (group (+? anything))
           ":"
           (group
            (one-or-more
             (or (not (any "[]\\"))
                 (and "\\" (zero-or-more "\\\\") (any "[]"))
                 (and (one-or-more "\\") (not (any "[]"))))))
           "]"
           (opt "[" (group (+? anything)) "]")
           "]"))
  "Matches a typed link in double brackets.")

(defvar minaduki::global-commands
  '(("Open or create a note"              . minaduki/open)
    ("Browse literature entries"          . minaduki/literature-entries)
    ("Open the notes directory"           . minaduki/open-directory)
    ("Open or create a template"          . minaduki/open-template)
    ("Open or create a diary entry"       . minaduki/open-diary-entry)
    ("Create a new fleeting note"         . minaduki/new-fleeting-note)
    ("Create a new concept note"          . minaduki/new-concept-note)
    ("Create a new note with the \"daily\" template" . minaduki/new-daily-note)
    ("Find broken local links"            . minaduki/fix-broken-links)
    ("Open the index file"                . minaduki/open-index)
    ("Create a new literature note from URL" . minaduki:new-literature-note-from-url)
    ("Open a random note"                 . minaduki/open-random-note)
    ("Refresh cache"                      . minaduki-db/build-cache))
  "Global commands shown in `minaduki:global-commands'.

List of (DISPLAY-NAME . COMMAND) pairs.")

(defvar minaduki::local-commands
  '(("Create ID for current heading" . minaduki:id)
    ("Move file to..."               . minaduki:move-file-to-directory)
    ("Insert a button"               . minaduki-btn:insert)
    ("Insert a link"                 . minaduki:insert)
    ("Insert a link to a heading in the same file" . minaduki:insert-local)
    ("Insert a citation"             . org-cite-insert)
    ("Add an alias"                  . minaduki-add-alias)
    ("Delete an alias"               . minaduki-delete-alias)
    ("Add a tag"                     . minaduki-add-tag)
    ("Delete a tag"                  . minaduki-delete-tag))
  "Local commands that act on the current file or heading.")

(defvar minaduki::literature-note-actions
  '(("Open URL, DOI, or PDF" . minaduki/visit-source)
    ("Show entry in the bibliography file" . minaduki/show-entry)
    ("Edit notes" . minaduki:lit-entry-edit-notes)
    ("Copy citekey" . minaduki/copy-citekey)
    ("Insert citation" . minaduki:insert-citation)
    ("Insert link to associated notes" . minaduki:insert-note-to-citekey))
  "Commands useful inside a literature note.

List of (DISPLAY-NAME . FUNCTION) pairs. Each function receives
one argument, the citekey.

Equivalent to `orb-note-actions-default'.")

(defvar minaduki::bibliography-commands
  '(("Create bibliography ID for current heading" . minaduki-lit:literature-key-get-create))
  "Local commands that act on the current file or heading.")

;;;; Faces
(defface minaduki-link
  '((t :inherit org-link))
  "Face for Org-roam links."
  :group 'minaduki-faces)

(defface minaduki-key
  '((t :inherit org-cite))
  "Face for highlighting literature entry keys."
  :group 'minaduki-faces)

(defface minaduki-type
  '((t :inherit shadow))
  "Face for highlighting literature entry types."
  :group 'minaduki-faces)

(defface minaduki-path
  '((t :inherit shadow))
  "Face for paths in Minaduki's completion."
  :group 'minaduki-faces)

(defface minaduki-tag
  '((t :inherit font-lock-keyword-face :slant italic))
  "Face for tags in Minaduki's completion."
  :group 'minaduki-faces)

(defface minaduki-link-current
  '((t :inherit org-link))
  "Face for internal links pointing to the current buffer."
  :group 'minaduki-faces)

(defface minaduki-link-invalid
  '((t :inherit (error org-link)))
  "Face for links that are not valid.
Invalid links are those without a destination."
  :group 'minaduki-faces)

(provide 'minaduki-vars)

;;; minaduki-vars.el ends here
