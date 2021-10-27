;;; kisaragi-notes-vars.el --- Variable declarations -*- lexical-binding: t -*-

;;; Commentary:

;; Variable declarations intended to make extra `defvar's unnecessary.

;;; Code:

(require 'f)
(require 'org)

;;;; User Options
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

(defcustom kisaragi-notes/templates-directory (f-slash
                                               (f-join
                                                org-directory "templates"))
  "Where to look for templates."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .gpg extension."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-file-extensions '("org" "md")
  "Only files with these extensions are indexed.

The first item in the list is used as the default file extension.

While the file extensions may be different, the only supported
file formats are Org-mode and Markdown (partially)."
  :type '(repeat string)
  :group 'org-roam)

(defcustom org-roam-file-exclude-regexp nil
  "Files matching this regular expression are excluded from the Org-roam."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'org-roam)

(defcustom org-roam-find-file-function nil
  "Function called when visiting files in Org-roam commands.
If nil, `find-file' is used."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-include-type-in-ref-path-completions nil
  "When t, include the type in ref-path completions.
Note that this only affects interactive calls.
See `org-roam--get-ref-path-completions' for details."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-index-file "index.org"
  "Path to the Org-roam index file.
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
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The formatter used when inserting Org-roam links that use their title.
Formatter may be a function that takes title as its only argument."
  :type '(choice
          (string :tag "String Format" "%s")
          (function :tag "Custom function"))
  :group 'org-roam)

(defcustom org-roam-completion-everywhere nil
  "If non-nil, provide completions from the current word at point."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-prefer-id-links t
  "If non-nil, use ID for linking instead where available."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-tag-separator ","
  "String to use to separate tags when `kisaragi-notes/tag-sources' is non-nil."
  :type 'string
  :group 'org-roam)

(defcustom kisaragi-notes/slug-replacements
  '(("[^[:alnum:][:digit:]]" . "_") ; convert anything not alphanumeric
    ("__*" . "_") ; remove sequential underscores (perhaps from above)
    ("^_" . "") ; remove starting underscore
    ("_$" . "")) ; remove trailing underscore
  "Extra replacements used to convert a title to a filename-suitable slug.

Replacements are applied in order. For example, by default, two
spaces (\" \") will be replaced with two underscores (\"__\"),
which will then be replaced with a single underscore (\"_\")."
  :type '(alist
          :key-type (string :tag "From (regexp)")
          :value-type (string :tag "To"))
  :group 'org-roam)

(defcustom org-roam-title-sources '((title headline) alias)
  "The list of sources from which to retrieve a note title.

Return values from each source, which are symbols corresponding
to a title retrieval function, are concatenated into the final
list of titles. A source can also be a list of sources --- in
which case the first non-nil return value is used.

For example, the default setting ((title headline) alias)
effectively stands for this:

    (append (or title headline)
            alias)

So, when the title is not empty, 'title + 'alias will be
returned; otherwise, 'headline + 'alias is the resulting list of
titles.

The currently supported symbols are:

  `title'
   The \"#+title\" property of org file.

  `alias'
   The \"#+roam_alias\" property of the org file, using
   space-delimited strings.

   `headline'
   The first headline in the org file."
  :type '(repeat
          (choice
           (repeat symbol)
           (symbol)))
  :group 'org-roam)

(defcustom org-roam-file-completion-tag-position 'prepend
  "Prepend, append, or omit tags from the file titles during completion."
  :type '(choice (const :tag "Prepend" prepend)
                 (const :tag "Append" append)
                 (const :tag "Omit" omit))
  :group 'org-roam)

(defcustom org-roam-enable-headline-linking t
  "Enable linking to headlines.
This includes automatic :ID: creation and scanning of :ID:s for
org-roam database."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-link-use-custom-faces t
  "Define where to apply custom faces to Org-roam links.

Valide values are:

t            Use custom faces inside Org-roam notes (i.e. files in
             `org-directory'.)

everywhere   Apply custom faces everywhere.

Otherwise, do not apply custom faces to Org-roam links."
  :type '(choice
          (const :tag "Use custom faces inside Org-roam notes" t)
          (const :tag "Apply custom faces everywhere" everywhere)
          (const :tag "Do not apply custom faces" nil))
  :group 'org-roam)

(defcustom kisaragi-notes/tag-sources
  '(org-roam--extract-tags-prop)
  "Sources to obtain tags from.

This should be a list of functions that will extract tags from a buffer.

Currently available sources:

  `org-roam--extract-tags-prop'
    Extract tags from the #+roam_tags property.
    Tags are space delimited.
    Tags may contain spaces if they are double-quoted.
    e.g. #+roam_tags: TAG \"tag with spaces\"

  `org-roam--extract-tags-vanilla'
    Extract vanilla `org-mode' tags, including #+FILETAGS and
    inherited tags.

  `org-roam--extract-tags-all-directories'
    Extract sub-directories relative to `org-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tags \"foo\" and \"bar\".

  `org-roam--extract-tags-last-directory'
    Extract the last directory relative to `org-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"bar\".

  `org-roam--extract-tags-first-directory'
    Extract the first directory relative to `org-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"foo\"."
  :group 'org-roam
  :type '(set (const :tag "#+roam_tags"
                     org-roam--extract-tags-prop)
              (const :tag "buffer org tags"
                     org-roam--extract-tags-vanilla)
              (const :tag "sub-directories"
                     org-roam--extract-tags-all-directories)
              (const :tag "parent directory"
                     org-roam--extract-tags-last-directory)
              (const :tag "first sub-directory"
                     org-roam--extract-tags-first-directory)))

(defcustom org-roam-tag-sort nil
  "When non-nil, sort tags in completions.

When t, sort the tags alphabetically, regardless of case.

This can also be a list like '(string-less-p :key downcase), in
which case the list is passed to `cl-sort' as arguments."
  :type '(choice
          (boolean)
          (list :tag "Arguments to cl-loop"))
  :group 'org-roam)

(defcustom org-roam-completion-ignore-case t
  "Whether to ignore case in Org-roam `completion-at-point' completions."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file/id equivalents."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-link-file-path-type 'relative
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory."
  :group 'org-roam
  :type '(choice
          (const relative)
          (const absolute)
          (const noabbrev))
  :safe #'symbolp)

;;;; Internal Variables

(defvar-local kisaragi-notes//file-name nil
  "The corresponding file for a temp buffer.
This is set by `org-roam--with-temp-buffer', to allow throwing of
descriptive warnings when certain operations fail (e.g. parsing).")

(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defvar org-roam--org-link-bracket-typed-re
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

;;;; Faces
(defface org-roam-link
  '((t :inherit org-link))
  "Face for Org-roam links."
  :group 'org-roam-faces)

(defface org-roam-tag
  '((t :weight bold))
  "Face for Org-roam tags in minibuffer commands."
  :group 'org-roam-faces)

(defface org-roam-link-current
  '((t :inherit org-link))
  "Face for Org-roam links pointing to the current buffer."
  :group 'org-roam-faces)

(defface org-roam-link-invalid
  '((t :inherit (error org-link)))
  "Face for Org-roam links that are not valid.
This face is used for links without a destination."
  :group 'org-roam-faces)

(defface org-roam-link-shielded
  '((t :inherit (warning org-link)))
  "Face for Org-roam links that are shielded.
This face is used on the region target by `org-roam-insertion'
during an `org-roam-capture'."
  :group 'org-roam-faces)

(provide 'kisaragi-notes-vars)

;;; kisaragi-notes-vars.el ends here
