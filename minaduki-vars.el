;;; minaduki-vars.el --- Variable declarations -*- lexical-binding: t -*-

;;; Commentary:

;; Variable declarations intended to make extra `defvar's unnecessary.

;;; Code:

(require 'f)
(require 'org)
(require 'eieio)

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
  "Number of idle seconds before triggering an Org-roam database update."
  :type 'integer
  :group 'minaduki)

(defcustom minaduki/templates-directory (f-slash
                                         (f-join
                                          org-directory "templates"))
  "Where to look for templates."
  :group 'minaduki
  :type 'string)

(defcustom minaduki/diary-directory (f-slash
                                     (f-join
                                      org-directory "diary"))
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

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .gpg extension."
  :type 'boolean
  :group 'minaduki)

(defcustom minaduki-file-extensions `("org" "md" "bib" "json")
  "Only files with these extensions are indexed.

The first item in the list is used as the default file extension.

While the file extensions may be different, the only supported
file formats are:

- Org-mode
- Markdown (partially)
- CSL-JSON (only as `minaduki-lit/bibliography')
- BibTeX / Biblatex (only as `minaduki-lit/bibliography')"
  :type '(repeat string)
  :group 'minaduki)

;; TODO: this should be a vault setting
;; Perhaps in <vault>/.minaduki/ignore as a gitignore-style file
(defcustom minaduki-file-exclude-regexp nil
  "Files matching this regular expression are not indexed."
  :type '(choice
          (string :tag "Regular expression matching files to ignore")
          (const :tag "Include everything" nil))
  :group 'minaduki)

(defcustom org-roam-find-file-function nil
  "Function called when visiting files in Org-roam commands.
If nil, `find-file' is used."
  :type 'function
  :group 'minaduki)

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
  :group 'minaduki)

(defcustom org-roam-completion-everywhere nil
  "If non-nil, provide completions from the current word at point."
  :group 'minaduki
  :type 'boolean)

(defcustom org-roam-tag-separator ","
  "String to use to separate tags when `minaduki-tag-sources' is non-nil."
  :type 'string
  :group 'minaduki)

(defcustom minaduki/slug-replacements
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
  :group 'minaduki)

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
   The \"#+alias\" property of the org file, using
   space-delimited strings.

   `headline'
   The first headline in the org file."
  :type '(repeat
          (choice
           (repeat symbol)
           (symbol)))
  :group 'minaduki)

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

(defcustom org-roam-completion-ignore-case t
  "Whether to ignore case in Org-roam `completion-at-point' completions."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'minaduki)

(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file/id equivalents."
  :group 'minaduki
  :type 'boolean)

(defcustom minaduki:link-insertion-format 'relative
  "How a new link should be inserted.

Options:

`absolute-in-vault': Absolute path in the containing vault
`relative': Relative to the current path
`absolute': Absolute path starting from root"
  ;; - [ ] Shortest path possible
  ;; - [X] Relative path
  ;; - [X] Absolute path in vault
  ;; - [X] Absolute path
  :group 'minaduki
  :type '(choice
          (const absolute-in-vault :tag "Absolute path in vault")
          (const relative :tag "Relative path")
          (const absolute :tag "Absolute path"))
  :safe #'symbolp)

;;;;; org-roam-bibtex

(defcustom orb-preformat-templates t
  "Non-nil to enable template preformatting.
See `orb-edit-notes' for details."
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil))
  :group 'minaduki-bibtex)

(defcustom orb-templates
  '(("r" "ref" plain
     (function minaduki-capture//get-point)
     ""
     :file-name "${citekey}"
     :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
     :unnarrowed t))
  "Template to use when creating a new note.
See `orb-edit-notes' for details."
  :type '(list)
  :group 'minaduki-bibtex)

(defcustom orb-preformat-keywords
  '("citekey" "entry-type" "date" "pdf?" "note?" "file"
    "author" "editor" "author-abbrev" "editor-abbrev"
    "author-or-editor-abbrev" "url")
  "(In Minaduki this is currently a giant mess.)

A list of template prompt wildcards for preformatting.
Any BibTeX field can be set for preformatting including
`bibtex-completion` \"virtual\" fields such as '=key=' and
'=type='.  BibTeX fields can be refered to by means of their
aliases defined in `orb-bibtex-field-aliases'.

Usage example:

\(setq orb-preformat-keywords '(\"citekey\" \"author\" \"date\"))
\(setq orb-templates
      '((\"r\" \"reference\" plain (function minaduki-capture//get-point)
         \"#+ROAM_KEY: %^{citekey}%?
%^{author} published %^{entry-type} in %^{date}: fullcite:%\\1.\"
         :file-name \"references/${citekey}\"
         :head \"#+TITLE: ${title}\"
         :unnarrowed t)))

Special cases:

The \"file\" keyword will be treated specially if the value of
`orb-process-file-keyword' is non-nil.  See its docstring for an
explanation.

The \"title\" keyword needs not to be set for preformatting if it
is used only within the `:head` section of the templates.

This variable takes effect when `orb-preformat-templates' is set
to t (default). See also `orb-edit-notes' for further details.

Consult bibtex-completion package for additional information
about BibTeX field names."
  :type '(repeat :tag "BibTeX field names" string)
  :group 'minaduki-bibtex)

(defcustom orb-process-file-keyword t
  "Whether to treat the file wildcards specially during template preformatting.
When this variable is non-nil, the \"%^{file}\" and \"${file}\"
wildcards will be expanded by `org-process-file-field' rather
than simply replaced with the field value.  This may be useful in
situations when the file field contains several file names and
only one file name is desirable for retrieval.  The \"file\"
keyword must be set for preformatting in `orb-preformat-keywords'
as usual.

If this variable is `string', for example \"my-file\", use its
value as the wildcard keyword instead of the default \"file\"
keyword.  Thus, it will be possible to get both the raw file
field value by expanding the %^{file} and ${file} wildcards and a
single file name by expanding the %^{my-file} and ${my-file}
wildcards.  The keyword, e.g. \"my-file\", must be set for
preformatting in `orb-preformat-keywords' as usual."
  :group 'minaduki-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)
          (string :tag "Custom wildcard keyword")))

(defcustom orb-bibtex-field-aliases
  '(("=type=" . "entry-type")
    ("=key=" . "citekey")
    ("=has-pdf=" . "pdf?")
    ("=has-note=" . "note?")
    ("citation-number" . "#"))
  "Alist of ORB-specific field aliases of the form (FIELD . ALIAS).
The ALIAS can be used instead of the FIELD anywhere in ORB's
configuration.  This variable is useful to replace
`bibtex-completion''s internal '='-embraced virtual fields with
more casual alternatives."
  :group 'minaduki-bibtex
  :type '(repeat
          (cons (string :tag "Field name")
                (string :tag "Alias name"))))

(defcustom orb-citekey-format "%s"
  "Format string for the citekey when capturing new ref notes."
  :type 'string
  :group 'minaduki-bibtex)

(defcustom orb-slug-source 'citekey
  "What should be used as a source for creating the note's slug.
Supported values are symbols `citekey' and `title'.

A special variable `${slug}` in `orb-templates' (and
`minaduki-capture/templates') is used as a placeholder for an
automatically generated string which is meant to be used in
filenames. Org Roam uses the note's title to create a slug. ORB
also allows for the citekey. `minaduki//title-to-slug' is
used to create the slug. This operation typilcally involves
removing whitespace and converting words to lowercase, among
possibly other things."
  :type '(choice
          (const citekey)
          (const title))
  :group 'minaduki-bibtex)

(defcustom orb-ignore-bibtex-store-link-functions
  '(org-bibtex-store-link)
  "Functions to override with `ignore' during note creation process.

Org Ref defines function `org-ref-bibtex-store-link' to store
links to a BibTeX buffer, e.g. with `org-store-link'.  At the
same time, Org ref requires `ol-bibtex' library, which defines
`org-bibtex-store-link' to do the same.  When creating a note
with `orb-edit-notes' from a BibTeX buffer, for example by
calling `org-ref-open-bibtex-notes', the initiated `org-capture'
process implicitly calls `org-store-link'.  The latter loops
through all the functions for storing links, and if more than one
function can store links to the location, the BibTeX buffer in
this particular case, the user will be prompted to choose one.
This is definitely annoying, hence ORB will advise all functions
in this list to return nil to trick `org-capture' and get rid of
the prompt.

The default value is `(org-bibtex-store-link)', which means this
function will be ignored and `org-ref-bibtex-store-link' will be
used to store a link to the BibTeX buffer.  See
`org-capture-templates' on how to use the link in your templates."
  :type '(repeat (function))
  :risky t
  :group 'minaduki-bibtex)

(defcustom orb-insert-link-description 'title
  "What should be used as link description for links created with `orb-insert'.
Possible values are the symbols `title', `citekey' and
`citation'.  When the value of this variable is `title' or
`citekey', then the title of the note the link points to or
respectively the citekey associated with it will be used as the
link's description:

[[file:path/to/note.org][title]] or [[file:path/to/note.org][citekey]]

When the value of this variable is `citation', instead of an
Org-mode link create an Org-ref link by appending the citation
key to `org-ref-default-citation-link' \(with a colon inbetween)
or \"cite:\", if the latter variable is not defined, for example
when Org-ref is not loaded.

The default value set by this variable can be overriden by
calling `orb-insert' with an appropriated numerical prefix
argument.  See the docstring of the function for more
information."
  :group 'minaduki-bibtex
  :type '(choice
          (const :tag "Title" title)
          (const :tag "Citation key" citekey)
          (const :tag "Citation link" citation)))

(defcustom orb-insert-follow-link nil
  "Whether to follow a newly inserted link."
  :group 'orb-roam-bibtex
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

;;;; Internal Variables

(defvar-local minaduki//file-name nil
  "The corresponding file for a temp buffer.
This is set by `minaduki//with-temp-buffer', to allow throwing of
descriptive warnings when certain operations fail (e.g. parsing).")

(defvar minaduki//last-window nil
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

(defface org-roam-link-current
  '((t :inherit org-link))
  "Face for internal links pointing to the current buffer."
  :group 'minaduki-faces)

(defface org-roam-link-invalid
  '((t :inherit (error org-link)))
  "Face for links that are not valid.
Invalid links are those without a destination."
  :group 'minaduki-faces)

(provide 'minaduki-vars)

;;; minaduki-vars.el ends here
