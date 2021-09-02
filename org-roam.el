;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "27.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (bibtex-completion "2.0.0"))

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
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Dependencies
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'ob-core) ;for org-babel-parse-header-arguments
(require 'ansi-color) ; org-roam--list-files strip ANSI color codes
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'rx)
(require 's)
(require 'seq)
(eval-when-compile (require 'subr-x))

;;;; Features
(require 'org-roam-macs)
;; These features should be able to be loaded order independently.
;; @TODO: implement something akin to `org-modules' that allows
;; selectively loading different sets of features.
;; ~NV [2020-05-22 Fri]

(require 'org-roam-faces)
(require 'org-roam-buffer)
(require 'org-roam-completion)
(require 'org-roam-capture)
(require 'org-roam-extract)
(require 'org-roam-db)
(require 'org-roam-doctor)
(require 'org-roam-graph)
(require 'org-roam-link)

;;;; Declarations
;; From org-ref-core.el
(defvar org-ref-cite-types)
(declare-function org-ref-split-and-strip-string "ext:org-ref-utils" (string))
;; From org-id.el
(defvar org-id-link-to-org-use-id)
(declare-function org-id-find-id-in-file "ext:org-id" (id file &optional markerp))

;;;; Customizable variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/org-roam/org-roam")
  :link '(url-link :tag "Online Manual" "https://www.orgroam.com/manual.html"))

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.
All Org files, at any level of nesting, are considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .gpg extension."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-file-extensions '("org")
  "Detected file extensions to include in the Org-roam ecosystem.
The first item in the list is used as the default file extension.
While the file extensions may be different, the file format needs
to be an `org-mode' file, and it is the user's responsibility to
ensure that."
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
The path can be a string or a function.  If it is a string, it
should be the path (absolute or relative to `org-roam-directory')
to the index file.  If it is is a function, the function should
return the path to the index file.  Otherwise, the index is
assumed to be a note in `org-roam-directory' whose title is
'Index'."
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

(defcustom org-roam-prefer-id-links t
  "If non-nil, use ID for linking instead where available."
  :type 'boolean
  :group 'org-roam)

(defcustom org-roam-list-files-commands
  (if (member system-type '(windows-nt ms-dos cygwin))
      nil
    '(rg))
  "Commands that will be used to find Org-roam files.

It should be a list of symbols or cons cells representing any of the following
 supported file search methods.

The commands will be tried in order until an executable for a command is found.
The Elisp implementation is used if no command in the list is found.

  `rg'
    Use ripgrep as the file search method.
    Example command: rg /path/to/dir/ --files -g \"*.org\" -g \"*.org.gpg\"

By default, `executable-find' will be used to look up the path to the
executable. If a custom path is required, it can be specified together with the
method symbol as a cons cell. For example: '(find (rg . \"/path/to/rg\"))."
  :type '(set (const :tag "rg" rg))
  :group 'org-roam)

(defcustom org-roam-tag-separator ","
  "String to use to separate tags when `org-roam-tag-sources' is non-nil."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-tag-sort nil
  "When non-nil, sort the tags in the completions.
When t, sort the tags alphabetically, regardless of case.
`org-roam-tag-sort' can also be a list of arguments to be applied
to `cl-sort'.  For example, these are the arguments used when
`org-roam-tag-sort' is set to t:
    \('string-lessp :key 'downcase)
Only relevant when `org-roam-tag-sources' is non-nil."
  :type '(choice
          (boolean)
          (list :tag "Arguments to cl-loop"))
  :group 'org-roam)

(defcustom org-roam-tag-sources '(prop)
  "Sources to obtain tags from.

It should be a list of symbols representing any of the following
extraction methods:

  `prop'
    Extract tags from the #+roam_tags property.
    Tags are space delimited.
    Tags may contain spaces if they are double-quoted.
    e.g. #+roam_tags: TAG \"tag with spaces\"

  `vanilla'
    Extract vanilla `org-mode' tags, including #+FILETAGS and
    inherited tags.

  `all-directories'
    Extract sub-directories relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tags \"foo\" and \"bar\".

  `last-directory'
    Extract the last directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"bar\".

  `first-directory'
    Extract the first directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"foo\"."
  :type '(set (const :tag "#+roam_tags" prop)
              (const :tag "buffer org tags" vanilla)
              (const :tag "sub-directories" all-directories)
              (const :tag "parent directory" last-directory)
              (const :tag "first sub-directory" first-directory)))

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
Each element in the list is either:

1. a symbol -- this symbol corresponds to a title retrieval
function, which returns the list of titles for the current buffer
2. a list of symbols -- symbols in the list are treated as
with (1).  The return value of this list is the first symbol in
the list returning a non-nil value.

The return results of the root list are concatenated.

For example the setting: '((title headline) alias) means the following:

1. Return the 'title + 'alias, if the title of current buffer is non-empty;
2. Or return 'headline + 'alias otherwise.

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

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

(defvar org-roam-completion-functions nil
  "List of functions to be used with `completion-at-point' for Org-roam.")

;;;; Dynamic variables
(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defvar-local org-roam-file-name nil
  "The corresponding file for a temp buffer.
This is set by `org-roam--with-temp-buffer', to allow throwing of
descriptive warnings when certain operations fail (e.g. parsing).")

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

;;;; Utilities
;;;; File functions and predicates
(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (org-roam--file-name-extension path)))
    (when (string= ext "gpg")           ; Handle encrypted files
      (setq ext (org-roam--file-name-extension (file-name-sans-extension path))))
    (member ext org-roam-file-extensions)))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (when-let ((path (or file
                       org-roam-file-name
                       (-> (buffer-base-buffer)
                           (buffer-file-name)))))
    (save-match-data
      (and
       (org-roam--org-file-p path)
       (not (and org-roam-file-exclude-regexp
                 (string-match-p org-roam-file-exclude-regexp path)))
       (f-descendant-of-p path (expand-file-name org-roam-directory))))))

(defun org-roam--shell-command-files (cmd)
  "Run CMD, a command list like in `make-process', to search for files.

Return a list of files."
  (with-temp-buffer
    (apply #'call-process (car cmd) nil '(t nil) nil "-L" org-roam-directory (cdr cmd))
    (s-split "\n" (buffer-string) :omit-nulls)))

(defun org-roam--list-files-search-globs (exts)
  "Given EXTS, return a list of search globs.
E.g. (\".org\") => (\"*.org\" \"*.org.gpg\")"
  (append
   (mapcar (lambda (ext) (s-wrap (concat "*." ext) "\"")) exts)
   (mapcar (lambda (ext) (s-wrap (concat "*." ext ".gpg") "\"")) exts)))

(defun org-roam--list-files-rg (executable dir)
  "Return all Org-roam files located recursively within DIR, using ripgrep, provided as EXECUTABLE."
  (let* ((globs (org-roam--list-files-search-globs org-roam-file-extensions))
         (command `(,executable "-L" ,dir "--files"
                                ,@(cons "-g" (-interpose "-g" globs)))))
    (org-roam--shell-command-files command)))

;; Emacs 26 does not have FOLLOW-SYMLINKS in `directory-files-recursively'
(defun org-roam--directory-files-recursively (dir regexp
                                                  &optional include-directories predicate
                                                  follow-symlinks)
  "Return list of all files under directory DIR whose names match REGEXP.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.

By default, the returned list excludes directories, but if
optional argument INCLUDE-DIRECTORIES is non-nil, they are
included.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (files nil)
         (dir (directory-file-name dir))
         ;; When DIR is "/", remote file names like "/method:" could
         ;; also be offered.  We shall suppress them.
         (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (member file '("./" "../"))
        (if (directory-name-p file)
            (let* ((leaf (substring file 0 (1- (length file))))
                   (full-file (concat dir "/" leaf)))
              ;; Don't follow symlinks to other directories.
              (when (and (or (not (file-symlink-p full-file))
                             (and (file-symlink-p full-file)
                                  follow-symlinks))
                         ;; Allow filtering subdirectories.
                         (or (eq predicate nil)
                             (eq predicate t)
                             (funcall predicate full-file)))
                (let ((sub-files
                       (if (eq predicate t)
                           (condition-case nil
                               (org-roam--directory-files-recursively
                                full-file regexp include-directories
                                predicate follow-symlinks)
                             (file-error nil))
                         (org-roam--directory-files-recursively
                          full-file regexp include-directories
                          predicate follow-symlinks))))
                  (setq result (nconc result sub-files))))
              (when (and include-directories
                         (string-match regexp leaf))
                (setq result (nconc result (list full-file)))))
          (when (string-match regexp file)
            (push (concat dir "/" file) files)))))
    (nconc result (nreverse files))))

(defun org-roam--list-files-elisp (dir)
  "Return all Org-roam files located recursively within DIR, using elisp."
  (let ((regex (concat "\\.\\(?:"(mapconcat #'regexp-quote org-roam-file-extensions "\\|" )"\\)\\(?:\\.gpg\\)?\\'"))
        result)
    (dolist (file (org-roam--directory-files-recursively dir regex nil nil t) result)
      (when (and (file-readable-p file)
                 (org-roam--org-roam-file-p file))
        (push file result)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located recursively within DIR.
Use external shell commands if defined in `org-roam-list-files-commands'."
  (let (path exe)
    (cl-dolist (cmd org-roam-list-files-commands)
      (pcase cmd
        (`(,e . ,path)
         (setq path (executable-find path)
               exe  (symbol-name e)))
        ((pred symbolp)
         (setq path (executable-find (symbol-name cmd))
               exe (symbol-name cmd)))
        (wrong-type
         (signal 'wrong-type-argument
                 `((consp symbolp)
                   ,wrong-type))))
      (when path (cl-return)))
    (if-let* ((files (when path
                       (let ((fn (intern (concat "org-roam--list-files-" exe))))
                         (unless (fboundp fn) (user-error "%s is not an implemented search method" fn))
                         (funcall fn path (format "\"%s\"" dir)))))
              (files (seq-filter #'org-roam--org-roam-file-p files))
              (files (mapcar #'expand-file-name files))) ; canonicalize names
        files
      (org-roam--list-files-elisp dir))))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (expand-file-name org-roam-directory)))

;;;; Title/Path/Slug conversion
(defun org-roam--path-to-slug (path)
  "Return a slug from PATH."
  (-> path
      (file-relative-name (expand-file-name org-roam-directory))
      (file-name-sans-extension)))

(defun org-roam-format-link (target &optional description type _link-type)
  ;; I'll probably eventually tear down `link-type'
  "Formats an org link for a given file TARGET, link DESCRIPTION and link TYPE.
TYPE defaults to \"file\".

If the file has an ID and `org-roam-prefer-id-links' is non-nil,
we will return an ID link."
  (setq type (or type "file"))
  (when-let ((id (and org-roam-prefer-id-links
                      (string-equal type "file")
                      (caar (org-roam-db-query [:select [id] :from ids
                                                :where (= file $s1)
                                                :and (= level 0)
                                                :limit 1]
                                               target)))))
    (setq type "id" target id))
  (org-link-make-string
   (if (string-equal type "file")
       (kisaragi-notes-link/apply-link-abbrev target)
     (concat type ":" target))
   (if (functionp org-roam-link-title-format)
       (funcall org-roam-link-title-format description type)
     (format org-roam-link-title-format description))))

(defun org-roam--add-tag-string (str tags)
  "Add TAGS to STR.

Depending on the value of `org-roam-file-completion-tag-position', this function
prepends TAGS to STR, appends TAGS to STR or omits TAGS from STR."
  (pcase org-roam-file-completion-tag-position
    ('prepend (concat
               (when tags (propertize (format "(%s) " (s-join org-roam-tag-separator tags))
                                      'face 'org-roam-tag))
               str))
    ('append (concat
              str
              (when tags (propertize (format " (%s)" (s-join org-roam-tag-separator tags))
                                     'face 'org-roam-tag))))
    ('omit str)))


(defun org-roam--get-title-path-completions ()
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--add-tag-string title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))

(defun org-roam--get-index-path ()
  "Return the path to the index in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, it is assumed to be a note in `org-roam-directory'
whose title is 'Index'."
  (let ((path (pcase org-roam-index-file
                ((pred functionp) (funcall org-roam-index-file))
                ((pred stringp) org-roam-index-file)
                ('nil (user-error "You need to set `org-roam-index-file' before you can jump to it"))
                (wrong-type (signal 'wrong-type-argument
                                    `((functionp stringp)
                                      ,wrong-type))))))
    (expand-file-name path org-roam-directory)))

;;;; dealing with file-wide properties
(defun org-roam--set-global-prop (name value)
  "Set a file property called NAME to VALUE.

If the property is already set, it's value is replaced."
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)") (point-max) t)
          (replace-match (concat " " value) 'fixedcase nil nil 1)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

;;;; org-roam-find-ref
(defun org-roam--get-ref-path-completions (&optional arg filter)
  "Return an alist of refs to absolute path of Org-roam files.

When called interactively (i.e. when ARG is 1), formats the car
of the completion-candidates with extra information: title, tags,
and type \(when `org-roam-include-type-in-ref-path-completions'
is non-nil).

When called with a `C-u' prefix (i.e. when ARG is 4), forces the
default format without the formatting.

FILTER can either be a string or a function:

- If it is a string, it should be the type of refs to include as
  candidates \(e.g. \"cite\", \"website\", etc.)

- If it is a function, it should be the name of a function that
  takes three arguments: the type, the ref, and the file of the
  current candidate. It should return t if that candidate is to
  be included as a candidate."
  (let ((rows (org-roam-db-query
               [:select [refs:type refs:ref refs:file titles:title tags:tags]
                :from titles
                :left :join tags
                :on (= titles:file tags:file)
                :left :join refs :on (= titles:file refs:file)
                :where refs:file :is :not :null]))
        completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,type ,ref ,file-path ,title ,tags) row))
        (when (pcase filter
                ('nil t)
                ((pred stringp) (string= type filter))
                ((pred functionp) (funcall filter type ref file-path))
                (wrong-type (signal 'wrong-type-argument
                                    `((stringp functionp)
                                      ,wrong-type))))
          (let ((k (if (eq arg 1)
                       (concat
                        (when org-roam-include-type-in-ref-path-completions
                          (format "{%s} " type))
                        (org-roam--add-tag-string (format "%s (%s)" title ref)
                                                  tags))
                     ref))
                (v (list :path file-path :type type :ref ref)))
            (push (cons k v) completions)))))))

(defun org-roam--find-file (file)
  "Open FILE using `org-roam-find-file-function' or `find-file'."
  (funcall (or org-roam-find-file-function #'find-file) file))

(defun org-roam--find-ref (ref)
  "Find and open and Org-roam file from REF if it exists.
REF should be the value of '#+roam_key:' without any
type-information (e.g. 'cite:').
Return nil if the file does not exist."
  (when-let* ((completions (org-roam--get-ref-path-completions))
              (file (plist-get (cdr (assoc ref completions)) :path)))
    (org-roam--find-file file)))

(defun org-roam--org-roam-buffer-p (&optional buffer)
  "Return t if BUFFER is accessing a part of Org-roam system.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        path)
    (with-current-buffer buffer
      (and (derived-mode-p 'org-mode)
           (setq path (buffer-file-name (buffer-base-buffer)))
           (org-roam--org-roam-file-p path)))))

(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (org-roam--org-roam-buffer-p it)
            (buffer-list)))

(defun org-roam--save-buffers (&optional ask update)
  "Save all Org-roam buffers.
When ASK is non-nil, ask whether the buffers should be saved.
When UPDATE is non-nil, update the database after."
  (save-some-buffers (not ask) #'org-roam--org-roam-buffer-p)
  (when update (org-roam-db-update)))

;;; org-roam-backlinks-mode
(define-minor-mode org-roam-backlinks-mode
  "Minor mode for the `org-roam-buffer'.
\\{org-roam-backlinks-mode-map}"
  :lighter " Backlinks"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1] 'org-open-at-point)
             (define-key map (kbd "RET") 'org-open-at-point)
             map)
  (cond (org-roam-backlinks-mode
         (add-hook 'post-command-hook #'kisaragi-notes-buffer//save-point nil :local)
         (add-hook 'org-open-at-point-functions #'org-roam-open-at-point nil :local))
        (t
         (remove-hook 'post-command-hook #'kisaragi-notes-buffer//save-point :local)
         (remove-hook 'org-open-at-point-functions #'org-roam-open-at-point :local))))

(defun org-roam--in-buffer-p ()
  "Return t if in the Org-roam buffer."
  (and (boundp org-roam-backlinks-mode)
       org-roam-backlinks-mode))

(defun org-roam--backlink-to-current-p ()
  "Return t if the link at point is to the current Org-roam file."
  (save-match-data
    (let ((current-file (buffer-file-name org-roam-buffer--current))
          (backlink-dest (save-excursion
                           (let* ((context (org-element-context))
                                  (type (org-element-property :type context))
                                  (dest (org-element-property :path context)))
                             (pcase type
                               ("id" (org-roam-id-get-file dest))
                               (_ dest))))))
      (string= current-file backlink-dest))))

(defun org-roam-open-at-point ()
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
        (org-roam-buffer--find-file path)
        (org-show-context)
        t)))
   ;; Org-roam preview text
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (org-roam-buffer--find-file file-from)
      (goto-char p)
      (org-show-context)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(defun org-roam--get-backlinks (targets)
  "Return the backlinks for TARGETS.
TARGETS is a list of strings corresponding to the TO value in the
Org-roam cache. It may be a file, for Org-roam file links, or a
citation key, for Org-ref cite links."
  (unless (listp targets)
    (setq targets (list targets)))
  (let ((conditions (--> targets
                         (mapcar (lambda (i) (list '= 'dest i)) it)
                         (org-roam--list-interleave it :or))))
    (org-roam-db-query `[:select [source dest properties] :from links
                         :where ,@conditions
                         :order-by (asc source)])))

;;; Completion at point
(defcustom org-roam-completion-everywhere nil
  "If non-nil, provide completions from the current word at point."
  :group 'org-roam
  :type 'boolean)

;;;; Tags completion
(defun org-roam-complete-tags-at-point ()
  "`completion-at-point' function for Org-roam tags."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (looking-back "^#\\+roam_tags:.*" (line-beginning-position))
      (when (looking-at "\\>")
        (setq start (save-excursion (skip-syntax-backward "w")
                                    (point))
              end (point)))
      (setq collection #'org-roam-db--get-tags
            exit-fn (lambda (str _status)
                      (delete-char (- (length str)))
                      (insert "\"" str "\""))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function exit-fn)))))

(defun org-roam--get-titles ()
  "Return all titles within Org-roam."
  (mapcar #'car (org-roam-db-query [:select [titles:title] :from titles])))

(defun org-roam-complete-everywhere ()
  "`completion-at-point' function for word at point.
This is active when `org-roam-completion-everywhere' is non-nil."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (and org-roam-completion-everywhere
               (thing-at-point 'word))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (setq start (car bounds)
              end (cdr bounds)
              collection #'org-roam--get-titles
              exit-fn (lambda (str _status)
                        (delete-char (- (length str)))
                        (insert "[[roam:" str "]]")))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-case-fold
                   (completion-table-dynamic
                    (lambda (_)
                      (cl-remove-if (apply-partially #'string= prefix)
                                    (funcall collection))))
                   (not org-roam-completion-ignore-case))
                collection)
              :exit-function exit-fn)))))

(add-to-list 'org-roam-completion-functions #'org-roam-complete-tags-at-point)
(add-to-list 'org-roam-completion-functions #'org-roam-complete-everywhere)
(add-to-list 'org-roam-completion-functions #'org-roam-link-complete-at-point)

;;; Org-roam-mode
;;;; Function Faces
;; These faces are used by `org-link-set-parameters', which take one argument,
;; which is the path.
(defcustom org-roam-link-use-custom-faces t
  "Define where to apply custom faces to Org-roam links.

Valide values are:

t            Use custom faces inside Org-roam notes (i.e. files in
             `org-roam-directory'.)

everywhere   Apply custom faces everywhere.

Otherwise, do not apply custom faces to Org-roam links."
  :type '(choice
          (const :tag "Use custom faces inside Org-roam notes" t)
          (const :tag "Apply custom faces everywhere" everywhere)
          (const :tag "Do not apply custom faces" nil))
  :group 'org-roam)

(defun org-roam--file-link-face (path)
  "Conditional face for file: links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (org-roam--org-roam-file-p)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and custom
                  (not (file-remote-p path)) ;; Prevent lockups opening Tramp links
                  (not (file-exists-p path)))
             'org-roam-link-invalid)
            ((and (org-roam--in-buffer-p)
                  (org-roam--backlink-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (org-roam--org-roam-file-p path))
             'org-roam-link)
            (t
             'org-link)))))

(defun org-roam--id-link-face (id)
  "Conditional face for id links.
Applies `org-roam-link-current' if ID corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if ID corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (org-roam--org-roam-file-p)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and (org-roam--in-buffer-p)
                  (org-roam--backlink-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (org-roam-id-get-file id t))
             'org-roam-link)
            ((and custom
                  (not (org-roam-id-get-file id)))
             'org-roam-link-invalid)
            (t
             'org-link)))))

;;;; Hooks and Advices
(defcustom org-roam-file-setup-hook nil
  "Hook that is run on setting up an Org-roam file."
  :group 'org-roam
  :type 'hook)

(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when mode symbol `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (run-hooks 'org-roam-file-setup-hook) ; Run user hooks
    (org-roam--setup-title-auto-update)
    (add-hook 'post-command-hook #'org-roam-buffer--update-maybe nil t)
    (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
    (add-hook 'after-save-hook #'org-roam-db-update nil t)
    (dolist (fn org-roam-completion-functions)
      (add-hook 'completion-at-point-functions fn nil t))
    (org-roam-buffer--update-maybe :redisplay t)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (org-roam-db--clear-file (expand-file-name file))))

(defun org-roam--get-link-replacement (old-path new-path &optional old-desc new-desc)
  "Create replacement text for link at point if OLD-PATH is a match.
Will update link to NEW-PATH. If OLD-DESC is set, and is not the
same as the link description, it is assumed that the user has
modified the description, and the description will not be
updated. Else, update with NEW-DESC."
  (let (type path link-type label new-label)
    (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
      (setq type (org-element-property :type link)
            path (org-element-property :path link))
      (when (and (string-equal (expand-file-name path) old-path)
                 (org-in-regexp org-link-bracket-re 1))
        (setq link-type (when (file-name-absolute-p path) 'absolute)
              label (if (match-end 2)
                        (match-string-no-properties 2)
                      (org-link-unescape (match-string-no-properties 1))))
        (setq new-label (if (string-equal label old-desc) new-desc label))
        (org-roam-format-link new-path new-label type link-type)))))

(defun org-roam--replace-link (old-path new-path &optional old-desc new-desc)
  "Replace Org-roam file links with path OLD-PATH to path NEW-PATH.
If OLD-DESC is passed, and is not the same as the link
description, it is assumed that the user has modified the
description, and the description will not be updated. Else,
update with NEW-DESC."
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (when-let ((link (save-match-data (org-roam--get-link-replacement old-path new-path old-desc new-desc))))
        (replace-match link)))))

(defun org-roam--fix-relative-links (old-path)
  "Fix file-relative links in current buffer.
File relative links are assumed to originate from OLD-PATH. The
replaced links are made relative to the current buffer."
  (org-with-point-at 1
    (let (link new-link type path)
      (while (re-search-forward org-link-bracket-re nil t)
        (when (setq link (save-match-data (org-element-lineage (org-element-context) '(link) t)))
          (setq type (org-element-property :type link))
          (setq path (org-element-property :path link))
          (when (and (string= type "file")
                     (f-relative-p path))
            (setq new-link
                  (concat type ":" (org-roam-link-get-path (expand-file-name path (file-name-directory old-path)))))
            (replace-match new-link nil t nil 1)))))))

(defcustom org-roam-rename-file-on-title-change t
  "If non-nil, alter the filename on title change.
The new title is converted into a slug using
`kisaragi-notes//title-to-slug', and compared with the current
filename."
  :group 'org-roam
  :type 'boolean)

(defcustom org-roam-title-change-hook '(org-roam--update-file-name-on-title-change
                                        org-roam--update-links-on-title-change)
  "Hook run after detecting a title change.
Each hook is passed two arguments: the old title, and new title
respectively."
  :group 'org-roam
  :type 'hook)

(defvar-local org-roam-current-title nil
  "The current title of the Org-roam file.")

(defun org-roam--handle-title-change ()
  "Detect a title change, and run `org-roam-title-change-hook'."
  (let ((new-title (car (org-roam--extract-titles)))
        (old-title org-roam-current-title))
    (unless (or (eq old-title nil)
                (string-equal old-title new-title))
      (run-hook-with-args 'org-roam-title-change-hook old-title new-title))
    (setq-local org-roam-current-title new-title)))

(defun org-roam--setup-title-auto-update ()
  "Setup automatic link description update on title change."
  (setq-local org-roam-current-title (car (org-roam--extract-titles)))
  (add-hook 'after-save-hook #'org-roam--handle-title-change nil t))

(defun org-roam--update-links-on-title-change (old-title new-title)
  "Update the link description of other Org-roam files.
Iterate over all Org-roam files that have link description of
OLD-TITLE, and replace the link descriptions with the NEW-TITLE
if applicable.

To be added to `org-roam-title-change-hook'."
  (let* ((current-path (buffer-file-name (buffer-base-buffer)))
         (files-affected (org-roam-db-query [:select :distinct [source]
                                             :from links
                                             :where (= dest $s1)]
                                            current-path)))
    (dolist (file files-affected)
      (org-roam-with-file (car file) nil
        (org-roam--replace-link current-path current-path old-title new-title)))))

(defun org-roam--update-file-name-on-title-change (old-title new-title)
  "Update the file name on title change.
The slug is computed from OLD-TITLE using
`kisaragi-notes//title-to-slug'. If the slug is part of the
current filename, the new slug is computed with NEW-TITLE, and
that portion of the filename is renamed.

To be added to `org-roam-title-change-hook'."
  (org-roam--save-buffers)
  (when org-roam-rename-file-on-title-change
    (let* ((old-slug (kisaragi-notes//title-to-slug old-title))
           (file (buffer-file-name (buffer-base-buffer)))
           (file-name (file-name-nondirectory file)))
      (when (string-match-p old-slug file-name)
        (let* ((new-slug (kisaragi-notes//title-to-slug new-title))
               (new-file-name (replace-regexp-in-string old-slug new-slug file-name)))
          (unless (string-equal file-name new-file-name)
            (rename-file file-name new-file-name)
            (set-visited-file-name new-file-name t t)
            (org-roam-db-update)
            (org-roam-message "File moved to %S" (abbreviate-file-name new-file-name))))))))

(defun org-roam--rename-file-advice (old-file new-file-or-dir &rest _args)
  "Rename backlinks of OLD-FILE to refer to NEW-FILE-OR-DIR.
When NEW-FILE-OR-DIR is a directory, we use it to compute the new file path."
  (let ((new-file (if (directory-name-p new-file-or-dir)
                      (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                    new-file-or-dir))
        files-affected)
    (setq new-file (expand-file-name new-file))
    (setq old-file (expand-file-name old-file))
    (when (and (not (auto-save-file-name-p old-file))
               (not (auto-save-file-name-p new-file))
               (not (backup-file-name-p old-file))
               (not (backup-file-name-p new-file))
               (org-roam--org-roam-file-p old-file))
      (org-roam-db--ensure-built)
      (setq files-affected (org-roam-db-query [:select :distinct [source]
                                               :from links
                                               :where (= dest $s1)]
                                              old-file))
      ;; Remove database entries for old-file.org
      (org-roam-db--clear-file old-file)
      ;; If the new path is in a different directory, relative links
      ;; will break. Fix all file-relative links:
      (unless (string= (file-name-directory old-file)
                       (file-name-directory new-file))
        (org-roam-with-file new-file nil
          (org-roam--fix-relative-links old-file)))
      (when (org-roam--org-roam-file-p new-file)
        (org-roam-db--update-file new-file))
      ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
      (mapc (lambda (file)
              (setq file (if (string-equal (car file) old-file)
                             new-file
                           (car file)))
              (org-roam-with-file file nil
                (org-roam--replace-link old-file new-file)
                (save-buffer)
                (org-roam-db--update-file)))
            files-affected))))

(defun org-roam--id-new-advice (&rest _args)
  "Update the database if a new Org ID is created."
  (when (and org-roam-enable-headline-linking
             (org-roam--org-roam-file-p)
             (not (eq org-roam-db-update-method 'immediate))
             (not (org-roam-capture-p)))
    (org-roam-db-update)))

;;;###autoload
(define-minor-mode org-roam-mode
  "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively."
  :lighter " Org-roam"
  :keymap  (let ((map (make-sparse-keymap)))
             map)
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                     (file-executable-p emacsql-sqlite3-executable))
                (executable-find "sqlite3"))
      (lwarn '(org-roam) :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'. \
M-x info for more information at Org-roam > Installation > Post-Installation Tasks."))
    (add-to-list 'org-execute-file-search-functions 'org-roam--execute-file-row-col)
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (add-hook 'org-open-at-point-functions #'org-roam-open-id-at-point)
    (when (and (not org-roam-db-file-update-timer)
               (eq org-roam-db-update-method 'idle-timer))
        (setq org-roam-db-file-update-timer (run-with-idle-timer org-roam-db-update-idle-seconds t #'org-roam-db-update-cache-on-timer)))
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice)
    (advice-add 'org-id-new :after #'org-roam--id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (org-link-set-parameters "file" :face 'org-roam--file-link-face)
      (org-link-set-parameters "id" :face 'org-roam--id-link-face))
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (add-hook 'post-command-hook #'org-roam-buffer--update-maybe nil t)
        (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
        (add-hook 'after-save-hook #'org-roam-db-update nil t)))
    (org-roam-db-build-cache))
   (t
    (setq org-execute-file-search-functions (delete 'org-roam--execute-file-row-col org-execute-file-search-functions))
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (remove-hook 'org-open-at-point-functions #'org-roam-open-id-at-point)
    (when org-roam-db-file-update-timer
      (cancel-timer org-roam-db-file-update-timer))
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    (advice-remove 'org-id-new #'org-roam--id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))
    (org-roam-db--close-all)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (remove-hook 'post-command-hook #'org-roam-buffer--update-maybe t)
        (remove-hook 'before-save-hook #'org-roam-link--replace-link-on-save t)
        (remove-hook 'after-save-hook #'org-roam-db-update t))))))

;;; Interactive Commands
;;;###autoload
(defalias 'org-roam 'org-roam-buffer-toggle-display)

;;;###autoload
(defun org-roam-diagnostics ()
  "Collect and print info for `org-roam' issues."
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*org-roam diagnostics*"))
    (erase-buffer)
    (insert (propertize "Copy info below this line into issue:\n" 'face '(:weight bold)))
    (insert (format "- Emacs: %s\n" (emacs-version)))
    (insert (format "- Framework: %s\n"
                    (condition-case _
                        (completing-read "I'm using the following Emacs framework:"
                                         '("Doom" "Spacemacs" "N/A" "I don't know"))
                      (quit "N/A"))))
    (insert (format "- Org: %s\n" (org-version nil 'full)))
    (insert (format "- Org-roam: %s" (org-roam-version)))))

;;;###autoload
(defun org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(kisaragi-notes//title-to-slug title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

(defun kisaragi-notes/search (term)
  "Return a list of notes matching TERM."
  (with-temp-buffer
    (call-process "ag" nil '(t nil) nil
                  "--vimgrep"
                  term ".")
    (setf (point) (point-min))
    (cl-loop while (re-search-forward
                    (rx bol
                        (group (*? any)) ":" ; file name (don't put colons in there)
                        (group (+? digit)) ":" ; line
                        (group (+? digit)) ":" ; column
                        (group (* any)))
                    nil t)
             collect
             (let ((file (match-string 1))
                   (line (match-string 2))
                   (column (match-string 3))
                   (context (match-string 4)))
               (list :title (org-roam-db--get-title (expand-file-name file))
                     :tags (kisaragi-notes-db//get-file-tags (expand-file-name file))
                     :file file :line line :column column :context context)))))

;;;###autoload
(defun org-roam-find-directory ()
  "Find and open `org-roam-directory'."
  (interactive)
  (org-roam--find-file org-roam-directory))

;;;###autoload
(defun org-roam-find-ref (arg &optional filter)
  "Find and open an Org-roam file from a ref.
ARG is used to forward interactive calls to
`org-roam--get-ref-path-completions'
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate."
  (interactive "p")
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (org-roam--get-ref-path-completions arg filter))
         (ref (org-roam-completion--completing-read "Ref: "
                                                    completions
                                                    :require-match t))
         (file (-> (cdr (assoc ref completions))
                   (plist-get :path))))
    (org-roam--find-file file)))

;;;###autoload
(defun org-roam-random-note ()
  "Find a random Org-roam file."
  (interactive)
  (find-file (seq-random-elt (org-roam--list-all-files))))

;;;###autoload
(defun org-roam-insert (&optional lowercase completions filter-fn description link-type)
  "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
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
               (title-with-tags (org-roam-completion--completing-read "File: " completions
                                                                      :initial-input region-text))
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
                 (insert (org-roam-format-link target-file-path description link-type)))
                (t
                 (let ((org-roam-capture--info `((title . ,title-with-tags)
                                                 (slug . ,(kisaragi-notes//title-to-slug title-with-tags))))
                       (org-roam-capture--context 'title))
                   (setq org-roam-capture-additional-template-props (list :region (org-roam-shield-region beg end)
                                                                          :insert-at (point-marker)
                                                                          :link-type link-type
                                                                          :link-description description
                                                                          :finalize 'insert-link))
                   (org-roam-capture--capture))))
          res))
    (deactivate-mark)))

;;;###autoload
(defun org-roam-insert-immediate (arg &rest args)
  "Find an Org-roam file, and insert a relative org link to it at point.
This variant of `org-roam-insert' inserts the link immediately by
using the template in `org-roam-capture-immediate-template'. The
interactive ARG and ARGS are passed to `org-roam-insert'.
See `org-roam-insert' for details."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list org-roam-capture-immediate-template)))
    (apply #'org-roam-insert args)))

;;;###autoload
(defun org-roam-find-file-immediate (arg &rest args)
  "Find and open an Org-roam file.
This variant of `org-roam-find-file' uses the template in
`org-roam-capture-immediate-template', avoiding the capture
process. The interactive ARG and ARGS are passed to
`org-roam-find-file'. See `org-roam-find-file' for details."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list org-roam-capture-immediate-template)))
    (apply #'org-roam-find-file args)))

;;;###autoload
(defun org-roam-jump-to-index ()
  "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((index (org-roam--get-index-path)))
    (if (and index
             (file-exists-p index))
        (org-roam--find-file index)
      (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
        (org-roam-find-file "Index")))))

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

(defun org-roam-tag-add ()
  "Add a tag to Org-roam file.

Return added tag."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((all-tags (org-roam-db--get-tags))
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

;;;###autoload
(defun org-roam-switch-to-buffer ()
  "Switch to an existing Org-roam buffer."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (org-roam-db--get-title
                                                 (buffer-file-name buffer))
                                                (buffer-name buffer))
                                            buffer))
                                    roam-buffers)))
    (unless roam-buffers
      (user-error "No roam buffers"))
    (when-let ((name (org-roam-completion--completing-read "Buffer: " names-and-buffers
                                                           :require-match t)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

(defun org-roam--execute-file-row-col (s)
  "Move to row:col if S match the row:col syntax. To be used with `org-execute-file-search-functions'."
  (when (string-match (rx (group (1+ digit))
                          ":"
                          (group (1+ digit))) s)
    (let ((row (string-to-number (match-string 1 s)))
          (col (string-to-number (match-string 2 s))))
      (org-goto-line row)
      (move-to-column (- col 1))
      t)))

;;###autoload
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
                               (string-join (mapcar (lambda (glob) (concat "-g " glob))
                                                    (org-roam--list-files-search-globs org-roam-file-extensions)) " ")
                               (format " '\\[([^[]]++|(?R))*\\]%s' "
                                       (mapconcat (lambda (title)
                                                    (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                  titles ""))
                               org-roam-directory))
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
                           (not (f-equal-p (expand-file-name file org-roam-directory)
                                           file-loc)))
                  (let ((rowcol (concat row ":" col)))
                    (insert "- "
                            (org-link-make-string (concat "file:" file "::" rowcol)
                                                  (format "[%s] %s" rowcol (or (org-roam-db--get-title file)
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
(defun org-roam-version (&optional message)
  "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((version
          (with-temp-buffer
            (insert-file-contents-literally (locate-library "org-roam.el"))
            (goto-char (point-min))
            (save-match-data
              (if (re-search-forward "\\(?:;; Version: \\([^z-a]*?$\\)\\)" nil nil)
                  (substring-no-properties (match-string 1))
                "N/A")))))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

(provide 'org-roam)
;;; org-roam.el ends here
