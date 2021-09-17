;;; kisaragi-notes-vars.el --- Variable declarations -*- lexical-binding: t -*-

;;; Commentary:

;; Variable declarations intended to make extra `defvar's unnecessary.

;;; Code:

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
    Extract sub-directories relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tags \"foo\" and \"bar\".

  `org-roam--extract-tags-last-directory'
    Extract the last directory relative to `org-roam-directory'.
    That is, if a file is located at relative path foo/bar/file.org,
    the file will have tag \"bar\".

  `org-roam--extract-tags-first-directory'
    Extract the first directory relative to `org-roam-directory'.
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

(provide 'kisaragi-notes-vars)

;;; kisaragi-notes-vars.el ends here
