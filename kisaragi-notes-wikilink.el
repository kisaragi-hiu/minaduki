;;; kisaragi-notes-wikilink.el --- Titles and headlines as links -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;;                  Alan Carroll

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;;
;;; Commentary:
;;
;; Links that refer to titles of files instead of filepaths.
;;
;; Currently this is a copy of org-roam-link.el.
;;
;; In Org mode, these links are written as
;;
;;     [[roam:Title of my file]]
;;
;; which will be replaced with a normal link on save or when visited.
;;
;; With a headline:
;;
;;     [[roam:Title of my file*A headline]]
;;
;; it will be replaced with an ID link.
;;
;; Current plans:
;;
;; - rename roam: prefix to note:
;; - support [[Title#Headline]] in Markdown
;; - maybe support not replacing the links
;;
;;; Code:
;;;; Dependencies

(require 'f)
(require 'ol)

(require 'kisaragi-notes-utils)
(require 'org-roam-db)

(require 'minaduki-vars)
(require 'minaduki-completion)

(require 'org-element)

(declare-function  minaduki//find-file                  "org-roam")
(declare-function  minaduki/open                  "org-roam")

;;; the roam: link
(org-link-set-parameters "roam"
                         :follow #'minaduki-link/follow-link)

(defun minaduki-link/follow-link (_path)
  "Navigates to location in Org-roam link.
This function is called by Org when following links of the type
`roam'. While the path is passed, assume that the cursor is on
the link."
  (pcase-let ((`(,link-type ,loc ,desc ,mkr) (org-roam-link--get-location)))
    (when (and org-roam-link-auto-replace loc desc)
      (org-roam-link--replace-link link-type loc desc))
    (pcase link-type
      ("file"
       (if loc
           (minaduki//find-file loc)
         (minaduki/open desc)))
      ("id"
       (org-goto-marker-or-bmk mkr)))))

;;; Retrieval Functions
(defun org-roam-link--get-headlines (&optional file with-marker use-stack)
  "Return all outline headings for the current buffer.
If FILE, return outline headings for passed FILE instead.
If WITH-MARKER, return a cons cell of (headline . marker).
If USE-STACK, include the parent paths as well."
  (org-roam-with-file file (when with-marker 'keep)
    (let* ((outline-level-fn outline-level)
           (path-separator "/")
           (stack-level 0)
           stack cands name level marker)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-complex-heading-regexp nil t)
          (save-excursion
            (setq name (substring-no-properties (or (match-string 4) "")))
            (setq marker (point-marker))
            (when use-stack
              (goto-char (match-beginning 0))
              (setq level (funcall outline-level-fn))
              ;; Update stack.  The empty entry guards against incorrect
              ;; headline hierarchies, e.g. a level 3 headline
              ;; immediately following a level 1 entry.
              (while (<= level stack-level)
                (pop stack)
                (cl-decf stack-level))
              (while (> level stack-level)
                (push name stack)
                (cl-incf stack-level))
              (setq name (mapconcat #'identity
                                    (reverse stack)
                                    path-separator)))
            (push (if with-marker
                      (cons name marker)
                    name) cands))))
      (nreverse cands))))

(defun minaduki-link//get-file-from-title (title &optional no-interactive)
  "Return the file path corresponding to TITLE.

When there are multiple options, ask the user to choose one. When
NO-INTERACTIVE is non-nil, return nil in this case."
  (let ((files (minaduki-db//query-title title)))
    (if (< (length files) 2)
        files
      (unless no-interactive
        (completing-read
         (format "More than one file has the title \"%s\". Select one: "
                 title)
         (minaduki-completion//mark-category files 'file))))))

(defun org-roam-link--get-id-from-headline (headline &optional file)
  "Return (marker . id) correspondng to HEADLINE in FILE.
If FILE is nil, get ID from current buffer.
If there is no corresponding headline, return nil."
  (save-excursion
    (org-roam-with-file file 'keep
      (let ((headlines (org-roam-link--get-headlines file 'with-markers)))
        (when-let ((marker (cdr (assoc-string headline headlines))))
          (goto-char marker)
          (cons marker
                (when org-roam-link-auto-replace
                  (org-id-get-create))))))))

;;; Path-related functions
(defun org-roam-link-get-path (path &optional type)
  "Return the PATH of the link to use.
If TYPE is non-nil, create a link of TYPE. Otherwise, respect
`org-link-file-path-type'."
  (pcase (or type org-roam-link-file-path-type)
    ('absolute
     (abbreviate-file-name (expand-file-name path)))
    ('noabbrev
     (expand-file-name path))
    ('relative
     (file-relative-name path))))

(defun org-roam-link--split-path (path)
  "Splits PATH into title and headline.
Return a list of the form (type title has-headline-p headline star-idx).
type is one of `title', `headline', `title+headline'.
title is the title component of the path.
headline is the headline component of the path.
star-idx is the index of the asterisk, if any."
  (save-match-data
    (let* ((star-index (string-match-p "\\*" path))
           (title (substring-no-properties path 0 star-index))
           (headline (if star-index
                         (substring-no-properties path (+ 1 star-index))
                       ""))
           (type (cond ((not star-index)
                        'title)
                       ((= 0 star-index)
                        'headline)
                       (t 'title+headline))))
      (list type title headline star-index))))

(defun org-roam-link--get-location ()
  "Return the location of the Org-roam fuzzy link at point.
The location is returned as a list containing (link-type loc desc marker).
nil is returned if there is no matching location.

link-type is either \"file\" or \"id\".
loc is the target location: e.g. a file path, or an id.
marker is a marker to the headline, if applicable.

desc is either the the description of the link under point, or
the target of LINK (title or heading content)."
  (let ((context (org-element-context))
        mkr link-type desc loc)
    (pcase (org-element-lineage context '(link) t)
      (`nil (error "Not at an Org link"))
      (link
       (if (not (string-equal "roam" (org-element-property :type link)))
           (error "Not at Org-roam link")
         (setq desc (and (org-element-property :contents-begin link)
                         (org-element-property :contents-end link)
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin link)
                          (org-element-property :contents-end link))))
         (pcase-let ((`(,type ,title ,headline _) (org-roam-link--split-path
                                                   (org-element-property :path link))))
           (pcase type
             ('title+headline
              (let ((file (minaduki-link//get-file-from-title title)))
                (if (not file)
                    (org-roam-message "Cannot find matching file")
                  (setq mkr (org-roam-link--get-id-from-headline headline file))
                  (pcase mkr
                    (`(,marker . ,target-id)
                     (progn
                       (setq mkr marker
                             loc target-id
                             desc (or desc headline)
                             link-type "id")))
                    (_ (org-roam-message "Cannot find matching id"))))))
             ('title
              (setq loc (minaduki-link//get-file-from-title title)
                    link-type "file"
                    desc (or desc title)))
             ('headline
              (setq mkr (org-roam-link--get-id-from-headline headline))
              (pcase mkr
                (`(,marker . ,target-id)
                 (setq mkr marker
                       loc target-id
                       link-type "id"
                       desc (or desc headline)))
                (_ (org-roam-message "Cannot find matching headline")))))))))
    (list link-type loc desc mkr)))

;;; Conversion Functions
(defun org-roam-link--replace-link (type loc &optional desc)
  "Replace link at point with a vanilla Org link.
TYPE is the Org link type, typically \"file\" or \"id\".
LOC is path for the Org link.
DESC is the link description."
  (save-excursion
    (save-match-data
      (unless (org-in-regexp org-link-bracket-re 1)
        (user-error "No link at point"))
      (replace-match "")
      (insert (org-roam-format-link loc desc type)))))

(defun org-roam-link-replace-all ()
  "Replace all roam links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (condition-case nil
          (pcase-let ((`(,link-type ,loc ,desc _) (org-roam-link--get-location)))
            (when (and link-type loc)
              (org-roam-link--replace-link link-type loc desc)))
        (error nil)))))

(defun org-roam-link--replace-link-on-save ()
  "Hook to replace all roam links on save."
  (when org-roam-link-auto-replace
    (org-roam-link-replace-all)))

(provide 'kisaragi-notes-wikilink)
;;; kisaragi-notes-wikilink.el ends here
