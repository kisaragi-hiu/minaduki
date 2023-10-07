;;; minaduki-wikilink.el --- Titles and headlines as links -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;;
;;; Mostly a copy of org-roam-link.el from Org-roam v1, by Jethro Kuan
;;; <jethrokuan95@gmail.com>.
;;;
;;; This is a special syntax for writing links with titles.
;;;
;;;   [[minaduki:File title]]
;;;   [[minaduki:File title*Headline]]
;;;
;;; By default, these are replaced with an ID link (if given a
;;; headline) or a normal link (if not) on save. This can be turned of
;;; by setting the `minaduki-wikilink-auto-replace' user option to nil.
;;
;; FIXME: [[minaduki:nonexistent][some description]] would try to open "some
;; description" and error out
;; FIXME: [[minaduki:nonexistent]] would try to open "nonexistent" and still
;; error out instead of creating a note (a problem with minaduki/open?)
;;;
;;; Code:

;;;; Dependencies

(require 'f)
(require 'map)
(require 'ol)
(require 'org-element)

(require 'minaduki-utils)
(require 'minaduki-db)
(require 'minaduki-vars)
(require 'minaduki-completion)

(declare-function minaduki/open "minaduki-commands" (&optional entry))

(defconst minaduki-wikilink::type "minaduki"
  "The link type for Minaduki Org wikilinks.

The \"minaduki\" in an Org wikilink like \"[[minaduki:title here]]\".")

(defun minaduki-wikilink:follow (_path)
  "Follow a minaduki: wikilink.
This function is called by Org when following links of the type
`minaduki'. This is registered by `minaduki-mode'."
  (pcase-let ((`(,link-type ,loc ,desc ,mkr) (minaduki-wikilink::get-location)))
    (when (and minaduki-wikilink-auto-replace loc desc)
      (minaduki-wikilink::replace-link link-type loc desc))
    (pcase link-type
      ("file"
       (if loc
           (minaduki::find-file loc)
         (minaduki/open desc)))
      ("id"
       (org-goto-marker-or-bmk mkr)))))

;;; Retrieval Functions
(defun minaduki-wikilink::get-headlines (&optional file with-marker use-stack)
  "Return all outline headings for the current buffer.
If FILE, return outline headings for passed FILE instead.
If WITH-MARKER, return a cons cell of (headline . marker).
If USE-STACK, include the parent paths as well."
  (minaduki::with-file file (when with-marker 'keep)
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

(defun minaduki-wikilink::get-file-from-title (title &optional no-interactive)
  "Return the file path corresponding to TITLE.

When there are multiple options, ask the user to choose one. When
NO-INTERACTIVE is non-nil, return nil in this case."
  (let ((files (minaduki-db::fetch-file :title title)))
    (if (< (length files) 2)
        (car files)
      (unless no-interactive
        (completing-read
         (format "More than one file has the title \"%s\". Select one: "
                 title)
         (minaduki-completion//mark-category files 'file))))))

(defun minaduki-wikilink::get-id-from-headline (headline &optional file)
  "Return (marker . id) correspondng to HEADLINE in FILE.
If FILE is nil, get ID from current buffer.
If there is no corresponding headline, return nil."
  (save-excursion
    (minaduki::with-file file 'keep
      (let ((headlines (minaduki-wikilink::get-headlines file 'with-markers)))
        (when-let ((marker (cdr (assoc-string headline headlines))))
          (goto-char marker)
          (cons marker
                (when minaduki-wikilink-auto-replace
                  (org-id-get-create))))))))

(defun minaduki-wikilink::split-path (path)
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

(defun minaduki-wikilink::get-location ()
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
       (if (not (string-equal minaduki-wikilink::type
                              (org-element-property :type link)))
           (error "Not at an Minaduki Org wikilink")
         (setq desc (and (org-element-property :contents-begin link)
                         (org-element-property :contents-end link)
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin link)
                          (org-element-property :contents-end link))))
         (pcase-let ((`(,type ,title ,headline _) (minaduki-wikilink::split-path
                                                   (org-element-property :path link))))
           (pcase type
             ('title+headline
              (let ((file (minaduki-wikilink::get-file-from-title title)))
                (if (not file)
                    (minaduki::message "Cannot find matching file")
                  (setq mkr (minaduki-wikilink::get-id-from-headline headline file))
                  (pcase mkr
                    (`(,marker . ,target-id)
                     (progn
                       (setq mkr marker
                             loc target-id
                             desc (or desc headline)
                             link-type "id")))
                    (_ (minaduki::message "Cannot find matching id"))))))
             ('title
              (setq loc (minaduki-wikilink::get-file-from-title title)
                    link-type "file"
                    desc (or desc title)))
             ('headline
              (setq mkr (minaduki-wikilink::get-id-from-headline headline))
              (pcase mkr
                (`(,marker . ,target-id)
                 (setq mkr marker
                       loc target-id
                       link-type "id"
                       desc (or desc headline)))
                (_ (minaduki::message "Cannot find matching headline")))))))))
    (list link-type loc desc mkr)))

;;; Conversion Functions
(defun minaduki-wikilink::replace-link (type loc &optional desc)
  "Replace link at point with a vanilla Org link.
TYPE is the Org link type, typically \"file\" or \"id\".
LOC is path for the Org link.
DESC is the link description."
  (save-excursion
    (save-match-data
      (unless (org-in-regexp org-link-bracket-re 1)
        (user-error "No link at point"))
      (replace-match "")
      (insert (minaduki::format-link :target loc
                                     :desc desc
                                     :id? (equal type "id"))))))

(defun minaduki-wikilink:replace-all ()
  "Replace all wikilinks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (condition-case nil
          (pcase-let ((`(,link-type ,loc ,desc _) (minaduki-wikilink::get-location)))
            (when (and link-type loc)
              (minaduki-wikilink::replace-link link-type loc desc)))
        (error nil)))))

(defun minaduki-wikilink::replace-link-on-save ()
  "Hook to replace all roam links on save."
  (when minaduki-wikilink-auto-replace
    (minaduki-wikilink:replace-all)))

(provide 'minaduki-wikilink)
;;; minaduki-wikilink.el ends here
