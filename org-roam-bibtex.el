;;; org-roam-bibtex.el --- Org Roam meets BibTeX -*- lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; 	Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam-bibtex
;; Keywords: bib, hypermedia, outlines, wp
;; Verstion 0.4.0
;; Package-Requires: ((emacs "27.1") (bibtex-completion "2.0.0"))

;; Soft dependencies: projectile, persp-mode

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
;; You should have received a copy of the GNU General Public License along with
;; this program; see the file LICENSE.  If not, visit
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library offers an integration between Bibtex-completion and
;; Org-roam by delegating the tasks of note's creation, editing and
;; retrieval to Org-roam.  From the Org-roam's perspective, the
;; library provides a means to populate Org-roam templates with
;; bibliographic information secured through Bibtex-completion,.
;;
;; To use it:
;;
;; call interactively `org-roam-bibtex-mode' or
;; call (org-roam-bibtex-mode +1) from Lisp.
;;
;; After enabling `org-roam-bibtex-mode', the function
;; `orb-edit-notes' will shadow `bibtex-completion-edit-notes' in
;; Helm-bibtex, Ivy-bibtex.
;;
;; Additionally, `orb-notes-fn', which is a simple wrapper around
;; `orb-edit-notes', is installed as Org-ref's
;; `org-ref-notes-function'.  See Org-ref's documentation for how to
;; setup many-files notes.  Take a notice that because of its size,
;; Org-ref is not a dependency of Org-roam-bibtex, so it will not be
;; pulled automatically by your package manager and must be installed
;; manually.
;;
;; As a user option, `org-roam-capture-templates' can be dynamically
;; preformatted with bibtex field values.  See
;; `orb-preformat-keywords' for more details.
;;
;; Optionally, automatic switching to the perspective (Persp-mode)
;; with the notes project (Projectile) is possible.  See
;; `orb-edit-notes' for more details.
;;

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'orb-core)
(require 'kisaragi-notes-utils)
(require 'kisaragi-notes-vars)

(require 'org-roam-capture)

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

;; declare external functions and variables

(defvar bibtex-completion-bibliography)
(defvar bibtex-completion-find-note-functions)
(declare-function bibtex-completion-get-value
                  "bibtex-completion" (field entry &optional default))
(declare-function bibtex-completion-get-entry
                  "bibtex-completion" (entry-key))
(declare-function bibtex-completion-clear-cache
                  "bibtex-completion" (&optional files))
(declare-function bibtex-completion-init "bibtex-completion")
(declare-function bibtex-completion-candidates "bibtex-completion")

(declare-function projectile-relevant-open-projects "projectile")
(declare-function persp-switch "persp-mode")
(declare-function persp-names "persp-mode")

(defvar org-ref-notes-function)
(declare-function org-ref-find-bibliography "org-ref-core")

(declare-function org-ref-format-entry "ext:org-ref-bibtex" (key))

;; ============================================================================
;;;; Customize definitions
;; ============================================================================

;; ============================================================================
;;;; Orb plist
;; ============================================================================

(defvar orb-plist nil
  "Communication channel for `orb-edit-notes' and related functions.")

(defun orb-plist-put (&rest props)
  "Add properties PROPS to `orb-plist'.
Returns the new plist."
  (while props
    (setq orb-plist (plist-put orb-plist (pop props) (pop props)))))

(defun orb-plist-get (prop)
  "Get PROP from `orb-plist'."
  (plist-get orb-plist prop))

(defun orb-cleanup ()
  "Clean up `orb-plist'."
  (let ((keywords (-filter #'keywordp orb-plist)))
    (dolist (keyword keywords)
      (orb-plist-put keyword nil))))

(defmacro with-orb-cleanup (&rest body)
  "Execute BODY calling `orb-cleanup' as its last form.
Return the result of executing BODY."
  (declare (indent 0) (debug t))
  `(prog1
       ,@body
     (orb-cleanup)))

;; ============================================================================
;;;; Managing Org-capture hooks (experimental)
;; ============================================================================

;; workflow:
;;
;; 1. (orb-register-hook-function func before nil (my-forms))
;;
;; 2. In `orb-edit-notes', if a note does not exist, an `org-capture' process is
;; started.  Before that, the function `func' is added
;; `org-capture-before-finalize-hook' with 0 depth by calling
;; `orb-do-hook-functions'.  Forms (my-forms) will be run by `org-capture'
;; within the hook.  The closure removes itself, so that it does not interfere
;; with any subsequent `org-capture' calls.  `orb-edit-notes' also takes care
;; to remove the function from the hook in case the `org-capture' process was
;; aborted.
;;
;; 3. If the note exists, the function will not be added to the hook, but it is
;; still possible to execute `my-forms' by calling:
;;
;; (orb-call-hook-function 'func)
;;
;; 4. The function is registered until `orb-clean' is run.

(defmacro orb-register-hook-function (name target depth &rest body)
  "Register a closure to be run in one of the `org-capture' hooks.
NAME (unquoted) is the name of the function.  TARGET should be an
unquoted SYMBOL, one of `prepare', `before' or `after', meaning
the function will be registered to run with the corresponding
`org-capture-SYMBOL-finlaize-hook'.  DEPTH is the hook depth, nil
is internally converted to 0.

BODY are forms which will be wrapped in an anonymous function
within a `letrec' form.  Additionally, a `remove-hook' called is
appended to BODY, making the closure self-removable:

\(letrec ((NAME
           (lambda ()
            BODY
           (remove-hook 'org-capture-TARGET-finalize-hook NAME)))))

These hook functions are therefore meant to run only in next
`org-capture' session.

The function is not actually added to a hook but is instead
registered on `orb-plist'.  The function `orb-edit-notes'
installs the hooks just before starting an `org-capture' process
by calling `orb-do-hook-functions'.  It also takes care of
removing the hooks in case the `org-capture' process was aborted.

After a function has been registered, it is possible to call it
by passing its NAME as a quoted symbol to
`orb-call-hook-function'.  This may be useful if the function
should be run regardless of whether an `org-capture' process was
initiated or not."
  (declare (indent 3) (debug t))
  (let ((hookvar (intern (format "org-capture-%s-finalize-hook" target)))
        (keyword (intern (format ":%s-functions" target)))
        (depth (or depth 0)))
    `(letrec ((,name (lambda () ,@body (remove-hook (quote ,hookvar) ,name))))
       (orb-plist-put ,keyword
                      (cons (list (quote ,name) ,name ,depth)
                            (orb-plist-get ,keyword))))))

(defun orb-call-hook-function (name)
  "Call function NAME registered by `orb-register-hook-function'."
  (let* ((functions (append (orb-plist-get :prepare-functions)
                            (orb-plist-get :before-functions)
                            (orb-plist-get :after-functions)))
         (func (alist-get name functions)))
    (when func (funcall (car func)))))

(defun orb-do-hook-functions (action &optional targets)
  "Add or remove functions to `org-capture-...-finilize-hook's.
ACTION should be a symbol `add' or `remove'.  If optional TARGETS
list is provided, do only the hooks in TARGETS.  TARGETS should
be any of symbols `prepare', `before' and `after'.  TARGETS can
also be a single symbols.  If TARGETS is nil, a list of all three
symbols is implied."
  (let* ((targets (--> (if (and targets (not (listp targets)))
                           (listp targets)
                         targets)
                    ;; filter irrelevant symbols,
                    ;; or if targets were nil, make all targets
                    (or (-intersection it '(prepare before after))
                        '(prepare before after)))))
    (dolist (target targets)
      (let ((functions (sort (orb-plist-get
                              (intern (format ":%s-functions" target)))
                             (lambda (a b)
                               (> (nth 2 a) (nth 2 b))))))
        (dolist (func functions)
          (let ((f (intern (format "%s-hook" action)))
                (hook (intern (format "org-capture-%s-finalize-hook" target))))
            (funcall f hook (when (eq action 'add)
                              (nth 1 func)))))))))

;; ============================================================================
;;;; Orb edit notes
;; ============================================================================

(defun orb--switch-perspective ()
  "Helper function for `orb-edit-notes'."
  (when (and (require 'projectile nil t)
             (require 'persp-mode nil t))
    (let ((notes-project (cdr orb-persp-project))
          (projects (projectile-relevant-open-projects))
          openp)
      (dolist (project projects openp)
        (setq openp (or (f-equal? project notes-project) openp)))
      (when openp
        (let ((p-names (cdr (persp-names))))
          (dolist (p-name p-names)
            (when (s-equals? p-name (car orb-persp-project))
              (persp-switch p-name))))))))

(defun orb--store-link-functions-advice (action)
  "Add or remove advice for each of `orb-ignore-bibtex-store-link-functions'.
ACTION should be a symbol `add' or `remove'.  A piece of advice
is the function `ignore', it is added as `:override'."
  (when orb-ignore-bibtex-store-link-functions
    (let ((advice-func (intern (format "advice-%s" action)))
          (advice (cl-case action
                    (add (list :override #'ignore))
                    (remove (list #'ignore))
                    (t (user-error "Action type not recognised: %s" action)))))
      (dolist (advisee orb-ignore-bibtex-store-link-functions)
        (apply advice-func (push advisee advice))))))

(defun orb--preformat-template (template entry)
  "Helper function for `orb--preformat-templates'.
TEMPLATE is an element of `org-roam-capture-templates' and ENTRY
is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  ;; Handle org-roam-capture part
  (let* (;; Org-capture templates: handle different types of
         ;; org-capture-templates: string, file and function; this is
         ;; a stripped down version of `org-capture-get-template'
         (tp
          (pcase (nth 4 template)       ; org-capture template is here
            (`nil 'nil)
            ((and (pred stringp) tmpl) tmpl)
            (`(file ,file)
             (let ((flnm (expand-file-name file org-directory)))
               (if (file-exists-p flnm) (f-read-text flnm)
                 (format "Template file %S not found" file))))
            (`(function ,fun)
             (if (functionp fun) (funcall fun)
               (format "Template function %S not found" fun)))
            (_ "Invalid capture template")))
         ;;  org-roam capture properties are here
         (plst (cdr template))
         ;; regexp for org-capture prompt wildcard
         (rx "\\(%\\^{[[:alnum:]-_]*}\\)")
         (file-keyword (when orb-process-file-keyword
                         (or (and (stringp orb-process-file-keyword)
                                  orb-process-file-keyword)
                             "file")))
         lst)
    ;; First run:
    ;; 1) Make a list of (rplc-s field-value match-position) for the
    ;; second run
    ;; 2) replace org-roam-capture wildcards
    (dolist (keyword orb-preformat-keywords)
      (let* (;; prompt wildcard keyword
             ;; bibtex field name
             (field-name (or (car (rassoc keyword orb-bibtex-field-aliases))
                             keyword))
             ;; get the bibtex field value
             (field-value
              ;; maybe process file keyword
              (or (if (and file-keyword (string= field-name file-keyword))
                      (prog1
                          (orb-process-file-field
                           (bibtex-completion-get-value "=key=" entry))
                        ;; we're done so don't even compare file-name with
                        ;; file-keyword in the successive cycles
                        (setq file-keyword nil))
                    ;; do the usual processing otherwise
                    ;; condition-case to temporary workaround an upstream bug
                    (condition-case nil
                        (bibtex-completion-get-value field-name entry)
                      (error "")))
                  ""))
             ;; org-capture prompt wildcard
             (rplc-s (concat "%^{" (or keyword "citekey") "}"))
             ;; org-roam-capture prompt wildcard
             (rplc-s2 (concat "${" (or keyword "citekey") "}"))
             ;; org-roam-capture :head template
             (head (plist-get plst :head))
             ;; org-roam-capture :file-name template
             (fl-nm (plist-get plst :file-name))
             (i 1)                        ; match counter
             pos)
        ;; Search for rplc-s, set flag m if found
        (when tp
          (while (string-match rx tp pos)
            (if (string= (match-string 1 tp) rplc-s)
                (progn
                  (setq pos (length tp))
                  (cl-pushnew (list rplc-s field-value i) lst))
              (setq pos (match-end 1)
                    i (1+ i)))))
        ;; Replace org-roam-capture prompt wildcards
        (when head
          (plist-put plst :head (s-replace rplc-s2 field-value head)))
        (when fl-nm
          (plist-put plst :file-name (s-replace rplc-s2 field-value fl-nm)))))
    ;; Second run: replace prompts and prompt matches in org-capture
    ;; template string
    (dolist (l lst)
      (when (and tp (nth 1 l))
        (let ((pos (concat "%\\" (number-to-string (nth 2 l)))))
          ;; replace prompt match wildcards with prompt wildcards
          ;; replace prompt wildcards with BibTeX field value
          (setq tp (s-replace pos (car l) tp)
                tp (s-replace (car l) (nth 1 l) tp))))
      (setf (nth 4 template) tp))
    template))

(defun orb--edit-notes (citekey)
  "Process templates and run `org-roam-capture--capture'.
CITEKEY is a citation key.
Helper function for `orb-edit-notes', which abstracts initiating
a capture session."
  ;; Check if the requested BibTeX entry actually exists and fail
  ;; gracefully otherwise
  (if-let* ((entry (or (bibtex-completion-get-entry citekey)
                       (orb-warning
                        "Could not find the BibTeX entry" citekey)))
            ;; Depending on the templates used: run
            ;; `org-roam-capture--capture' or call `org-roam-find-file'
            (org-capture-templates
             (or orb-templates org-roam-capture-templates
                 (orb-warning "Could not find the requested templates")))
            ;; hijack org-capture-templates
            ;; entry is our bibtex entry, it just happens that
            ;; `org-capture' calls a single template entry "entry";
            (template (--> (if (null (cdr org-capture-templates))
                               ;; if only one template is defined, use it
                               (car org-capture-templates)
                             (org-capture-select-template))
                        (copy-tree it)
                        ;; optionally preformat templates
                        (if orb-preformat-templates
                            (orb--preformat-template it entry)
                          it)))
            ;; pretend we had only one template
            ;; `org-roam-capture--capture' behaves specially in this case
            ;; NOTE: this circumvents using functions other than
            ;; `org-capture', see `org-roam-capture-function'.
            ;; If the users start complaining, we may revert previous
            ;; implementation
            (org-roam-capture-templates (list template))
            ;; Org-roam coverts the templates to its own syntax;
            ;; since we are telling `org-capture' to use the template entry
            ;; (by setting `org-capture-entry'), and Org-roam converts the
            ;; whole template list, we must do the conversion of the entry
            ;; ourselves
            (org-capture-entry
             (org-roam-capture--convert-template template))
            (citekey-formatted (format (or orb-citekey-format "%s") citekey))
            (title
             (or (bibtex-completion-get-value "title" entry)
                 (orb-warning "Title not found for this entry")
                 ;; this is not critical, the user may input their own
                 ;; title
                 "Title not found")))
      (progn
        ;; fix some Org-ref related stuff
        (orb--store-link-functions-advice 'add)
        (unwind-protect
            ;; data collection hooks functions: remove themselves once run
            (progn
              ;; install capture hook functions
              (orb-do-hook-functions 'add)
              ;; Depending on the templates used: run
              ;; `org-roam-capture--capture' with ORB-predefined
              ;; settings or call vanilla `org-roam-find-file'
              (if orb-templates
                  (let* ((org-roam-capture--context 'ref)
                         (slug-source (cl-case orb-slug-source
                                        (citekey citekey)
                                        (title title)
                                        (t (user-error "Only `citekey' \
or `title' should be used for slug: %s not supported" orb-slug-source))))
                         (org-roam-capture--info
                          `((title . ,title)
                            (ref . ,citekey-formatted)
                            ,@(when-let (url (bibtex-completion-get-value "url" entry))
                                `((url . ,url)))
                            (slug . ,(kisaragi-notes//title-to-slug slug-source)))))
                    (setq org-roam-capture-additional-template-props
                          (list :finalize 'find-file))
                    (org-roam-capture--capture))
                (kisaragi-notes/open title)))
          (orb--store-link-functions-advice 'remove)))
    (message "ORB: Something went wrong. Check the *Warnings* buffer")))

;;;###autoload
(defun orb-edit-notes (citekey)
  "Open an Org-roam note associated with the CITEKEY or create a new one.

CITEKEY is normally a string. When it's a list, the first entry
is used as the key. This allows us to receive the same arguments
as `bibtex-completion' commands such as
`bibtex-completion-show-entry'.

This function allows to use Org-roam as a backend for managing
bibliography notes.  It relies on `bibtex-completion' to get
retrieve bibliographic information from a BibTeX file.

Implementation details and features:

1. This function first calls `org-roam-find-ref' trying to find
the note file associated with the CITEKEY.  The Org-roam key can
be set with '#+ROAM_KEY:' in-buffer keyword.

2. If the Org-roam reference has not been found, the function
calls `org-roam-find-file' passing to it the title associated
with the CITEKEY as retrieved by `bibtex-completion-get-entry'.
The prompt presented by `org-roam-find-file' will thus be
pre-populated with the record title.

3. The template used to create the note is stored in
`orb-templates'.  If the variable is not defined, revert to using
`org-roam-capture-templates'.  In the former case, a new file
will be created and filled according to the template, possibly
preformatted (see below) without additional user interaction.  In
the latter case, an interactive `org-capture' process will be
run.

4. Optionally, when `orb-preformat-templates' is non-nil, any
prompt wildcards in `orb-templates' or
`org-roam-capture-templates', associated with the bibtex record
fields as specified in `orb-preformat-templates', will be
preformatted.  Both `org-capture-templates' (%^{}) and
`org-roam-capture-templates' (`s-format', ${}) prompt syntaxes
are supported.

See `orb-preformat-keywords' for more details on how
to properly specify prompts for replacement.

Please pay attention when using this feature that by setting
title for preformatting, it will be impossible to change it in
the `org-roam-find-file' interactive prompt since all the
template expansions will have taken place by then.  All the title
wildcards will be replace with the BibTeX field value.

5. Optionally, if you are using Projectile and Persp-mode and
have a dedicated workspace to work with your Org-roam collection,
you may want to set the perspective name and project path in
`orb-persp-project' and `orb-switch-persp' to t.  In this case,
the perspective will be switched to the Org-roam notes project
before calling any Org-roam functions."
  (when (consp citekey)
    (setq citekey (car citekey)))
  (unless org-roam-mode (org-roam-mode))
  ;; Optionally switch to the notes perspective
  (when orb-switch-persp
    (orb--switch-perspective))
  (let ((note-data (orb-note-exists-p citekey)))
    ;; Find org-roam reference with the CITEKEY and collect data into
    ;; `orb-plist'
    (orb-plist-put :note-existed (and note-data t))
    (cond
     (note-data
      (apply #'orb-plist-put (cdr note-data))
      (ignore-errors (org-roam--find-file (orb-plist-get :file))))
     ;; we need to clean up if the capture process was aborted signaling
     ;; user-error
     (t (condition-case nil
            (orb--edit-notes citekey)
          (error
           (with-orb-cleanup (orb-do-hook-functions 'remove))))))))


;; ============================================================================
;;;; Orb insert
;; ============================================================================

(defun orb-insert--link (file citekey &optional description lowercase)
  "Insert a link to FILE.
If a region is active, replace the region with the link and used
the selected text as the link's label.  If DESCRIPTION is
provided, use it as the link's label instead.  If none of the
above is true, insert the CITEKEY as a formatted Org-ref citation
using `org-ref-default-citation-link' or 'cite:' if this variable
is not bound.

If LOWERCASE is non-nil, downcase the link description.
Return the filename if it exists."
  ;; Deactivate the mark on quit since `atomic-change-group' prevents it
  (unwind-protect
      ;; Group functions together to avoid inconsistent state on quit
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text
                          (buffer-substring-no-properties beg end))))
               (description (or region-text description)))
          (when (and file (file-exists-p file))
            (when region-text
              (delete-region beg end)
              (set-marker beg nil)
              (set-marker end nil))
            (if description
                (let ((description (if lowercase
                                       (downcase description)
                                     description)))
                  (insert (org-roam-format-link file description)))
              (let ((cite-link (if (boundp 'org-ref-default-citation-link)
                                   (concat org-ref-default-citation-link ":")
                                 "cite:")))
                (insert (concat cite-link citekey))))
            ;; return value
            file)))
    (deactivate-mark)))

(defvar orb-insert-lowercase nil)
(defun orb-insert--link-h ()
  "Prepare the environement and call `orb-insert--link'."
  ;; insert link only when file is non-nil
  (with-orb-cleanup
    (when-let ((file (orb-plist-get :file)))
      (let* ((citekey (orb-plist-get :citekey))
             (insert-description (or (orb-plist-get :link-description)
                                     orb-insert-link-description))
             (lowercase (or (orb-plist-get :link-lowercase)
                            orb-insert-lowercase))
             (description (cl-case insert-description
                            (title (orb-plist-get :title))
                            (citekey citekey)
                            (citation nil))))
        (with-current-buffer (orb-plist-get :buffer)
          (save-excursion
            (orb-insert--link file citekey description lowercase)))))
    (set-window-configuration (orb-plist-get :window-conf))
    (when (and orb-insert-follow-link
               (looking-at org-link-any-re))
      (org-open-at-point))
    ;; if any left
    (orb-do-hook-functions 'remove)))

(defun orb-insert-generic (&optional arg)
  "Present a list of BibTeX entries for completion.
This is a generic completion function for `orb-insert', which
runs `orb-insert-edit-notes' on the selected entry.  The list is
made by `bibtex-completion-candidates'.

The appearance of selection candidates is determined by
`orb-insert-generic-candidates-format'.

This function is not interactive, call `orb-insert' interactively
instead.

If ARG is non-nil, rebuild `bibtex-completion-cache'."
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (candidates2
          (if (eq orb-insert-generic-candidates-format 'key)
              (mapcar (lambda (item)
                        (alist-get "=key=" (cdr item) nil nil #'equal))
                      candidates)
            (mapcar #'car candidates)))
         (selection (completing-read "BibTeX entry:" candidates2 nil t))
         (citekey (if (eq orb-insert-generic-candidates-format 'key)
                      selection
                    (--> (alist-get selection candidates nil nil #'equal)
                      (cdr it)
                      (alist-get "=key=" it  nil nil #'equal)))))
    (orb-insert-edit-notes (list citekey))))

(defun orb-insert-edit-notes (citekey)
  "Call `orb-edit-notes' and insert a link to a note.
CITEKEY is a citation key and #+ROAM_KEY of the retrieved or
newly created note."
  (orb-plist-put :buffer (current-buffer)
                 :window-conf (current-window-configuration)
                 :citekey (car citekey))
  ;; Here, we play on the specifics of a capture process.
  ;; `org-capture-finalize' runs `prepare-hook', `before-hook' and `after-hook'
  ;; in that order.  But, if `org-capture-finalize' was run via
  ;; `org-capture-kill', the `before-hook' is forced to nil via a let form.
  ;;
  ;; 1. In an interactive session, we get the title while the capture buffer
  ;; still exists.  But if the capture process was killed, our before hook
  ;; function did not run and therefore title is nil on `orb-plist'.
  (orb-register-hook-function get-title before nil
    (orb-plist-put :title (orb-get-buffer-keyword "title")
                   :immediate-finish
                   (plist-get org-capture-plist :immediate-finish)))

  (orb-register-hook-function get-file after -90
    (let ((file (buffer-file-name)))
      ;; 2. We check whether the title on `orb-plist' is nil.  When it is, we
      ;; set file to nil to signal `org-insert--link-h' not to insert a link.
      ;; We do this only in interactive process
      (unless (or (orb-plist-get :immediate-finish)
                  (orb-plist-get :title))
        (setq file nil)
        ;; before hook functions did not run, so they are still in
        ;; `org-capture-before-finalize-hook'; remove them.
        (orb-do-hook-functions 'remove 'before))
      (orb-plist-put :file file)))

  (orb-register-hook-function insert-link after 90
    (orb-insert--link-h))

  (save-excursion
    (orb-edit-notes (car citekey)))
  ;; when note existed, a capture process did not run.  We have all the info on
  ;; `orb-plist', so just insert a link
  (when (orb-plist-get :note-existed)
    ;; we call the hook function so that the hook is removed
    (orb-call-hook-function 'insert-link)))

;;;###autoload
(defun orb-insert (&optional arg)
  "Insert a link to an Org-roam bibliography note.

If the note does not exist, create it using `orb-insert-generic'.

A simple list of available citation keys will be presented using
`completion-read' and after choosing a candidate the appropriate
link will be inserted.

If the note does not exist yet, it will be created using
`orb-edit-notes' function.

\\<universal-argument-map>\\<org-roam-bibtex-mode-map> The
customization option `orb-insert-link-description' determines
what will be used as the link's description.  It is possible to
override the default value with numerical prefix ARG:

`C-1' \\[orb-insert] will force `title'
`C-2' \\[orb-insert] will force `citekey'
`C-0' \\[orb-insert] will force `citation'

If a region of text is active (selected) when calling `orb-insert',
the text in the region will be replaced with the link and the
text string will be used as the link's description — similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied."
  (interactive "P")
  ;; parse arg
  ;; C-u or C-u C-u C-u => force lowercase
  ;; C-u C-u or C-u C-u C-u => force `bibtex-completion-clear-cache'
  ;; C-1 force title in description
  ;; C-2 force citekey in description
  ;; C-3 force inserting the link as Org-ref citation
  (let* ((lowercase (or (equal arg '(4))
                        (equal arg '(64))))
         (description (cl-case arg
                        (1 'title)
                        (2 'citekey)
                        (0 'citation)))
         (clear-cache (or (equal arg '(16))
                          (equal arg '(64)))))
    (orb-plist-put :link-description
                   (or description orb-insert-link-description)
                   :link-lowercase
                   (or lowercase orb-insert-lowercase))
    (unless org-roam-mode (org-roam-mode))
    ;; execution chain:
    ;; 1. interface function
    ;; 2. orb-insert-edit-notes
    ;; 3. orb-edit-notes
    ;; 4. orb-insert--link-h
    ;; if the note exists or a new note was created and capture not cancelled
    ;; 5. orb-insert--link
    (orb-insert-generic clear-cache)))

;; ============================================================================
;;;; Non-ref functions
;; ============================================================================

;;;###autoload
(defun orb-insert-non-ref (prefix)
  "Find a non-ref Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion.  See
`org-roam-insert' and `kisaragi-notes-completion//get-non-literature' for
details."
  (interactive "P")
  (org-roam-insert prefix (kisaragi-notes-completion//get-non-literature)))

;; ============================================================================
;;;; Orb note actions
;; ============================================================================
;; ============================================================================
;;;; Org-roam-bibtex minor mode
;; ============================================================================

(defun orb-notes-fn (citekey)
  "Open an Org-roam note associated with the CITEKEY or create a new one.
Set `org-ref-notes-function' to this function if your
bibliography notes are managed by Org-roam and you want some
extra integration between the two packages.

This is a wrapper function around `orb-edit-notes'
intended for use with Org-ref."
  (when (require 'org-ref nil t)
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (orb-edit-notes citekey))))

(defun orb-edit-notes-ad (keys)
  "Open an Org-roam note associated with the first key from KEYS.
This function replaces `bibtex-completion-edit-notes'.  Only the
first key from KEYS will actually be used."
  (orb-edit-notes (car keys)))

(defun orb-bibtex-completion-parse-bibliography-ad (&optional _ht-strings)
  "Update `orb-notes-cache' before `bibtex-completion-parse-bibliography'."
  (orb-make-notes-cache))

(provide 'org-roam-bibtex)

;;; org-roam-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
