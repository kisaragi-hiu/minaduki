;;; minaduki-bibtex.el --- BibTeX integration -*- lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;; Copyright © 2020 Mykhailo Shevchuk <mail@mshevchuk.com>
;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; 	Mykhailo Shevchuk <mail@mshevchuk.com>
;; 	Jethro Kuan <jethrokuan95@gmail.com>

;;; Commentary:

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'minaduki-utils)
(require 'minaduki-vars)

(require 'org-roam-capture)

(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;;;; Utils
;; ============================================================================

;;;###autoload
(defun orb-process-file-field (citekey)
  "Return a source of CITEKEY.

Sources are resources like PDFs, URLs, etc. that are associated
with a literature entry.

If CITEKEY has multiple sources, prompt to select one of them.

If variable `orb-file-field-extensions' is non-nil, return only
the file paths with the respective extensions."
  (ignore-errors
    (when-let* ((entry (minaduki-db//fetch-lit-entry citekey))
                (paths (minaduki//resolve-org-links
                        (gethash "sources" entry))))
      (when-let ((extensions orb-file-field-extensions))
        (unless (listp extensions)
          (setq extensions (list extensions)))
        (setq paths (--filter
                     (lambda ()
                       (when-let ((extension (file-name-extension it)))
                         (member-ignore-case extension extensions)))
                     paths)))
      (when paths
        (if (= (length paths) 1)
            (car paths)
          (completing-read "File to use: "
                           (minaduki-completion//mark-category
                            paths 'file)))))))

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

;; TODO: ENTRY should be in `minaduki-lit''s entry format.
(defun orb--preformat-template (template entry)
  "Helper function for `orb--preformat-templates'.
TEMPLATE is an element of `minaduki-capture/templates' and ENTRY
is a BibTeX entry as returned by `bibtex-completion-get-entry'."
  ;; Handle minaduki-capture part
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
         (rx "\\(%\\^{[[:alnum:]_-]*}\\)")
         (file-keyword (when orb-process-file-keyword
                         (or (and (stringp orb-process-file-keyword)
                                  orb-process-file-keyword)
                             "file")))
         lst)
    ;; First run:
    ;; 1) Make a list of (rplc-s field-value match-position) for the
    ;; second run
    ;; 2) replace minaduki-capture wildcards
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
                           (cdr (assoc "=key=" entry)))
                        ;; we're done so don't even compare file-name with
                        ;; file-keyword in the successive cycles
                        (setq file-keyword nil))
                    (cdr (assoc field-name entry)))
                  ""))
             ;; org-capture prompt wildcard
             (rplc-s (concat "%^{" (or keyword "citekey") "}"))
             ;; minaduki-capture prompt wildcard
             (rplc-s2 (concat "${" (or keyword "citekey") "}"))
             ;; minaduki-capture :head template
             (head (plist-get plst :head))
             ;; minaduki-capture :file-name template
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
        ;; Replace minaduki-capture prompt wildcards
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
  "Process templates and run `minaduki-capture//capture'.
CITEKEY is a citation key.
Helper function for `orb-edit-notes', which abstracts initiating
a capture session."
  ;; Check if the requested BibTeX entry actually exists and fail
  ;; gracefully otherwise
  (let ((entry (or (caar (minaduki-db/query [:select [props] :from keys
                                             :where (= key $s1)]
                                            citekey))
                   (minaduki//warn
                    :warning
                    "%s: Could not find the literature entry" citekey))))
    (puthash "=key=" (gethash "key" entry) entry)
    (remhash "key" entry)
    (puthash "=type=" (gethash "type" entry) entry)
    (remhash "type" entry)
    (setq entry (map-into entry 'alist))
    (if-let* (;; Depending on the templates used: run
              ;; `minaduki-capture//capture' or call `org-roam-find-file'
              (org-capture-templates
               (or orb-templates minaduki-capture/templates
                   (minaduki//warn
                    :warning
                    "Could not find the requested templates")))
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
              ;; `minaduki-capture//capture' behaves specially in this case
              ;; NOTE: this circumvents using functions other than
              ;; `org-capture', see `minaduki-capture/function'.
              ;; If the users start complaining, we may revert previous
              ;; implementation
              (minaduki-capture/templates (list template))
              ;; Org-roam coverts the templates to its own syntax;
              ;; since we are telling `org-capture' to use the template entry
              ;; (by setting `org-capture-entry'), and Org-roam converts the
              ;; whole template list, we must do the conversion of the entry
              ;; ourselves
              (org-capture-entry
               (minaduki-capture//convert-template template))
              (citekey-formatted (format (or orb-citekey-format "%s") citekey))
              (title
               (or (cdr (assoc "title" entry))
                   (minaduki//warn
                    :warning
                    "Title not found for this entry")
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
                ;; `minaduki-capture//capture' with ORB-predefined
                ;; settings or call vanilla `org-roam-find-file'
                (if orb-templates
                    (let* ((minaduki-capture//context 'ref)
                           (slug-source (cl-case orb-slug-source
                                          (citekey citekey)
                                          (title title)
                                          (t (user-error "Only `citekey' \
or `title' should be used for slug: %s not supported" orb-slug-source))))
                           (minaduki-capture//info
                            `((title . ,title)
                              (ref . ,citekey-formatted)
                              ,@(when-let (url (cdr (assoc  "url" entry)))
                                  `((url . ,url)))
                              (slug . ,(minaduki//title-to-slug slug-source)))))
                      (setq minaduki-capture/additional-template-props
                            (list :finalize 'find-file))
                      (minaduki-capture//capture))
                  (minaduki/open title)))
            (orb--store-link-functions-advice 'remove)))
      (message "ORB: Something went wrong. Check the *Warnings* buffer"))))

;;;###autoload
(defun orb-edit-notes (citekey)
  "Open a note associated with the CITEKEY or create a new one.

CITEKEY's information is extracted from files listed in
`minaduki-lit/bibliography' during Minaduki's cache build
process."
  (let* ((file (minaduki-db//fetch-file :key citekey))
         (note-data (when file
                      (list (minaduki-db//fetch-title file)
                            file))))
    ;; Find org-roam reference with the CITEKEY and collect data into
    ;; `orb-plist'
    (orb-plist-put :note-existed (and note-data t))
    (cond
     (note-data
      (orb-plist-put :title (elt note-data 0)
                     :file (elt note-data 1))
      (apply #'orb-plist-put (cdr note-data))
      (ignore-errors (minaduki//find-file (orb-plist-get :file))))
     ;; we need to clean up if the capture process was aborted signaling
     ;; user-error
     (t (condition-case nil
            (orb--edit-notes citekey)
          (error
           (with-orb-cleanup (orb-do-hook-functions 'remove))))))))

(provide 'minaduki-bibtex)

;;; minaduki-bibtex.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
