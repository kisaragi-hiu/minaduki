;;; minaduki-utils.el --- Low level utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous macros and utility functions. These cannot depend on
;; the cache.
;;
;; TODO: minaduki-mode-case

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 's)
(require 'faceup)
(require 'map)

(require 'minaduki-vars)
(require 'ucs-normalize)

;; This is necessary to ensure all dependents on this module see
;; `org-mode-hook' and `org-inhibit-startup' as dynamic variables,
;; regardless of whether Org is loaded before their compilation.
(require 'org)
(require 'markdown-mode)
(require 'org-element)

(defvar markdown-mode-hook)

(defun minaduki::object-to-vector (obj)
  "Turn OBJ into a vector to make it easier to insert into the database."
  (let* ((len (1- (length obj)))
         (v (make-vector len nil)))
    (dotimes (i len)
      (aset v i (aref obj (1+ i))))
    v))
(defun minaduki::vector-to-object (vec type)
  "Turn VEC into an object of TYPE.

This does zero type checks, and it is up to the user to make sure
#s(TYPE (elt VEC 0) (elt VEC 1) ...) is valid for TYPE."
  (apply #'record type (append vec nil)))

(defun minaduki::current-file-name (&optional fallback buffer)
  "Return current file name of BUFFER in a consistent way.

BUFFER defaults to the current buffer.

FALLBACK should be a one element list containing the file name
to use instead of `minaduki::file-name'. This allows easily
distinguishing between a caller that does not want to use
`minaduki::file-name' and a caller that does."
  (or (if fallback
          (car fallback)
        minaduki::file-name)
      (buffer-file-name (buffer-base-buffer buffer))))

(defun minaduki::file-type::path (path)
  "Determine the file type from PATH only*.

*: and `minaduki-file-extension-type-alist'."
  (declare
   ;; only reads input from arguments
   (pure nil)
   ;; only sends output through return value
   (side-effect-free t))
  (let* ((ext (minaduki::file-name-extension path))
         (pair (assoc ext minaduki-file-extension-type-alist)))
    (cond (pair (cdr pair))
          ;; (ext  (intern ext))
          (t    nil))))
(defun minaduki::file-type ()
  "Return the file type of current buffer."
  (let ((case-fold-search t))
    (cond ((derived-mode-p 'org-mode) 'org)
          ((derived-mode-p 'markdown-mode) 'markdown)
          ((derived-mode-p 'bibtex-mode) 'bibtex)
          ((derived-mode-p 'json-mode) 'json)
          ((buffer-file-name)
           (minaduki::file-type::path
            (buffer-file-name))))))

(defmacro minaduki::file-type-case (&rest clauses)
  "Run bodies in CLAUSES conditionally depending on `minaduki::file-type'.
Each clause looks like (FILE-TYPE BODY...).

This is roughly modeled after `cl-typecase', though each clause
is more like a `cl-case' clause, except with the ability to match
strings."
  (declare (indent 0))
  (cl-with-gensyms (type-sym)
    (let ((conditions))
      (--each clauses
        (push `(,(minaduki::keyword-to-symbol (car it))
                ,@(cdr it))
              conditions))
      (setq conditions (nreverse conditions))
      `(let ((,type-sym (minaduki::file-type)))
         ;; HACK to get `cl-case' to match strings
         ;; probably not necessary since the input only comes from ::file-type,
         ;; which returns symbols and maybe nil.
         ;; (cl-letf (((symbol-function #'eql)
         ;;            (symbol-function #'equal)))
         ;; Special case: return the value if there is no match specified
         ,(cond ((not conditions) type-sym)
                (t `(cl-case ,type-sym ,@conditions)))))))

(defmacro minaduki::with-comp-setup (defaults &rest body)
  "Run BODY with completion frameworks set up according to DEFAULTS.

Example:

  (minaduki::with-comp-setup
      ((ivy-sort-functions-alist . nil)
       (ivy-sort-matches-functions-alist . #\\='ivy--shorter-matches-first))
    BODY)

This will run BODY with Ivy set to not sort the initial
collection, and to sort matches with `ivy--shorter-matches-first'."
  (declare (indent 1))
  `(progn
     ;; The keys are never supposed to be lexical variables.
     ,@(cl-loop for (key . _) in defaults
                collect `(defvar ,key nil))
     (defvar selectrum-should-sort nil)
     (let ((selectrum-should-sort nil)
           ,@(cl-loop for (key . value) in defaults
                      collect `(,key
                                (when (boundp ',key)
                                  (if (assoc this-command ,key)
                                      ,key
                                    (list (cons this-command
                                                ,(alist-get key defaults))))))))
       ,@body)))

;;;; Error and progress reporting
(defun minaduki::message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `minaduki-verbose' is t."
  (when minaduki-verbose
    (apply #'message `(,(concat "(minaduki) " format-string) ,@args))))

(defun minaduki::keyword-to-symbol (kw)
  "Given keyword :KW, return \\='KW.

Return KW unchanged if it\\='s not a keyword."
  (if (not (keywordp kw))
      kw
    (intern (substring (symbol-name kw) 1))))

(defun minaduki::warn (level msg &rest args)
  "Display a warning for minaduki at LEVEL.

MSG and ARGS are formatted by `format-message'.

Difference between this and `lwarn':

- TYPE is always \"(minaduki)\"
- This always returns nil
- `faceup' syntax can be used in MSG and ARGS
- Maps in ARGS are formatted as maps in the message"
  (declare (indent 1))
  (prog1 nil
    (apply #'lwarn '(minaduki) level
           (faceup-render-string msg)
           (--map
            (faceup-render-string
             (cond
              ;; A string is also `mapp', because it's an array
              ((stringp it) it)
              ((mapp it)
               (->> it
                    (map-apply
                     (lambda (k v)
                       (format "\t«k:%s»: %S"
                               (minaduki::keyword-to-symbol k)
                               v)))
                    (s-join "\n")))
              (t it)))
            args))))

(defmacro minaduki::loading (str &rest body)
  "Show STR, run BODY, then show STR + \"done\"."
  (declare (indent 1))
  `(progn
     (minaduki::message "%s" ,str)
     ,@body
     (minaduki::message "%sdone" ,str)))

(defmacro minaduki::for (message var sequence &rest body)
  "Iterate BODY over SEQUENCE.

VAR is the variable bound for each element in SEQUENCE. This is
the X in (cl-loop for X in sequence).

MESSAGE is a format string which must have two slots: the first
is the 1-based index, the second is the total length of
SEQUENCE.

It can also be an unquoted list (MSG arg1 arg2 ...), where the
extra arguments are passed to the message."
  (declare (indent 3))
  `(cl-loop for ,var being the elements of ,sequence
            using (index i)
            with length = (length ,sequence)
            do
            (progn
              (minaduki::message
               ,(if (stringp message)
                    message
                  (car message))
               (1+ i)
               length
               ,@(if (stringp message)
                     nil
                   (cdr message)))
              (let ((inhibit-message t))
                ,@body))))

(defun minaduki::truncate (len str)
  "Truncate STR to LEN number of characters."
  ;; `citar--fit-to-width'
  (setq len (floor len))
  (let ((truncated (truncate-string-to-width str len)))
    (if (<= (string-width str) len)
        str
      (concat truncated (propertize (substring str (length truncated))
                                    'invisible t)))))

(defun minaduki::ensure-display-width (pixels str)
  "Ensure STR displays as PIXELS wide."
  (concat
   (minaduki::truncate
    (floor (/ pixels (frame-char-width)))
    str)
   (propertize
    " "
    'display `(space :align-to (,pixels)))))

(defun minaduki::remove-curly (str)
  "Remove curly braces from STR."
  (when str
    (save-match-data
      (replace-regexp-in-string "[{}]" "" str))))

(defun minaduki::remove-org-links (str)
  "Remove Org bracket links from STR."
  (let ((links (s-match-strings-all org-link-bracket-re str)))
    (--> (cl-loop for link in links
                  collect
                  (let ((orig (elt link 0))
                        (desc (or (elt link 2)
                                  (elt link 1))))
                    (cons orig desc)))
         (s-replace-all it str))))

(defun minaduki::resolve-org-links (strings)
  "Resolve STRINGS as a list of Org links.

Each string in STRINGS might be something like \"[[file:/abc]]\";
we run `org-element-link-parser' on it and return the
`:raw-link'."
  ;; Optimization: use the same buffer for this. `erase-buffer' is
  ;; faster than destroying the buffer and creating another one.
  ;;
  ;; Try:
  ;;
  ;; (list
  ;;  (benchmark-run-compiled 1000
  ;;    (dotimes (i 100)
  ;;      (with-temp-buffer
  ;;        (insert "a"))))
  ;;  (benchmark-run-compiled 1000
  ;;    (with-temp-buffer
  ;;      (dotimes (i 100)
  ;;        (erase-buffer)
  ;;        (insert "a")))))
  ;;
  ;; -> ((7.5397476370000005 1 0.9251548320000005)
  ;;     (0.11945125100000001 0 0.0))
  (with-temp-buffer
    (cl-loop for str in strings
             collect
             (progn
               (erase-buffer)
               (insert str)
               (goto-char (point-min))
               (let ((link (org-element-link-parser)))
                 ;; If we resolve something...
                 (if link
                     ;; Return the resolved result
                     (org-element-property :raw-link link)
                   ;; Otherwise return the original
                   str))))))

;;;; URL
(defun minaduki::url? (path)
  "Check if PATH is a URL.

Works even if the protocol is not present in PATH, for example
when URL `https://google.com' is passed as `//google.com'."
  (or (s-prefix? "//" path)
      (s-prefix? "http://" path)
      (s-prefix? "https://" path)))

(defun minaduki::apply-link-abbrev (path)
  "Apply `org-link-abbrev-alist' to PATH.

For example, if `org-link-abbrev-alist' maps \"x\" to \"/home/\",
and PATH is \"/home/abc\", this returns \"x:abc\".

Inverse of `org-link-expand-abbrev'."
  ;; FIXME: this should be applied to `minaduki/vaults'.
  (catch 'ret
    (unless path
      (throw 'ret path))
    (setq path (f-canonical path))
    (pcase-dolist (`(,key . ,abbrev) org-link-abbrev-alist)
      ;; Get the symbol property if the value is a function / symbol
      ;; FIXME: this is specific to `kisaragi-file-finders'. There
      ;; should be support for function values in `minaduki/vaults'.
      (when (symbolp abbrev)
        (setq abbrev (get abbrev 'k/file-finders-abbrev-path)))
      ;; Only do something when we actually have a string
      (when (stringp abbrev)
        ;; Resolving symlinks here allows us to treat different ways
        ;; to reach a path as the same
        (setq abbrev (f-canonical abbrev))
        ;; starts-with is more accurate
        (when (s-starts-with? abbrev path)
          (throw 'ret (s-replace abbrev (concat key ":") path)))))
    (throw 'ret path)))

(cl-defun minaduki::format-plain-link (&key target desc)
  "Format TARGET and DESC as a link according to the major mode.

Like `minaduki::format-link' but without the path magic."
  (minaduki::file-type-case
    (:org
     (org-link-make-string target desc))
    (:markdown
     (cond ((not desc)
            ;; Just a URL
            (format "[%s](%s)"
                    target
                    target))
           (t
            (format "[%s](%s)" desc target))))
    (_ target)))

(cl-defun minaduki::format-link (&key target desc type)
  "Format TARGET and DESC as a link according to the major mode.

`org-link-abbrev-alist' is applied when in Org mode, unless
TARGET is an HTTP link.

TYPE can be `id' or nil. When it is nil, automatically
determine if we need a file link or a URL link.

If ID? is non-nil and we're in Org mode, return an ID link instead."
  (let ((url? (minaduki::url? target))
        ;; Occasionally we get a file URL. Turn it back into a proper
        ;; path.
        (target (org-link-decode
                 (replace-regexp-in-string "^file://" "" target))))
    (minaduki::file-type-case
      (:org
       (unless (or url? type)
         (setq target (minaduki::convert-path-format target)))
       (when (eq type 'id)
         (setq target (concat "id:" target)))
       (if (and (not desc) url?) ; plain url
           target
         ;; Avoid inserting [[a][a]]
         (when (equal target desc)
           (setq desc nil))
         (org-link-make-string target desc)))
      (:markdown
       (cond ((and (not desc) url?)
              ;; plain URL links
              (format "<%s>" target))
             ((not desc)
              ;; Just a URL
              (format "[%s](%s)"
                      (f-filename target)
                      (f-relative target)))
             (t
              (format "[%s](%s)" desc target))))
      ;; No common way to insert descriptions
      (_ target))))

(defun minaduki::to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (let ((slug
         (--> title
              ;; Normalize combining characters (use single character ä
              ;; instead of combining a + #x308 (combining diaeresis))
              ucs-normalize-NFC-string
              ;; Do the replacement. Note that `s-replace-all' does not
              ;; use regexp.
              (--reduce-from
               (replace-regexp-in-string (car it) (cdr it) acc) it
               minaduki/slug-replacements))))
    (downcase slug)))

;;;; Dates
(defun minaduki-date::ymd->calendar.el (yyyy-mm-dd)
  "Convert date string YYYY-MM-DD to calendar.el list (MM DD YYYY)."
  (pcase-let ((`(,year ,month ,day) (cdr (s-match (rx (group (= 4 digit)) "-"
                                                      (group (= 2 digit)) "-"
                                                      (group (= 2 digit)))
                                                  yyyy-mm-dd))))
    (list
     (string-to-number month)
     (string-to-number day)
     (string-to-number year))))

(defun minaduki-date::calendar.el->ymd (calendar-el-date)
  "Convert CALENDAR-EL-DATE (a list (MM DD YYYY)) to a date string YYYY-MM-DD."
  (apply #'format "%3$04d-%1$02d-%2$02d" calendar-el-date))

(defun minaduki::today (&optional n ignore-extend)
  "Return today's date, taking `org-extend-today-until' into account.

If IGNORE-EXTEND is non-nil, then *don\\='t* take
`org-extend-today-until' into account.

Return values look like \"2020-01-23\".

If N is non-nil, return N days from today. For example, N = 1
means tomorrow, and N = -1 means yesterday."
  (unless n (setq n 0))
  (format-time-string
   "%Y-%m-%d"
   (time-add
    (* n 86400)
    (time-since
     ;; if it's bound and it's a number, do the same thing `org-today' does
     (if (and (not ignore-extend)
              (boundp 'org-extend-today-until)
              (numberp org-extend-today-until))
         (* 3600 org-extend-today-until)
       ;; otherwise just return (now - 0) = now.
       0)))))

;;;; File utilities
(defun minaduki::ensure-not-file:// (path)
  "If PATH is a file:// URL, convert it back to a normal path."
  (if (s-prefix? "file://" path)
      (org-link-decode
       (replace-regexp-in-string "^file://" "" path))
    path))

(defun minaduki::convert-path-format (path)
  "Convert PATH to the right format according to `minaduki:link-insertion-format'."
  (setq path (minaduki::ensure-not-file:// path))
  (pcase minaduki:link-insertion-format
    ('absolute
     (abbreviate-file-name (expand-file-name path)))
    ('absolute-in-vault
     (minaduki::apply-link-abbrev (expand-file-name path)))
    ('relative
     ;; We can use "file:..." or "./..." here; the former is
     ;; consistent with `org-insert-link'.
     (concat "file:" (file-relative-name path)))))

(defun minaduki::file-name-extension (path)
  "Return the extension of PATH.

Like `file-name-extension', but:

- this does not strip version number
- this strips the .gpg and .gz extensions"
  (let (file ext)
    (save-match-data
      (setq file (file-name-nondirectory path))
      (when (and (string-match "\\.[^.]*\\'" file)
                 (not (eq 0 (match-beginning 0))))
        (setq ext (substring file (1+ (match-beginning 0))))))
    ;; This can repeat more than once. Is this a problem?
    (cond ((member ext '("gpg" "gz"))
           (minaduki::file-name-extension (f-no-ext path)))
          (t ext))))

;;;; File functions
(defun minaduki::find-file (file &optional other?)
  "Open FILE in an appropriate way.

If OTHER? is non-nil, open FILE in another window, otherwise open
it in the current window."
  ;; `other-window' is from Emacs 17. It's fine.
  (when other? (other-window 1))
  (find-file file))

(defun minaduki::compute-content-hash (file)
  "Compute the hash of the raw bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

;;;; Macros
(defmacro minaduki::with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

(defmacro minaduki::with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.

Like `with-temp-buffer', but sets some things up.

If FILE, set `minaduki::file-name' and variable
`buffer-file-name' to FILE and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-directory (make-symbol "current-org-directory")))
    `(let ((,current-org-directory org-directory))
       (with-temp-buffer
         (cl-letf ((org-directory ,current-org-directory)
                   (org-inhibit-startup t)
                   (after-change-major-mode-hook '(minaduki-initialize))
                   ,@(when file
                       `((minaduki::file-name ,file)
                         (default-directory (file-name-directory ,file))
                         (buffer-file-name ,file)))
                   ((symbol-function 'run-mode-hooks)
                    (symbol-function #'ignore))
                   ((symbol-function 'org-install-agenda-files-menu)
                    (symbol-function #'ignore)))
           (setq-local org-mode-hook nil)
           (setq-local markdown-mode-hook nil)
           ,@(when file
               `((insert-file-contents ,file)))
           (set-auto-mode)
           ,@body)))))

(defmacro minaduki::lambda-self (args &rest body)
  "Like `lambda', except a symbol `self' is bound to the function itself.

ARGS and BODY are as in `lambda'."
  (declare (indent defun)
           (doc-string 2))
  `(let (self)
     (setq self (lambda ,args ,@body))
     self))

(cl-defmacro minaduki::with-front-matter (&rest body)
  "Run BODY with the buffer narrowed to the front matter, if any."
  (declare (indent 0))
  (cl-with-gensyms (start end)
    `(save-excursion
       (goto-char (point-min))
       (let (,start ,end)
         ;; The beginning of the frontmatter, which has to be at the beginning
         ;; of the buffer (before char position 4).
         (setq ,start (re-search-forward "^---$" 4 t))
         ;; The end of the frontmatter
         (setq ,end (re-search-forward "^---$" nil t))
         (when (and ,start ,end)
           (save-restriction
             (narrow-to-region
              ,start
              ;; `end' is after the "---", so the region between `start' and
              ;; `end' right now includes the ending marker. Subtracting like
              ;; this excludes it.
              (- ,end (length "---")))
             ,@body))))))

(defun minaduki::file-content (file)
  "Return the decoded content of FILE."
  (minaduki::with-temp-buffer file
    (buffer-string)))

;;;; Org mode stuff

(defun minaduki--org-has-children? ()
  "Does the current Org heading have child entries?

Cursor should be placed on the heading line."
  (let ((this-level (or (org-current-level) 0))
        (next-level (save-excursion
                      (or (and (goto-char (line-end-position))
                               (re-search-forward org-heading-regexp nil t)
                               (org-current-level))
                          0))))
    (< this-level next-level)))

(defun minaduki::org-entry-properties-original (&optional epom)
  "Reference implementation of `minaduki::org-entry-properties'.

Written here because my Emacs 30 pretest crashed while I was trying to
put this into the test file and I just want the commit done."
  (let ((props (org-entry-properties)))
    (--remove
     (member (car it)
             (-difference org-special-properties '("ITEM" "TODO")))
     props)))

(defun minaduki::org-entry-properties (&optional epom)
  "Return relevant properties of the org entry at EPOM efficiently.

If EPOM is nil, return properties for the entry at point.

The relevant properties are non-special properties plus ITEM and TODO."
  (org-with-point-at epom
    (when (and (derived-mode-p 'org-mode)
               (org-back-to-heading-or-point-min t))
      (catch 'exit
        (let* ((beg (point))
               props)
          ;; Special property: ITEM
          (let ((case-fold-search nil))
            (when (looking-at org-complex-heading-regexp)
              (push (cons "ITEM"
                          (let ((title (match-string-no-properties 4)))
                            (if (org-string-nw-p title)
                                (org-remove-tabs title)
                              "")))
                    props)))
          ;; Special property: TODO
          (let ((case-fold-search nil))
            (when (and (looking-at org-todo-line-regexp) (match-end 2))
              (push (cons "TODO" (match-string-no-properties 2)) props)))
          ;; Get the standard properties, like :PROP:.
          (let ((range (org-get-property-block beg)))
            (when range
              (let ((end (cdr range)) seen-base)
                (goto-char (car range))
                ;; Unlike to `org--update-property-plist', we
                ;; handle the case where base values is found
                ;; after its extension.  We also forbid standard
                ;; properties to be named as special properties.
                (while (re-search-forward org-property-re end t)
                  (let* ((key (upcase (match-string-no-properties 2)))
                         (extendp (string-match-p "\\+\\'" key))
                         (key-base (if extendp (substring key 0 -1) key))
                         (value (match-string-no-properties 3)))
                    (cond
                     ((member-ignore-case key-base org-special-properties))
                     (extendp
                      (setq props
                            (org--update-property-plist key value props)))
                     ((member key seen-base))
                     (t (push key seen-base)
                        (let ((p (assoc-string key props t)))
                          (if p (setcdr p (concat value " " (cdr p)))
                            (push (cons key value) props))))))))))
          (unless (assoc "CATEGORY" props)
            (push (cons "CATEGORY" (org-get-category beg)) props))
          ;; Return value.
          props)))))

;;;; Markdown local functions

(defun minaduki::markdown-matched-heading
    (&optional skip-match skip-outline-level)
  "Return (ID TEXT LEVEL) for the current heading.

If SKIP-MATCH is non-nil, assume the caller has already matched
on `markdown-regex-header'.
If SKIP-OUTLINE-LEVEL is non-nil, don't calculate outline level."
  (when (or skip-match
            (save-excursion
              (and (outline-back-to-heading)
                   (looking-at markdown-regex-header))))
    (-let* ((whole (or (match-string-no-properties 1)
                       (match-string-no-properties 5)))
            ((_ text id) (s-match "\\(.*\\) {#\\(.*?\\)}" whole)))
      (list id text (and (not skip-outline-level)
                         (markdown-outline-level))))))

;;;; Shared local functions

;; Alternative to `org-get-outline-path' that doesn't break
(defun minaduki::get-outline-path ()
  "Return the outline path to the current entry.

An outline path is a vector of ancestors for current headline, as a
list of strings. Statistics cookies are removed and links are
kept.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline.

Assume buffer is widened and point is on a headline."
  (org-with-wide-buffer
   (save-match-data
     (when (and (or (condition-case nil
                        (org-back-to-heading t)
                      (error nil))
                    (org-up-heading-safe))
                org-complex-heading-regexp)
       (cl-loop with headings
                do (push (let ((case-fold-search nil))
                           (looking-at org-complex-heading-regexp)
                           (if (not (match-end 4)) ""
                             ;; Remove statistics cookies.
                             (org-trim
                              (replace-regexp-in-string
                               "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                               (match-string-no-properties 4)))))
                         headings)
                while (org-up-heading-safe)
                finally return (vconcat headings))))))

(defun minaduki::set-global-prop (name value)
  "Set a file property called NAME to VALUE.

If the property is already set, it's value is replaced."
  (org-with-wide-buffer
   (goto-char 1)
   (let ((case-fold-search t))
     (if (re-search-forward (format "^#\\+%s:\\(.*\\)"
                                    (regexp-quote name))
                            (point-max) t)
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

(defun minaduki::get-heading-id ()
  "Return the ID of the current heading."
  (minaduki::file-type-case
    (:markdown
     (car (minaduki::markdown-matched-heading nil t)))
    (:org (org-id-get))))

(defun minaduki::set-heading-id (new-id &optional force)
  "Set the ID of the current heading to NEW-ID if it\\='s not set yet.
If FORCE is non-nil, overwrite the current value, if any.
Returns NEW-ID."
  (cl-block nil
    (unless force
      (when (minaduki::get-heading-id)
        (cl-return nil)))
    (minaduki::file-type-case
      (:markdown
       (save-excursion
         (outline-back-to-heading)
         (end-of-line)
         (insert (format " {#%s}" new-id))))
      (:org
       (org-entry-put nil "ID" new-id)))
    new-id))

(defun minaduki::collate-types (type)
  "Collate TYPE into a parent type."
  (cond ((member type '("http" "https"))
         "website")
        (t type)))

(defun minaduki::set-buffer-substring (start end &rest strings)
  "Set buffer content between START and END to STRINGS."
  (declare (indent 2))
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (apply #'insert strings)))

(provide 'minaduki-utils)

;;; minaduki-utils.el ends here
