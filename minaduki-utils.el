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

(require 'minaduki-vars)
(require 'ucs-normalize)

;; This is necessary to ensure all dependents on this module see
;; `org-mode-hook' and `org-inhibit-startup' as dynamic variables,
;; regardless of whether Org is loaded before their compilation.
(require 'org)
(require 'markdown-mode)
(require 'org-element)

(defvar markdown-mode-hook)

;;;; Error and progress reporting
(defun minaduki//message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `minaduki-verbose' is t."
  (when minaduki-verbose
    (apply #'message `(,(concat "(minaduki) " format-string) ,@args))))

(defun minaduki//warn (level message &rest args)
  "Display a warning for minaduki at LEVEL.

MESSAGE and ARGS are formatted by `format-message'.

This is a convenience wrapper around `lwarn'. Difference:

- TYPE is always '(minaduki).
- This always returns nil."
  (prog1 nil
    (apply #'lwarn '(minaduki) level message args)))

;; From `orb--with-message!'
(defmacro minaduki//with-message (message &rest body)
  "Put MESSAGE before and after BODY.

Echo \"MESSAGE...\", run BODY, then echo \"MESSAGE...done\"
afterwards. The value of BODY is returned."
  (declare (indent 1) (debug (stringp &rest form)))
  `(prog2
       (message "%s..." ,message)
       (progn ,@body)
     (message "%s...done" ,message)))

(defmacro minaduki//for (message var sequence &rest body)
  "Iterate BODY over SEQUENCE.

VAR is the variable bound for each element in SEQUENCE. This is
the X in (cl-loop for X in sequence).

MESSAGE is a format string which must have two slots: the first
is the 1-based index, the second is the total length of
SEQUENCE."
  (declare (indent 3))
  `(cl-loop for ,var being the elements of ,sequence
            using (index i)
            with length = (length ,sequence)
            do
            (progn
              (minaduki//message ,message (1+ i) length)
              ,@body)))

(defun minaduki--pixel-width (str)
  "Return pixel width of STR."
  (cl-letf ((shr-use-fonts t)
            ;; - evil assumes `set-window-buffer' is only used for
            ;;   windows that are to be displayed - -
            ;; - `shr-string-pixel-width' relies on
            ;;   `window-text-pixel-size' which requires a live,
            ;;   selected window, thus uses `set-window-buffer'
            ;;
            ;; So this function would waste a ton of time initializing
            ;; evil in a temporary buffer without this.
            (evil-mode nil))
    (shr-string-pixel-width str)))
(defun minaduki--truncate (len str)
  "Truncate STR to LEN number of characters."
  ;; `citar--fit-to-width'
  (setq len (floor len))
  (let ((truncated (truncate-string-to-width str len)))
    (if (<= (string-width str) len)
        str
      (concat truncated (propertize (substring str (length truncated))
                                    'invisible t)))))
(defun minaduki--ensure-pixel-width (len str)
  "Make sure STR is LEN wide."
  (let ((w (minaduki--pixel-width str))
        (one-char (minaduki--pixel-width "x")))
    (cond ((> w len)
           (->>
            ;; Not pixel-wise. That is too slow.
            (minaduki--truncate (floor (/ len one-char)) str)
            ;; But do pad it out again.
            (minaduki--ensure-pixel-width len)))
          ((< w len)
           ;; Add len - w pixels
           (concat str (propertize
                        (make-string (- len w) ?\s)
                        'face '(:height 1))))
          (t str))))
(defun minaduki--ensure-width (len str)
  "Ensure STR is LEN number of pixels wide."
  (setq len (floor len))
  (if (display-graphic-p)
      (minaduki--ensure-pixel-width len str)
    ;; `citar--fit-to-width'
    (let* ((one-char (minaduki--pixel-width "x"))
           (chars (floor (/ len one-char)))
           (truncated (truncate-string-to-width str chars))
           (display (truncate-string-to-width str chars 0 ?\s)))
      (if (<= (string-width str) chars)
          display
        (concat display (propertize (substring str (length truncated))
                                    'invisible t))))))

(defun minaduki//remove-curly (str)
  "Remove curly braces from STR."
  (when str
    (save-match-data
      (replace-regexp-in-string "[{}]" "" str))))

(defun minaduki//add-tag-string (str tags)
  "Add TAGS to STR.

Depending on the value of `org-roam-file-completion-tag-position', this function
prepends TAGS to STR, appends TAGS to STR or omits TAGS from STR."
  (pcase org-roam-file-completion-tag-position
    ('prepend (concat
               (when tags (propertize (format "(%s) " (s-join org-roam-tag-separator tags))
                                      'face 'minaduki-tag))
               str))
    ('append (concat
              str
              (when tags (propertize (format " (%s)" (s-join org-roam-tag-separator tags))
                                     'face 'minaduki-tag))))
    ('omit str)))

(defun minaduki//remove-org-links (str)
  "Remove Org bracket links from STR."
  (let ((links (s-match-strings-all org-link-bracket-re str)))
    (--> (cl-loop for link in links
                  collect
                  (let ((orig (elt link 0))
                        (desc (or (elt link 2)
                                  (elt link 1))))
                    (cons orig desc)))
         (s-replace-all it str))))

(defun minaduki//resolve-org-links (strings)
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
(defun minaduki//url? (path)
  "Check if PATH is a URL.

Works even if the protocol is not present in PATH, for example
when URL `https://google.com' is passed as `//google.com'."
  (or (s-prefix? "//" path)
      (s-prefix? "http://" path)
      (s-prefix? "https://" path)))

(defun minaduki//apply-link-abbrev (path)
  "Apply `org-link-abbrev-alist' to PATH.

For example, if `org-link-abbrev-alist' maps \"x\" to \"/home/\",
and PATH is \"/home/abc\", this returns \"x:abc\".

Inverse of `org-link-expand-abbrev'."
  (catch 'ret
    (setq path (f-canonical path))
    (pcase-dolist (`(,key . ,abbrev) org-link-abbrev-alist)
      ;; Get the symbol property if the value is a function / symbol
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

(cl-defun minaduki/format-link (&key target desc id?)
  "Format TARGET and DESC as a link according to the major mode.

`org-link-abbrev-alist' is applied when in Org mode, unless
TARGET is an HTTP link.

If ID? is non-nil and we're in Org mode, return an ID link instead."
  (let ((url? (minaduki//url? target))
        ;; - Allow `minaduki//apply-link-abbrev' to work even on file: links
        ;; - Allow f.el to work in general
        (target (replace-regexp-in-string "^file://" "" target)))
    (cond ((derived-mode-p 'org-mode)
           ;; Don't apply link-abbrev if TARGET is https or http
           (unless (or url? id?)
             (setq target (minaduki//apply-link-abbrev target)))
           (when id?
             (setq target (concat "id:" target)))
           (if (and (not desc) url?) ; plain url
               target
             (org-link-make-string target desc)))

          ((derived-mode-p 'markdown-mode)
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
          (t target))))

(defun org-roam-format-link (target &optional description type)
  "Format a link for TARGET and DESCRIPTION.

TYPE defaults to \"file\".

In Markdown, TYPE has no effect."
  (setq type (or type "file"))
  (cond
   ((derived-mode-p 'org-mode)
    (org-link-make-string
     (if (string-equal type "file")
         (minaduki//apply-link-abbrev target)
       (concat type ":" target))
     description))
   ((derived-mode-p 'markdown-mode)
    (cond ((and (not description) target)
           (format "<%s>" target))
          ((not description)
           (format "[%s](%s)"
                   (f-filename target)
                   (f-relative target)))
          (t
           (format "[%s](%s)" description target))))))

(defun minaduki//path-to-title (path)
  "Convert PATH to a string that's suitable as a title."
  (-> path
      (f-relative (f-expand org-directory))
      f-no-ext))

(defun minaduki//title-to-slug (title)
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
(defun minaduki//date/ymd->calendar.el (yyyy-mm-dd)
  "Convert date string YYYY-MM-DD to calendar.el list (MM DD YYYY)."
  (pcase-let ((`(,year ,month ,day) (cdr (s-match (rx (group (= 4 digit)) "-"
                                                      (group (= 2 digit)) "-"
                                                      (group (= 2 digit)))
                                                  yyyy-mm-dd))))
    (list
     (string-to-number month)
     (string-to-number day)
     (string-to-number year))))

(defun minaduki//date/calendar.el->ymd (calendar-el-date)
  "Convert CALENDAR-EL-DATE (a list (MM DD YYYY)) to a date string YYYY-MM-DD."
  (apply #'format "%3$04d-%1$02d-%2$02d" calendar-el-date))

(defun minaduki//today (&optional n)
  "Return today's date, taking `org-extend-today-until' into account.

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
     (or (and (boundp 'org-extend-today-until)
              (numberp org-extend-today-until)
              (* 3600 org-extend-today-until))
         ;; otherwise just return (now - 0) = now.
         0)))))

;;;; File utilities
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

(defun minaduki//file-name-extension (path)
  "Return the extension of PATH.

Like `file-name-extension', but:

- this does not strip version number, and
- this strips the .gpg extension."
  (let (file ext)
    (save-match-data
      (setq file (file-name-nondirectory path))
      (when (and (string-match "\\.[^.]*\\'" file)
                 (not (eq 0 (match-beginning 0))))
        (setq ext (substring file (1+ (match-beginning 0))))))
    ;; This will do it more than once. Is this a problem?
    (if (equal ext "gpg")
        (minaduki//file-name-extension (f-no-ext path))
      ext)))

;;;; File functions
(defun minaduki//find-file (file)
  "Open FILE using `org-roam-find-file-function' or `find-file'."
  (funcall (or org-roam-find-file-function #'find-file) file))

(defun minaduki//compute-content-hash (file)
  "Compute the hash of the raw bytes of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

;;;; Macros
(defmacro minaduki//with-file (file keep-buf-p &rest body)
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

(defmacro minaduki//with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.

Like `with-temp-buffer', but propagates `org-directory'.

If FILE, set `minaduki//file-name' and variable
`buffer-file-name' to FILE and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-directory (make-symbol "current-org-directory")))
    `(let ((,current-org-directory org-directory))
       (with-temp-buffer
         (let ((org-directory ,current-org-directory)
               (org-mode-hook nil)
               (markdown-mode-hook nil)
               (org-inhibit-startup t)
               (after-change-major-mode-hook '(minaduki-initialize))
               ,@(when file
                   `((minaduki//file-name ,file)
                     (default-directory (file-name-directory ,file))
                     (buffer-file-name ,file))))
           (funcall (or (-some-> ,file
                          (assoc-default auto-mode-alist #'string-match))
                        #'org-mode))
           ,@(when file
               `((insert-file-contents ,file)))
           ,@body)))))

(defmacro minaduki//lambda-self (args &rest body)
  "Like `lambda', except a symbol `self' is bound to the function itself.

ARGS and BODY are as in `lambda'."
  (declare (indent defun)
           (doc-string 2))
  `(let (self)
     (setq self (lambda ,args ,@body))
     self))

;;;; Org mode local functions
;; `org-roam--extract-global-props'
(defun minaduki//org-props (props)
  "Extract PROPS from the current Org buffer.
Props are extracted from both the file-level property drawer (if
any), and Org keywords. Org keywords take precedence."
  (let (ret)
    ;; Org: keyword properties
    (pcase-dolist (`(,key . ,values) (org-collect-keywords props))
      (dolist (value values)
        (push (cons key value) ret)))
    ;; Org: file-level property drawer properties
    (org-with-point-at 1
      (dolist (prop props)
        (when-let ((v (org-entry-get (point) prop)))
          (push (cons prop v) ret))))
    ret))

(defsubst minaduki//org-prop (prop)
  "Return values of PROP as a list.

Given an Org file

  #+title: abc
  #+prop1: abcdef
  #+prop1: ghi

\(minaduki//org-prop \"title\") -> '(\"abc\")
\(minaduki//org-prop \"prop1\") -> '(\"abcdef\" \"ghi\")"
  (nreverse (mapcar #'cdr (minaduki//org-props (list prop)))))

;; Alternative to `org-get-outline-path' that doesn't break
(defun org-roam--get-outline-path ()
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as a
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
                finally return headings)))))

(defun org-roam--set-global-prop (name value)
  "Set a file property called NAME to VALUE.

If the property is already set, it's value is replaced."
  (org-with-point-at 1
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

(defun minaduki//collate-types (type)
  "Collate TYPE into a parent type."
  (cond ((member type '("http" "https"))
         "website")
        (t type)))

(provide 'minaduki-utils)

;;; minaduki-utils.el ends here
