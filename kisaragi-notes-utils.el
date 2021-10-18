;;; kisaragi-notes-utils.el --- Utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Miscellaneous macros and utility functions.
;;
;;; Code:

(require 'dash)
(require 's)

(require 'kisaragi-notes-vars)

;; This is necessary to ensure all dependents on this module see
;; `org-mode-hook' and `org-inhibit-startup' as dynamic variables,
;; regardless of whether Org is loaded before their compilation.
(require 'org)

(defun kisaragi-notes//compute-content-hash (&optional file)
  "Compute the hash of the contents of FILE or the current buffer."
  (if file
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (secure-hash 'sha1 (current-buffer)))
    (org-with-wide-buffer
     (secure-hash 'sha1 (current-buffer)))))

(defmacro kisaragi-notes//for (message var sequence &rest body)
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
              (org-roam-message ,message (1+ i) length)
              ,@body)))

;; From `orb--with-message!'
(defmacro kisaragi-notes//with-message (message &rest body)
  "Put MESSAGE before and after BODY.

Echo \"MESSAGE...\", run BODY, then echo \"MESSAGE...done\"
afterwards. The value of BODY is returned."
  (declare (indent 1) (debug (stringp &rest form)))
  `(prog2
       (message "%s..." ,message)
       (progn ,@body)
     (message "%s...done" ,message)))

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

(defun kisaragi-notes//remove-org-links (str)
  "Remove Org bracket links from STR."
  (let ((links (s-match-strings-all org-link-bracket-re str)))
    (--> (cl-loop for link in links
                  collect
                  (let ((orig (elt link 0))
                        (desc (or (elt link 2)
                                  (elt link 1))))
                    (cons orig desc)))
      (s-replace-all it str))))

(defun kisaragi-notes//today (&optional n)
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

;;;; Title/Path/Slug conversion

(defun kisaragi-notes//path-to-title (path)
  "Convert PATH to a string that's suitable as a title."
  (-> path
    (f-relative (f-expand org-roam-directory))
    f-no-ext))

(defun kisaragi-notes//title-to-slug (title)
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
            kisaragi-notes/slug-replacements))))
    (downcase slug)))

;;;; File utilities

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

;;;; Utility Functions

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

(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam--url-p (path)
  "Check if PATH is a URL.
Assume the protocol is not present in PATH; e.g. URL `https://google.com' is
passed as `//google.com'."
  (string-prefix-p "//" path))

(defmacro org-roam-with-file (file keep-buf-p &rest body)
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

(defmacro org-roam--with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.
Like `with-temp-buffer', but propagates `org-roam-directory'.
If FILE, set `org-roam-temp-file-name' to file and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory)
               (org-mode-hook nil)
               (org-inhibit-startup t))
           (org-mode)
           (when ,file
             (insert-file-contents ,file)
             (setq-local kisaragi-notes//file-name ,file)
             (setq-local default-directory (file-name-directory ,file)))
           ,@body)))))

(defun org-roam-message (format-string &rest args)
  "Pass FORMAT-STRING and ARGS to `message' when `org-roam-verbose' is t."
  (when org-roam-verbose
    (apply #'message `(,(concat "(org-roam) " format-string) ,@args))))

(defun org-roam-string-quote (str)
  "Quote STR."
  (->> str
    (s-replace "\\" "\\\\")
    (s-replace "\"" "\\\"")))

;;; Shielding regions
(defun org-roam-shield-region (beg end)
  "Shield REGION against modifications.
REGION must be a cons-cell containing the marker to the region
beginning and maximum values."
  (when (and beg end)
    (add-text-properties beg end
                         '(font-lock-face org-roam-link-shielded
                                          read-only t)
                         (marker-buffer beg))
    (cons beg end)))

(defun org-roam-unshield-region (beg end)
  "Unshield the shielded REGION."
  (when (and beg end)
    (let ((inhibit-read-only t))
      (remove-text-properties beg end
                              '(font-lock-face org-roam-link-shielded
                                               read-only t)
                              (marker-buffer beg)))
    (cons beg end)))

(provide 'kisaragi-notes-utils)

;;; kisaragi-notes-utils.el ends here
