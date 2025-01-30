;;; minaduki-mode.el --- The entry point minor modes -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'dash)
(require 'map)

(require 'minaduki-vars)
(require 'minaduki-extract)
(require 'minaduki-db)
(require 'minaduki-btn)
(require 'minaduki-commands)
(require 'minaduki-buffer)

(require 'minaduki-wikilink)

(declare-function org-cite-basic--complete-style "oc-basic")
;; The minor mode is defined at the end, after functions used it. So declare it
;; here.
(defvar minaduki-local-mode)

(defun minaduki:buffer-name-for-display ()
  "Return a name for the current buffer suitable for display."
  (or (car (minaduki-extract/main-title))
      (buffer-name)))

;;;; Function faces
(defun minaduki::link-to-current-p ()
  "Return t if the link at point points to the current file."
  (save-match-data
    (let ((current-file (buffer-file-name minaduki-buffer//current))
          (link-dest
           (minaduki::file-type-case
             (:org (let* ((context (org-element-context))
                          (type (org-element-property :type context))
                          (dest (org-element-property :path context)))
                     (pcase type
                       ("id" (minaduki-db::fetch-file :id dest))
                       (_ dest))))
             (:markdown
              (when (markdown-link-p)
                (let ((dest (markdown-link-url)))
                  (if (s-prefix? "#" dest)
                      (minaduki-db::fetch-file :id dest)
                    (expand-file-name dest))))))))
      (string= current-file link-dest))))

(defun minaduki::apply-link-faces? ()
  "Return whether we should apply custom link faces in the current context."
  (or (and (-> (buffer-file-name (buffer-base-buffer))
             minaduki-vault:in-vault?)
           minaduki:use-custom-link-faces)
      (eq minaduki:use-custom-link-faces 'everywhere)))

(defun minaduki-org::a-fuzzy-face (orig-func type key)
  "Advice for `org-link-get-parameter' to apply face for nonexistant wiki links.

ORIG-FUNC is the original `org-link-get-parameter'.
TYPE and KEY are passed to ORIG-FUNC. When TYPE is \"fuzzy\" and
KEY is `:face', return `minaduki-link-invalid' if the link at
point resolves to a nonexistant file."
  (cl-block nil
    (unless (and minaduki-local-mode
                 (equal type "fuzzy")
                 (eq key :face)
                 (minaduki::apply-link-faces?))
      (cl-return (funcall orig-func type key)))
    (lambda (path)
      (let ((resolved (minaduki-obsidian-path path)))
        (if (and (not (file-remote-p resolved)) ; Prevent looking up remote files
                 (not (file-exists-p resolved)))
            'minaduki-link-invalid
          'minaduki-link)))))

(defun minaduki::file-link-face (path)
  "Conditional face for file: links.
Applies the `minaduki-link-current' face if PATH corresponds
to the current file in the backlink buffer, or the
`minaduki-link' face if PATH corresponds to any other
file in a vault."
  (save-match-data
    (let ((custom (minaduki::apply-link-faces?)))
      (cond ((and custom
                  (not (file-remote-p path)) ; Prevent lockups opening Tramp links
                  (not (file-exists-p path)))
             'minaduki-link-invalid)
            ((and custom
                  (bound-and-true-p minaduki-buffer/mode)
                  (minaduki::link-to-current-p))
             'minaduki-link-current)
            ((and custom
                  (minaduki-vault:in-vault? path))
             'minaduki-link)
            (t
             'org-link)))))

(defun minaduki::id-link-face (id)
  "Conditional face for id links.
Applies the `minaduki-link-current' face if ID corresponds
to the current file in the backlink buffer, or the
`minaduki-link' face if ID corresponds to any other
file in a vault."
  (save-match-data
    (let* ((in-vault (-> (buffer-file-name (buffer-base-buffer))
                         (minaduki-vault:in-vault?)))
           (custom (or (and in-vault minaduki:use-custom-link-faces)
                       (eq minaduki:use-custom-link-faces 'everywhere))))
      (cond ((and custom
                  (bound-and-true-p minaduki-buffer/mode)
                  (minaduki::link-to-current-p))
             'minaduki-link-current)
            ((and custom
                  (minaduki-db::fetch-file :id id))
             'minaduki-link)
            ;; FIXME: this breaks the display of ID links to untracked
            ;; files.
            ((and custom
                  (not (minaduki-db::fetch-file :id id)))
             'minaduki-link-invalid)
            (t
             'org-link)))))

;;;; Custom link following behavior
(defun minaduki::a-markdown-follow (orig-func arg)
  "Use `minaduki-markdown-follow' to replace `markdown-follow-thing-at-point'.
ORIG-FUNC is the original function.
ARG is whether the thing should be opened in another window."
  ;; HACK: this allows `minaduki-markdown-follow' to use the original
  ;; `markdown-follow-thing-at-point' directly without receiving an
  ;; ORIG-FUNC argument itself. This way `minaduki-markdown-follow'
  ;; can still be used as a normal command.
  (cl-letf (((symbol-function
              'markdown-follow-thing-at-point)
             orig-func))
    (if minaduki-local-mode
        (minaduki-markdown-follow arg)
      (funcall orig-func arg))))

(defun minaduki-org::h-open-wiki-link (target)
  "`org-open-link-functions' handler for Obsidian-style wiki links in Org.
TARGET is the link's target, as passed to `org-open-link-functions'."
  (let ((resolved-path (minaduki-obsidian-path target)))
    (minaduki::find-file resolved-path)))

(defun minaduki-org::fuzzy-follow (target)
  "A follow function that acts like Org [[plain]] links.

This reads the link from the point position. TARGET is passed in
by `org-link-open', which we\\='ll forward onto a request to open
a fuzzy link.

Minaduki overrides [[plain]] links as Markdown-style wiki links,
so we need another way to access fuzzy link features.

Minaduki binds this to [[f:<target>]]."
  (let ((link (org-element-context))
        (org-open-link-functions
         (remove 'minaduki-org::h-open-wiki-link org-open-link-functions)))
    (org-link-open
     `(link (:type "fuzzy" :path ,target
             :format bracket :raw-link ,target
             :application nil :search-option nil
             ;; This is needed by org-link-open, so forward it.
             ;; (We don't need the buffer as it's the current buffer)
             ;; HACK: the 2 is the length of "f:". By reporting the start as if
             ;; the "f:" isn't there, org-link-open can continue to successfully
             ;; prevent fuzzy links from matching themselves.
             :begin ,(+ 2 (org-element-begin link)))))))

;;;; Link replacement after deletion and rename
(defun minaduki::a-delete-file (file &optional _trash)
  "Advice: as FILE is deleted, delete its cache entries as well."
  (when (and (not (auto-save-file-name-p file))
             (minaduki-vault:in-vault? file))
    (minaduki-db::clear-file (expand-file-name file))))

(defun minaduki::get-link-replacement (old-path new-path &optional old-desc new-desc)
  "Create replacement text for link at point if OLD-PATH is a match.
Will update link to NEW-PATH. If OLD-DESC is set, and is not the
same as the link description, it is assumed that the user has
modified the description, and the description will not be
updated. Else, update with NEW-DESC."
  (minaduki::file-type-case
    (:markdown
     (let ((type :obsidian) link
           label new-label)
       (setq link (minaduki::link-obsidian::parse))
       (unless link
         (setq type :markdown)
         (setq link (minaduki::link-markdown::parse)))
       (when link
         (when (equal (expand-file-name (oref link to))
                      old-path)
           (setq label (or (oref link desc)
                           (oref link to)))
           (setq new-label (if (equal label old-desc)
                               new-desc
                             label))
           (minaduki::link::write
            (minaduki-link
             :to (minaduki-vault:path-relative new-path)
             :desc new-label)
            type)))))
    (:org
     (let (label new-label)
       (when-let (link (minaduki::link-org::parse))
         (when (and (equal (expand-file-name (oref link to))
                           old-path)
                    (org-in-regexp org-link-bracket-re 1))
           (setq label (or (oref link desc)
                           (org-link-unescape (oref link to))))
           (setq new-label (if (equal label old-desc)
                               new-desc
                             label))
           (minaduki::format-link
            :target (minaduki-vault:path-relative new-path)
            :desc new-label)))))))

(defun minaduki::replace-link (old-path new-path &optional old-desc new-desc)
  "Replace Org-roam file links with path OLD-PATH to path NEW-PATH.
If OLD-DESC is passed, and is not the same as the link
description, it is assumed that the user has modified the
description, and the description will not be updated. Else,
update with NEW-DESC."
  (org-with-wide-buffer
   (goto-char 1)
   (while (re-search-forward
           (minaduki::file-type-case
             (:org org-link-bracket-re)
             (:markdown (concat "\\(?:" markdown-regex-link-inline
                                "\\|" markdown-regex-angle-uri
                                "\\|" markdown-regex-uri
                                "\\|" markdown-regex-email
                                "\\|" minaduki--wikilink-regexp
                                "\\)"))
             (t "^^"))
           nil t)
     (save-excursion
       (goto-char (match-beginning 0))
       (when-let ((link (save-match-data (minaduki::get-link-replacement old-path new-path old-desc new-desc))))
         (replace-match link))))))

;; TODO: markdown
(defun minaduki::fix-relative-links (old-path)
  "Fix file-relative links in current buffer.
File relative links are assumed to originate from OLD-PATH. The
replaced links are made relative to the current buffer."
  (minaduki::file-type-case
    (:org
     (org-with-wide-buffer
      (goto-char 1)
      (let (link new-link type path)
        (while (re-search-forward org-link-bracket-re nil t)
          (when (setq link (save-match-data (org-element-lineage (org-element-context) '(link) t)))
            (setq type (org-element-property :type link))
            (setq path (org-element-property :path link))
            (when (and (string= type "file")
                       (f-relative-p path))
              (setq new-link
                    (concat type ":" (minaduki::convert-path-format (expand-file-name path (file-name-directory old-path)))))
              (replace-match new-link nil t nil 1)))))))))

(defun minaduki::rename-file-advice (old-file new-file-or-dir &rest _args)
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
               (minaduki-vault:in-vault? old-file))
      (minaduki-db::ensure-built)
      (setq files-affected
            (minaduki-db-select "select distinct source from links where dest = ?" old-file))
      ;; Remove database entries for old-file.org
      (minaduki-db::clear-file old-file)
      ;; If the new path is in a different directory, relative links
      ;; will break. Fix all file-relative links:
      (unless (string= (file-name-directory old-file)
                       (file-name-directory new-file))
        (minaduki::with-file new-file nil
          (minaduki::fix-relative-links old-file)))
      (when (minaduki-vault:in-vault? new-file)
        (minaduki-db:update-file new-file))
      ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
      (mapc (lambda (file)
              (setq file (if (string-equal (car file) old-file)
                             new-file
                           (car file)))
              (minaduki::with-file file nil
                (minaduki::replace-link old-file new-file)
                (save-buffer)
                (minaduki-db:update-file)))
            files-affected))))

(defun minaduki-org::buttonize-tags ()
  "Turn tags into buttons in this buffer."
  (when (eq 'org-mode major-mode)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx bol "#+" (or "tags[]" "roam_tags") ":") nil t)
      (cl-loop for tag in (--sort (> (length it)
                                     (length other))
                                  (minaduki-extract//tags/org-prop))
               do (save-excursion
                    (re-search-forward
                     (regexp-quote
                      ;; We use `combine-and-quote-strings' because it
                      ;; is the inverse of `split-string-and-unquote',
                      ;; used when collecting the value list.
                      (combine-and-quote-strings
                       (list tag))))
                    (let* ((end (point))
                           (start (- end (length tag)))
                           ;; Introduce a new scope for each iteration
                           ;; Otherwise all iterations share the same
                           ;; `tag' variable.
                           (tag tag)
                           (file (car (minaduki-db::fetch-file
                                       :title tag
                                       :nocase? t))))
                      (remove-overlays start end 'face 'button)
                      (when file
                        (make-button
                         start end
                         'action (lambda (&rest _)
                                   (minaduki::find-file file))
                         'follow-link t
                         'face 'button))))))))

(defun minaduki-mode::handle-double-bracket-h ()
  "Run completion after inserting double brackets."
  (when (and (eql ?\[ (char-before (point)))
             ;; This does not cause out-of-bounds at point = 1
             ;; because (char-before 0) is nil and does not error
             (eql ?\[ (char-before (1- (point)))))
    ;; Delete the open brackets
    (delete-char -2)
    ;; Try to handle the auto brackets added by Smartparens etc.
    ;; We try twice, because there's two open brackets. But we also don't assert
    ;; that there's either none or two: there could be just one if Smartparen's
    ;; post-self-insert-hook function hasn't run yet.
    (when (eql ?\] (char-after (point)))
      (delete-char 1))
    (when (eql ?\] (char-after (point)))
      (delete-char 1))
    (call-interactively #'minaduki-insert)))

;;;; The minor mode itself
(defun minaduki::local-mode-enable ()
  "Do the actual work to enable `minaduki-local-mode'."
  (setq minaduki::last-window (get-buffer-window))
  (when minaduki:note-title-in-frame-title
    (setq-local frame-title-format
                (if (stringp frame-title-format)
                    (->> frame-title-format
                         (s-split "%b")
                         (-interpose '(:eval (minaduki:buffer-name-for-display)))
                         (-remove-item ""))
                  '((:eval (minaduki:buffer-name-for-display))
                    " - GNU Emacs"))))
  (minaduki::file-type-case
    (:org
     ;; FIXME: org-open-link-functions happens before Org does the current
     ;; buffer file search, and will override the target seeking behavior. This
     ;; makes it somewhat unusable in this case.
     (add-hook 'org-open-link-functions #'minaduki-org::h-open-wiki-link nil t)
     (add-hook 'before-save-hook #'minaduki-wikilink::replace-link-on-save nil t)
     (add-hook 'post-command-hook #'minaduki-org::buttonize-tags nil t)))
  (dolist (vault
           ;; Ensure first element in `minaduki/vaults' is the
           ;; first element in `org-link-abbrev-alist' by
           ;; setting it last.
           (reverse minaduki/vaults))
    (when (minaduki-vault-path vault)
      (setf (map-elt org-link-abbrev-alist (minaduki-vault-name vault))
            (f-slash
             (minaduki-vault-path vault)))))
  (add-hook 'post-command-hook #'minaduki-buffer//update-maybe nil t)
  (add-hook 'after-save-hook #'minaduki-db::incremental-update nil t)
  (add-hook 'post-self-insert-hook #'minaduki-mode::handle-double-bracket-h nil t)
  (dolist (fn '(minaduki-completion/tags-at-point))
    (add-hook 'completion-at-point-functions fn nil t))
  (minaduki-buffer//update-maybe :redisplay t))

(define-minor-mode minaduki-local-mode
  "Minor mode active in files tracked by minaduki.

This should not be called directly. `minaduki-mode' enables this
when appropriate."
  ;; FIXME: this doesn't actually turn itself off. You have to turn
  ;; off the global mode and revert the file.
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (cl-loop
             for (suffix . cmd)
             in '(("n" . minaduki:local-commands)
                  ("l" . minaduki:toggle-sidebar))
             do (let ((key (format "%s %s" minaduki-mode:command-prefix suffix)))
                  (define-key map (kbd key) cmd)))
            map)
  (minaduki::local-mode-enable))

(defun minaduki-initialize ()
  "Initialize minaduki for this buffer."
  (when (minaduki-vault:in-vault?)
    (minaduki-local-mode)))

(define-minor-mode minaduki-mode
  "Toggle Minaduki-Local mode in all buffers.

With prefix ARG, enable Minaduki mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Minaduki-Local mode is enabled in all buffers where
`minaduki-initialize' would do it.
See `minaduki-local-mode' for more information on Minaduki-Local mode."
  :global t
  :group 'minaduki
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map
                        (kbd (format "%s N" minaduki-mode:command-prefix))
                        #'minaduki:global-commands)
            map)
  :require 'minaduki
  (unless (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))
    (minaduki::warn :error "SQLite support not found in this Emacs. Minaduki requires the builtin SQLite support found in Emacs 29 and higher."))
  (if minaduki-mode
      (progn
        (org-cite-register-processor 'minaduki
          :follow #'minaduki-cite//follow
          :insert (org-cite-make-insert-processor
                   #'minaduki-read:lit-entry
                   #'org-cite-basic--complete-style))
        (setq org-cite-follow-processor 'minaduki
              org-cite-insert-processor 'minaduki)
        (add-hook 'after-change-major-mode-hook 'minaduki-initialize)
        (add-hook 'find-file-hook 'minaduki-initialize)
        (when (and (not minaduki-db::file-update-timer)
                   (eq minaduki-db/update-method 'idle-timer))
          (setq minaduki-db::file-update-timer (run-with-idle-timer minaduki-db/update-idle-seconds t #'minaduki-db::file-update-timer::update-cache)))
        (advice-add 'rename-file :after #'minaduki::rename-file-advice)
        (advice-add 'delete-file :before #'minaduki::a-delete-file)
        (advice-add 'markdown-follow-thing-at-point :around #'minaduki::a-markdown-follow)
        (advice-add 'org-link-get-parameter :around #'minaduki-org::a-fuzzy-face)
        (add-to-list 'org-execute-file-search-functions 'minaduki-org//move-to-row-col)
        (add-hook 'org-open-at-point-functions #'minaduki/open-id-at-point)
        (advice-add 'org-id-new :after #'minaduki-org//id-new-advice)
        (setq calendar-mark-diary-entries-flag t)
        (advice-add 'diary-mark-entries :override #'minaduki//mark-calendar)
        (advice-add 'org-read-date :before #'minaduki//set-calendar-mark-diary-entries-flag-nil)
        (advice-add 'org-read-date :after #'minaduki//set-calendar-mark-diary-entries-flag-t)
        (when (fboundp 'org-link-set-parameters)
          (org-link-set-parameters minaduki-wikilink::type :follow #'minaduki-wikilink:follow)
          (org-link-set-parameters "minaduki-btn" :follow #'minaduki-btn:follow)
          (org-link-set-parameters "info" :follow #'info)
          (org-link-set-parameters "f" :follow #'minaduki-org::fuzzy-follow)
          (org-link-set-parameters "file" :face 'minaduki::file-link-face)
          (org-link-set-parameters "id" :face 'minaduki::id-link-face))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (minaduki-initialize))))
    (org-cite-unregister-processor 'minaduki)
    (setq org-cite-follow-processor 'basic
          org-cite-insert-processor 'basic)
    (remove-hook 'after-change-major-mode-hook 'minaduki-initialize)
    (remove-hook 'find-file-hook 'minaduki-initialize)
    (remove-hook 'org-open-at-point-functions #'minaduki/open-id-at-point)
    (when minaduki-db::file-update-timer
      (cancel-timer minaduki-db::file-update-timer))
    (advice-remove 'rename-file #'minaduki::rename-file-advice)
    (advice-remove 'delete-file #'minaduki::a-delete-file)
    (advice-remove 'org-id-new #'minaduki-org//id-new-advice)
    (advice-remove 'markdown-follow-thing-at-point #'minaduki::a-markdown-follow)
    (advice-remove 'org-link-get-parameter #'minaduki-org::a-fuzzy-face)
    (when (fboundp 'org-link-set-parameters)
      (org-link-set-parameters minaduki-wikilink::type :follow nil)
      (org-link-set-parameters "minaduki-btn" :follow nil)
      (org-link-set-parameters "f" :follow nil)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))
    (minaduki-db::close)
    (setq calendar-mark-diary-entries-flag nil)
    (advice-remove 'diary-mark-entries
                   #'minaduki//mark-calendar)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-nil)
    (advice-remove 'org-read-date
                   #'minaduki//set-calendar-mark-diary-entries-flag-t)
    ;; TODO: we could keep track of the buffers with
    ;; minaduki-local-mode on so we don't have to iterate through
    ;; every open buffer.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when minaduki-local-mode
          (minaduki-local-mode -1))))))

(provide 'minaduki-mode)

;;; minaduki-mode.el ends here
