;;; minaduki-mode.el --- The entry point minor modes -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'dash)

(require 'minaduki-extract)
(require 'minaduki-db)
(require 'minaduki-commands)
(require 'minaduki-buffer)

(require 'kisaragi-notes-wikilink)

;;;; Function faces
(defun minaduki::link-to-current-p ()
  "Return t if the link at point points to the current file."
  (save-match-data
    (let ((current-file (buffer-file-name minaduki-buffer//current))
          (link-dest
           (pcase (minaduki--file-type)
             ('org (let* ((context (org-element-context))
                          (type (org-element-property :type context))
                          (dest (org-element-property :path context)))
                     (pcase type
                       ("id" (minaduki-db//fetch-file :id dest))
                       (_ dest))))
             ('markdown
              (when (markdown-link-p)
                (let ((dest (markdown-link-url)))
                  (if (s-prefix? "#" dest)
                      (minaduki-db//fetch-file :id dest)
                    (expand-file-name dest))))))))
      (string= current-file link-dest))))

(defun minaduki::file-link-face (path)
  "Conditional face for file: links.
Applies `org-roam-link-current' if PATH corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if PATH corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (minaduki//in-vault?)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and custom
                  (not (file-remote-p path)) ;; Prevent lockups opening Tramp links
                  (not (file-exists-p path)))
             'org-roam-link-invalid)
            ((and (bound-and-true-p minaduki-buffer/mode)
                  (minaduki::link-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (minaduki//in-vault? path))
             'org-roam-link)
            (t
             'org-link)))))

(defun minaduki::id-link-face (id)
  "Conditional face for id links.
Applies `org-roam-link-current' if ID corresponds to the
currently opened Org-roam file in the backlink buffer, or
`org-roam-link-face' if ID corresponds to any other Org-roam
file."
  (save-match-data
    (let* ((in-note (-> (buffer-file-name (buffer-base-buffer))
                        (minaduki//in-vault?)))
           (custom (or (and in-note org-roam-link-use-custom-faces)
                       (eq org-roam-link-use-custom-faces 'everywhere))))
      (cond ((and (bound-and-true-p minaduki-buffer/mode)
                  (minaduki::link-to-current-p))
             'org-roam-link-current)
            ((and custom
                  (minaduki-db//fetch-file :id id))
             'org-roam-link)
            ;; FIXME: this breaks the display of ID links to untracked
            ;; files.
            ((and custom
                  (not (minaduki-db//fetch-file :id id)))
             'org-roam-link-invalid)
            (t
             'org-link)))))

;;;; Hooks and advices
(defun minaduki::delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (minaduki//in-vault? file))
    (minaduki-db//clear-file (expand-file-name file))))

(defun minaduki-org::get-link-replacement (old-path new-path &optional old-desc new-desc)
  "Create replacement text for link at point if OLD-PATH is a match.
Will update link to NEW-PATH. If OLD-DESC is set, and is not the
same as the link description, it is assumed that the user has
modified the description, and the description will not be
updated. Else, update with NEW-DESC."
  (let (path label new-label)
    (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
      (setq path (org-element-property :path link))
      (when (and (string-equal (expand-file-name path) old-path)
                 (org-in-regexp org-link-bracket-re 1))
        (setq label (if (match-end 2)
                        (match-string-no-properties 2)
                      (org-link-unescape (match-string-no-properties 1))))
        (setq new-label (if (string-equal label old-desc) new-desc label))
        (minaduki/format-link :target new-path
                              :desc new-label)))))

(defun minaduki-org::replace-link (old-path new-path &optional old-desc new-desc)
  "Replace Org-roam file links with path OLD-PATH to path NEW-PATH.
If OLD-DESC is passed, and is not the same as the link
description, it is assumed that the user has modified the
description, and the description will not be updated. Else,
update with NEW-DESC."
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (when-let ((link (save-match-data (minaduki-org::get-link-replacement old-path new-path old-desc new-desc))))
        (replace-match link)))))

(defun minaduki-org::fix-relative-links (old-path)
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
                  (concat type ":" (minaduki::convert-path-format (expand-file-name path (file-name-directory old-path)))))
            (replace-match new-link nil t nil 1)))))))

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
               (minaduki//in-vault? old-file))
      (minaduki-db//ensure-built)
      (setq files-affected (minaduki-db/query [:select :distinct [source]
                                               :from links
                                               :where (= dest $s1)]
                                              old-file))
      ;; Remove database entries for old-file.org
      (minaduki-db//clear-file old-file)
      ;; If the new path is in a different directory, relative links
      ;; will break. Fix all file-relative links:
      (unless (string= (file-name-directory old-file)
                       (file-name-directory new-file))
        (minaduki//with-file new-file nil
          (minaduki-org::fix-relative-links old-file)))
      (when (minaduki//in-vault? new-file)
        (minaduki-db//update-file new-file))
      ;; Replace links from old-file.org -> new-file.org in all Org-roam files with these links
      (mapc (lambda (file)
              (setq file (if (string-equal (car file) old-file)
                             new-file
                           (car file)))
              (minaduki//with-file file nil
                (minaduki-org::replace-link old-file new-file)
                (save-buffer)
                (minaduki-db//update-file)))
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
                    (search-forward tag)
                    (let* ((end (point))
                           (start (- end (length tag)))
                           ;; Introduce a new scope for each iteration
                           ;; Otherwise all iterations share the same
                           ;; `tag' variable.
                           (tag tag)
                           (file (car (minaduki-db//fetch-file
                                       :title tag
                                       :nocase? t))))
                      (remove-overlays start end 'face 'button)
                      (when file
                        (make-button
                         start end
                         'action (lambda (&rest _)
                                   (minaduki//find-file file))
                         'follow-link t
                         'face 'button))))))))

;;;; The minor mode itself
(defun minaduki::local-mode-enable ()
  "Do the actual work to enable `minaduki-local-mode'."
  (setq minaduki//last-window (get-buffer-window))
  (pcase (minaduki--file-type)
    ('org
     (add-hook 'before-save-hook #'org-roam-link--replace-link-on-save nil t)
     (add-hook 'post-command-hook #'minaduki-org::buttonize-tags nil t)))
  (pcase-dolist (`(,name ,path)
                 ;; Ensure first element in `minaduki/vaults' is the
                 ;; first element in `org-link-abbrev-alist' by
                 ;; setting it last.
                 (reverse minaduki/vaults))
    (when path
      (setf (map-elt org-link-abbrev-alist name) path)))
  (add-hook 'post-command-hook #'minaduki-buffer//update-maybe nil t)
  (add-hook 'after-save-hook #'minaduki-db/update nil t)
  (dolist (fn '(minaduki-completion/tags-at-point
                minaduki-completion/everywhere))
    (add-hook 'completion-at-point-functions fn nil t))
  (minaduki-buffer//update-maybe :redisplay t))

(define-minor-mode minaduki-local-mode
  "Minor mode active in files tracked by minaduki."
  ;; FIXME: this doesn't actually turn itself off. You have to turn
  ;; off the global mode and revert the file.
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map
              [remap markdown-follow-thing-at-point]
              #'minaduki-markdown-follow)
            map)
  (minaduki::local-mode-enable))

(defun minaduki-initialize ()
  "Initialize minaduki for this buffer."
  (when (minaduki//in-vault?)
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
            (define-key map (kbd "C-c ) a") #'minaduki/local-commands)
            map)
  :require 'minaduki
  (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                   (file-executable-p emacsql-sqlite3-executable))
              (executable-find "sqlite3"))
    (minaduki//warn :error "Cannot find executable 'sqlite3'. \
Ensure it is installed and can be found within `exec-path'."))
  (if minaduki-mode
      (progn
        (org-cite-register-processor 'minaduki
          :follow #'minaduki-cite//follow
          :insert (org-cite-make-insert-processor
                   #'minaduki-completion//read-lit-entry
                   #'org-cite-basic--complete-style))
        (setq org-cite-follow-processor 'minaduki
              org-cite-insert-processor 'minaduki)
        (add-hook 'after-change-major-mode-hook 'minaduki-initialize)
        (add-hook 'find-file-hook 'minaduki-initialize)
        (add-hook 'kill-emacs-hook #'minaduki-db//close)
        (when (and (not minaduki-db/file-update-timer)
                   (eq minaduki-db/update-method 'idle-timer))
          (setq minaduki-db/file-update-timer (run-with-idle-timer minaduki-db/update-idle-seconds t #'minaduki-db/update-cache-on-timer)))
        (advice-add 'rename-file :after #'minaduki::rename-file-advice)
        (advice-add 'delete-file :before #'minaduki::delete-file-advice)
        (add-to-list 'org-execute-file-search-functions 'minaduki-org//move-to-row-col)
        (add-hook 'org-open-at-point-functions #'minaduki/open-id-at-point)
        (advice-add 'org-id-new :after #'minaduki-org//id-new-advice)
        (setq calendar-mark-diary-entries-flag t)
        (advice-add 'diary-mark-entries :override #'minaduki//mark-calendar)
        (advice-add 'org-read-date :before #'minaduki//set-calendar-mark-diary-entries-flag-nil)
        (advice-add 'org-read-date :after #'minaduki//set-calendar-mark-diary-entries-flag-t)
        (when (fboundp 'org-link-set-parameters)
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
    (remove-hook 'kill-emacs-hook #'minaduki-db//close)
    (remove-hook 'org-open-at-point-functions #'minaduki/open-id-at-point)
    (when minaduki-db/file-update-timer
      (cancel-timer minaduki-db/file-update-timer))
    (advice-remove 'rename-file #'minaduki::rename-file-advice)
    (advice-remove 'delete-file #'minaduki::delete-file-advice)
    (advice-remove 'org-id-new #'minaduki-org//id-new-advice)
    (when (fboundp 'org-link-set-parameters)
      (dolist (face '("file" "id"))
        (org-link-set-parameters face :face 'org-link)))
    (minaduki-db//close)
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

(add-hook 'minaduki-mode-hook #'minaduki-db/build-cache)

(provide 'minaduki-mode)

;;; minaduki-mode.el ends here
