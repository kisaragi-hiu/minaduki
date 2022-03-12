;;; org-roam-id.el --- ID functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-id)
(require 'org-element)
(require 'minaduki-db)

(defun minaduki-id/get-file (id &optional strict)
  "Return the file if ID exists.
When STRICT is non-nil, only consider Org-roam's database.
Return nil otherwise."
  (or (caar (minaduki-db/query [:select [file]
                                :from ids
                                :where (= id $s1)
                                :limit 1]
                               id))
      (and (not strict)
           (progn
             (unless org-id-locations (org-id-locations-load))
             (or (and org-id-locations
                      (hash-table-p org-id-locations)
                      (gethash id org-id-locations)))))))

(defun minaduki-id/find (id &optional markerp strict keep-buffer-p)
  "Return the location of the entry with the id ID.
When MARKERP is non-nil, return a marker pointing to the headline.
Otherwise, return a cons formatted as \(file . pos).
When STRICT is non-nil, only consider Org-roam’s database.
When KEEP-BUFFER-P is non-nil, keep the buffers navigated by Org-roam open."
  (let ((file (minaduki-id/get-file id strict)))
    (when file
      (org-roam-with-file file keep-buffer-p
        (org-id-find-id-in-file id file markerp)))))

(defun minaduki-id/open (id-or-marker &optional strict)
  "Go to the entry with ID-OR-MARKER.
Wrapper for `org-id-open' which tries to find the ID in the
Org-roam's database.
ID-OR-MARKER can either be the ID of the entry or the marker
pointing to it if it has already been computed by
`minaduki-id/find'. If the ID-OR-MARKER is not found, it reverts
to the default behaviour of `org-id-open'.
When STRICT is non-nil, only consider Org-roam’s database."
  (when-let ((marker (if (markerp id-or-marker)
                         id-or-marker
                       (minaduki-id/find id-or-marker t strict t))))
    (org-mark-ring-push)
    (org-goto-marker-or-bmk marker)
    (set-marker marker nil)))

(defun minaduki-id/open-id-at-point ()
  "Open link, timestamp, footnote or tags at point.
The function tries to open ID-links with Org-roam’s database
before falling back to the default behaviour of
`org-open-at-point'. It also asks the user whether to parse
`org-id-files' when an ID is not found because it might be a slow
process.
This function hooks into `org-open-at-point' via
`org-open-at-point-functions'."
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (id (org-element-property :path context)))
    (when (string= type "id")
      (cond ((minaduki-id/open id)
             t)
            ;; Ask whether to parse `org-id-files'
            ((not (y-or-n-p (concat "ID was not found in `org-directory' nor in `org-id-locations'.\n"
                                    "Search in `org-id-files'? ")))
             t)
            ;; Conditionally fall back to default behaviour
            (t
             nil)))))

(provide 'org-roam-id)
;;; org-roam-id.el ends here
