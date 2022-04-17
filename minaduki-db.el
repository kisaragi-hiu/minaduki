;;; minaduki-db.el --- Org-roam database API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library is provides the underlying database api to org-roam.
;;
;; - Low level DB interface
;; - minaduki-db/build-cache
;; - minaduki-db//get-* (except get-collection)
;;   minaduki-db//fetch-*
;;
;;; Code:
;;;; Library Requires
(eval-when-compile (require 'subr-x))
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'seq)

(eval-and-compile
  ;; For `org-with-wide-buffer'
  (require 'org-macs))

(require 'minaduki-utils)
(require 'minaduki-vars)
(require 'minaduki-extract)

(defvar org-agenda-files)

;;;; Options

(defconst minaduki-db//version 13)

(defvar minaduki-db//connection nil
  "Database connection to the cache.")

(defvar minaduki-db//dirty nil
  "Whether the cache needs to be updated.")

;;;; Core Functions

(defun minaduki-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
  (unless (and minaduki-db//connection
               (emacsql-live-p minaduki-db//connection))
    (let ((initialize? (not (file-exists-p minaduki/db-location))))
      (make-directory (file-name-directory minaduki/db-location) t)
      (let ((conn (emacsql-sqlite3 minaduki/db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (setq minaduki-db//connection conn)
        (when initialize?
          (minaduki-db//init conn))
        (let* ((version (caar (emacsql conn "PRAGMA user_version")))
               (version (minaduki-db//upgrade-maybe conn version)))
          (cond
           ((> version minaduki-db//version)
            (emacsql-close conn)
            (user-error
             "The Org-roam database was created with a newer Org-roam version.  "
             "You need to update the Org-roam package"))
           ((< version minaduki-db//version)
            (emacsql-close conn)
            (error "BUG: The Org-roam database scheme changed %s"
                   "and there is no upgrade path")))))))
  minaduki-db//connection)

;;;; Entrypoint: (minaduki-db/query)
(defun minaduki-db/query (sql &rest args)
  "Run SQL query on Org-roam database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (let ((print-level nil)
        (print-length nil))
    (if (stringp sql)
        (emacsql (minaduki-db) (apply #'format sql args))
      (apply #'emacsql (minaduki-db) sql args))))

;;;; Schemata
(defconst minaduki-db//table-schemata
  '((files
     [(file :unique :primary-key)
      (hash :not-null)
      (meta :not-null)])

    (ids
     [(id :unique :primary-key)
      (file :not-null)
      (level :not-null)
      title])

    (links
     [(source :not-null)
      (dest :not-null)
      (type :not-null)
      (properties :not-null)])

    (tags
     [(file :unique :primary-key)
      (tags)])

    (titles
     [(file :not-null)
      title])

    ;; TODO: "keys" maps keys to literature entry listings; "refs"
    ;; maps keys to note files. These should be in the same table.
    (keys
     [(key :unique :not-null)
      (file :not-null)
      (point :not-null)
      (props :not-null)])

    (refs
     [(ref :unique :not-null)
      (file :not-null)
      (type :not-null)])))

(defun minaduki-db//init (conn)
  "Initialize database connection CONN with the correct schema and user version."
  (emacsql-with-transaction conn
    (pcase-dolist (`(,table . ,schema) minaduki-db//table-schemata)
      (emacsql conn [:create-table $i1 $S2] table schema))
    (emacsql conn (format "PRAGMA user_version = %s" minaduki-db//version))))

(defun minaduki-db//upgrade-maybe (db version)
  "Upgrades the database schema for DB, if VERSION is old."
  (emacsql-with-transaction db
    'ignore
    (if (< version minaduki-db//version)
        (progn
          (org-roam-message (format "Upgrading the Org-roam database from version %d to version %d"
                                    version minaduki-db//version))
          (minaduki-db/build-cache t))))
  version)

(defun minaduki-db//close ()
  "Close the connection with the cache database."
  (let ((conn minaduki-db//connection))
    (when (and conn (emacsql-live-p conn))
      (emacsql-close conn))))

;;;; Timer-based updating
(defvar minaduki-db/file-update-timer nil
  "Timer for updating the database when dirty.")

(defun minaduki-db/mark-dirty ()
  "Mark the Org-roam database as dirty."
  (setq minaduki-db//dirty t))

(defun minaduki-db/update-cache-on-timer ()
  "Update the cache if the database is dirty.
This function is called on `minaduki-db/file-update-timer'."
  (when minaduki-db//dirty
    (minaduki-db/build-cache))
  (setq minaduki-db//dirty nil))

;;;; Database API
;;;;; Initialization
(defun minaduki-db//initialized-p ()
  "Whether the Org-roam cache has been initialized."
  (and (file-exists-p minaduki/db-location)
       (> (caar (minaduki-db/query [:select (funcall count) :from titles]))
          0)))

(defun minaduki-db//ensure-built ()
  "Ensures that Org-roam cache is built."
  (unless (minaduki-db//initialized-p)
    (error "[Org-roam] your cache isn't built yet! Please run minaduki-db/build-cache")))

;;;;; Clearing
(defun minaduki-db/clear ()
  "Clears all entries in the Org-roam cache."
  (interactive)
  (when (file-exists-p minaduki/db-location)
    (dolist (table (mapcar #'car minaduki-db//table-schemata))
      (minaduki-db/query `[:delete :from ,table]))))

(defun minaduki-db//clear-file (&optional file)
  "Remove any related links to the FILE.
This is equivalent to removing the node from the graph."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (dolist (table (mapcar #'car minaduki-db//table-schemata))
    (minaduki-db/query `[:delete :from ,table
                         :where (= ,(if (eq table 'links) 'source 'file) $s1)]
                       file)))

;;;;; Inserting
(defun minaduki-db//insert-meta (&optional update-p)
  "Update the metadata of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the meta for the file in the database."
  (let* ((file (or minaduki//file-name (buffer-file-name)))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (minaduki//compute-content-hash)))
    (when update-p
      (minaduki-db/query [:delete :from files
                          :where (= file $s1)]
                         file))
    (minaduki-db/query
     [:insert :into files
      :values $v1]
     (list (vector file hash (list :atime atime :mtime mtime))))))

(defun minaduki-db//insert-titles (&optional update-p)
  "Update the titles of the current buffer into the cache.
If UPDATE-P is non-nil, first remove titles for the file in the database.
Returns the number of rows inserted."
  (let* ((file (or minaduki//file-name (buffer-file-name)))
         (titles (or (org-roam--extract-titles)
                     (list (minaduki//path-to-title file))))
         (rows (mapcar (lambda (title)
                         (vector file title))
                       titles)))
    (when update-p
      (minaduki-db/query [:delete :from titles
                          :where (= file $s1)]
                         file))
    (minaduki-db/query
     [:insert :into titles
      :values $v1]
     rows)
    (length rows)))

(defun minaduki-db//insert-lit-entries (&optional update-p)
  "Update the lit-entries of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the entries from the file in the database."
  (cl-block nil
    (let ((file (or minaduki//file-name (buffer-file-name)))
          (count 0))
      (when update-p
        (minaduki-db/query [:delete :from keys
                            :where (= file $s1)]
                           file))
      (cl-loop for (point . entry) in (minaduki-extract/lit-entries)
               do (progn
                    (minaduki-db/query
                     [:insert :into keys :values $v1]
                     (list (vector (gethash "key" entry)
                                   file
                                   point
                                   entry)))
                    (cl-incf count)))
      count)))

(defun minaduki-db//insert-refs (&optional update-p)
  "Update the refs of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the ref for the file in the database."
  (let ((file (or minaduki//file-name (buffer-file-name)))
        (count 0))
    (when update-p
      (minaduki-db/query [:delete :from refs
                          :where (= file $s1)]
                         file))
    (when-let ((refs (minaduki-extract/refs)))
      (dolist (ref refs)
        (let ((key (cdr ref))
              (type (car ref)))
          (condition-case nil
              (progn
                (minaduki-db/query
                 [:insert :into refs :values $v1]
                 (list (vector key file type)))
                (cl-incf count))
            (error
             (minaduki//warn
              :error
              "Duplicate ref %s in:\n\nA: %s\nB: %s\n\nskipping..."
              key
              file
              (caar (minaduki-db/query
                     [:select file :from refs
                      :where (= ref $v1)]
                     (vector key)))))))))
    count))

(defun minaduki-db//insert-links (&optional update-p)
  "Update the file links of the current buffer in the cache.
If UPDATE-P is non-nil, first remove the links for the file in the database.
Return the number of rows inserted."
  (let ((file (or minaduki//file-name (buffer-file-name))))
    (when update-p
      (minaduki-db/query [:delete :from links
                          :where (= source $s1)]
                         file))
    (if-let ((links (org-roam--extract-links)))
        (progn
          (minaduki-db/query
           [:insert :into links
            :values $v1]
           links)
          (length links))
      0)))

(defun minaduki-db//insert-ids (&optional update-p)
  "Update the ids of the current buffer into the cache.
If UPDATE-P is non-nil, first remove ids for the file in the database.
Returns the number of rows inserted."
  (let ((file (or minaduki//file-name (buffer-file-name))))
    (when update-p
      (minaduki-db/query [:delete :from ids
                          :where (= file $s1)]
                         file))
    (if-let ((ids (minaduki-extract/ids file)))
        (condition-case nil
            (progn
              (minaduki-db/query
               [:insert :into ids
                :values $v1]
               ids)
              (length ids))
          (error
           (minaduki//warn
            :error
            "Duplicate IDs in %s, one of:\n\n%s\n\nskipping..."
            (aref (car ids) 1)
            (string-join (mapcar (lambda (hl)
                                   (aref hl 0)) ids) "\n"))
           0))
      0)))

(defun minaduki-db//insert-tags (&optional update-p)
  "Insert tags for the current buffer into the Org-roam cache.
If UPDATE-P is non-nil, first remove tags for the file in the database.
Return the number of rows inserted."
  (let* ((file (or minaduki//file-name (buffer-file-name)))
         (tags (org-roam--extract-tags file)))
    (when update-p
      (minaduki-db/query [:delete :from tags
                          :where (= file $s1)]
                         file))
    (if tags
        (progn (minaduki-db/query
                [:insert :into tags
                 :values $v1]
                (list (vector file tags)))
               1)
      0)))

;;;;; Fetching
(defun minaduki-db//file-present? (file)
  "Does FILE exist in the cache DB?"
  (> (caar (minaduki-db/query [:select (funcall count) :from files
                               :where (= file $s1)]
                              file))
     0))

(cl-defun minaduki-db//fetch-file (&key title key id nocase?)
  "Return files from the DB.

When NOCASE? is non-nil, match case-insentively.

- ID: return the file containing a headline with ID.
- TITLE: return files with TITLE
- KEY: return the file associated with KEY."
  (let ((maybe-nocase (when nocase? `(:collate :nocase))))
    (cond
     (id
      (caar (minaduki-db/query
             `[:select [file] :from ids
               :where (= id $s1)
               :limit 1
               ,@maybe-nocase]
             id)))
     (title
      (->> (minaduki-db/query
            `[:select [file] :from titles
              :where (= title $s0)
              ,@maybe-nocase]
            title)
           ;; The above returns ((path1) (path2) ...).
           ;; Turn it into (path1 path2 ...).
           (apply #'nconc)))
     (key
      (caar (minaduki-db/query
             `[:select [file] :from refs
               :where (= ref $s0)
               ,@maybe-nocase]
             key))))))

(defun minaduki-db//fetch-lit-entry (key)
  "Fetch the literature entry with KEY in the DB."
  (caar (minaduki-db/query
         [:select [props] :from keys
          :where (= key $s1)]
         key)))

(defun minaduki-db//fetch-all-files-hash ()
  "Return ((path . content-hash) ...) for all cached files as a hash-table."
  (let* ((current-files (minaduki-db/query [:select [file hash] :from files]))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun minaduki-db//fetch-title (file)
  "Return the main title of FILE from the cache."
  (caar (minaduki-db/query [:select [title] :from titles
                            :where (= file $s1)
                            :limit 1]
                           file)))

(defun minaduki-db//fetch-all-tags ()
  "Return all distinct tags from the cache."
  (let ((rows (minaduki-db/query [:select :distinct [tags] :from tags]))
        acc)
    (dolist (row rows)
      (dolist (tag (car row))
        (unless (member tag acc)
          (push tag acc))))
    acc))

(defun minaduki-db//fetch-tag-references (tag)
  "Return files that are tagged with TAG."
  ;; We narrow the list as much as possible in SQLite with the
  ;; string match first, then do the accurate filtering in Emacs
  ;; Lisp afterwards.
  ;;
  ;; This seems to indeed be faster: try this (the 2 variant is with
  ;; the while clause removed)
  ;;
  ;; (list
  ;;  (benchmark-run-compiled 1000
  ;;    (minaduki-buffer//insert-tag-references "public"))
  ;;  (benchmark-run-compiled 1000
  ;;    (minaduki-buffer//insert-tag-references2 "public")))
  ;; ; -> ((1.961452458 2 0.3958529560000006)
  ;;       (12.077074859 12 2.0858843059999987)))
  (let ((candidates
         (minaduki-db/query `[:select [file tags] :from tags
                              :where (like tags (quote ,(concat "%" tag "%")))])))
    (cl-loop for cand in candidates
             when (member tag (cadr cand))
             collect (car cand))))

(defun minaduki-db//fetch-backlinks (targets)
  "Fetch backlinks to TARGETS from the cache.

TARGETS are strings that are either file paths or ref keys. They
correspond to the TO field in the cache DB."
  (unless (listp targets)
    (setq targets (list targets)))
  (let ((conditions
         ;; ((= dest target1) :or (= dest target2) :or ...)
         (--> targets
              (--map `(= dest ,it) it)
              (-interpose :or it))))
    (minaduki-db/query `[:select [source dest properties] :from links
                         :where ,@conditions
                         :order-by (asc source)])))

(defun minaduki-db//connected-component (file)
  "Return all files reachable from/connected to FILE, including the file itself.
If the file does not have any connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"dest\", \"file\" FROM citelinks),
                   connected_component(file) AS
                     (SELECT link FROM links_of WHERE file = $s1
                      UNION
                      SELECT link FROM links_of JOIN connected_component USING(file))
                   SELECT * FROM connected_component;")
         (files (mapcar 'car-safe (emacsql (minaduki-db) query file))))
    files))

(defun minaduki-db//links-with-max-distance (file max-distance)
  "Return all files connected to FILE in at most MAX-DISTANCE steps.
This includes the file itself. If the file does not have any
connections, nil is returned."
  (let* ((query "WITH RECURSIVE
                   links_of(file, link) AS
                     (WITH filelinks AS (SELECT * FROM links WHERE NOT \"type\" = '\"cite\"'),
                           citelinks AS (SELECT * FROM links
                                                  JOIN refs ON links.\"dest\" = refs.\"ref\"
                                                            AND links.\"type\" = '\"cite\"')
                      SELECT \"source\", \"dest\" FROM filelinks UNION
                      SELECT \"dest\", \"source\" FROM filelinks UNION
                      SELECT \"file\", \"source\" FROM citelinks UNION
                      SELECT \"source\", \"file\" FROM citelinks),
                   -- Links are traversed in a breadth-first search.  In order to calculate the
                   -- distance of nodes and to avoid following cyclic links, the visited nodes
                   -- are tracked in 'trace'.
                   connected_component(file, trace) AS
                     (VALUES($s1, json_array($s1))
                      UNION
                      SELECT lo.link, json_insert(cc.trace, '$[' || json_array_length(cc.trace) || ']', lo.link) FROM
                      connected_component AS cc JOIN links_of AS lo USING(file)
                      WHERE (
                        -- Avoid cycles by only visiting each file once.
                        (SELECT count(*) FROM json_each(cc.trace) WHERE json_each.value == lo.link) == 0
                        -- Note: BFS is cut off early here.
                        AND json_array_length(cc.trace) < ($s2 + 1)))
                   SELECT DISTINCT file, min(json_array_length(trace)) AS distance
                   FROM connected_component GROUP BY file ORDER BY distance;")
         ;; In principle the distance would be available in the second column.
         (files (mapcar 'car-safe (emacsql (minaduki-db) query file max-distance))))
    files))

(defun minaduki-db//fetch-file-hash (&optional file)
  "Fetch the hash of FILE as stored in the cache."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (caar (minaduki-db/query [:select hash :from files
                            :where (= file $s1)]
                           file)))

;;;;; Updating
(defun minaduki-db//update-file (&optional file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path
                      (buffer-file-name (buffer-base-buffer))))
  (if (not (file-exists-p file-path))
      (minaduki-db//clear-file file-path)
    ;; save the file before performing a database update
    (when-let ((buf (find-buffer-visiting file-path)))
      (with-current-buffer buf
        (save-buffer)))
    (minaduki//with-temp-buffer file-path
      (emacsql-with-transaction (minaduki-db)
        (when (member file-path minaduki-lit/bibliography)
          (minaduki-db//insert-lit-entries 'update))
        (unless (eq major-mode 'bibtex-mode)
          (minaduki-db//insert-meta 'update)
          (minaduki-db//insert-tags 'update)
          (minaduki-db//insert-titles 'update)
          (minaduki-db//insert-refs 'update)
          (minaduki-db//insert-ids 'update)
          (minaduki-db//insert-links 'update))))))

(defun minaduki-db/build-cache (&optional force)
  "Build the cache for `org-directory'.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file minaduki/db-location))
  (minaduki-db//close) ;; Force a reconnect
  (minaduki-db) ;; To initialize the database, no-op if already initialized
  (let* ((gc-cons-threshold minaduki-db/gc-threshold)
         (org-agenda-files nil)
         (deleted-count 0)
         dir-files db-files count-plist modified-files)
    (setq dir-files (org-roam--list-all-files)
          db-files (minaduki-db//fetch-all-files-hash))
    (dolist-with-progress-reporter (file dir-files)
        "(org-roam) Finding modified files"
      (let ((content-hash (minaduki//compute-content-hash file)))
        (unless (string= content-hash (gethash file db-files))
          (push (cons file content-hash) modified-files)))
      (remhash file db-files))
    (dolist-with-progress-reporter (file (hash-table-keys db-files))
        "(org-roam) Removing deleted files from cache"
      ;; These files are no longer around, remove from cache...
      (minaduki-db//clear-file file)
      (setq deleted-count (1+ deleted-count)))
    (setq count-plist (minaduki-db//update-files modified-files))
    (org-roam-message "total: Δ%s, files-modified: Δ%s, ids: Δ%s, links: Δ%s, tags: Δ%s, titles: Δ%s, refs: Δ%s, lit: Δ%s, deleted: Δ%s"
                      (- (length dir-files) (plist-get count-plist :error-count))
                      (plist-get count-plist :modified-count)
                      (plist-get count-plist :id-count)
                      (plist-get count-plist :link-count)
                      (plist-get count-plist :tag-count)
                      (plist-get count-plist :title-count)
                      (plist-get count-plist :ref-count)
                      (plist-get count-plist :lit-count)
                      deleted-count)))

(defun minaduki-db/update-file (file-path)
  "Update Org-roam cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (let ((content-hash (minaduki//compute-content-hash file-path))
        (db-hash (minaduki-db//fetch-file-hash file-path)))
    (unless (string= content-hash db-hash)
      (minaduki-db//update-files (list (cons file-path content-hash)))
      (org-roam-message "Updated: %s" file-path))))

(defun minaduki-db//update-files (file-hash-pairs)
  "Update Org-roam cache for FILE-HASH-PAIRS.
FILE-HASH-PAIRS is a list of (file . hash) pairs."
  (let* ((gc-cons-threshold minaduki-db/gc-threshold)
         (org-agenda-files nil)
         (error-count 0)
         (id-count 0)
         (link-count 0)
         (tag-count 0)
         (title-count 0)
         (ref-count 0)
         (lit-count 0)
         (modified-count 0))
    ;; Clear existing cache entries so that we can put in new versions
    (minaduki//for "Clearing files (%s/%s)..."
        (file . _) file-hash-pairs
      (minaduki-db//clear-file file))
    ;; Process titles and tags first to allow links to depend on
    ;; titles later; process IDs first so IDs are already cached
    ;; during link extraction
    (minaduki//for "Processing titles, tags, and lit-entries (%s/%s)..."
        (file . contents-hash) file-hash-pairs
      (condition-case nil
          (minaduki//with-temp-buffer file
            (let* ((attr (file-attributes file))
                   (atime (file-attribute-access-time attr))
                   (mtime (file-attribute-modification-time attr)))
              (minaduki-db/query
               [:insert :into files
                :values $v1]
               (vector file contents-hash (list :atime atime :mtime mtime))))
            (setq id-count (+ id-count (minaduki-db//insert-ids)))
            (setq tag-count (+ tag-count (minaduki-db//insert-tags)))
            (setq title-count (+ title-count (minaduki-db//insert-titles)))
            (setq lit-count (+ lit-count (minaduki-db//insert-lit-entries))))
        (file-error
         (setq error-count (1+ error-count))
         (minaduki-db//clear-file file)
         (minaduki//warn
          :warning
          "Skipping unreadable file while building cache: %s" file))))
    ;; Process links and ref / cite links
    (minaduki//for "Processing links (%s/%s)..."
        (file . _) file-hash-pairs
      (condition-case nil
          (minaduki//with-temp-buffer file
            (setq modified-count (1+ modified-count))
            (setq ref-count (+ ref-count (minaduki-db//insert-refs)))
            (setq link-count (+ link-count (minaduki-db//insert-links))))
        (file-error
         (setq error-count (1+ error-count))
         (minaduki-db//clear-file file)
         (minaduki//warn
          :warning
          "Skipping unreadable file while building cache: %s" file))))

    (list :error-count error-count
          :modified-count modified-count
          :id-count id-count
          :title-count title-count
          :tag-count tag-count
          :link-count link-count
          :ref-count ref-count
          :lit-count lit-count)))

(defun minaduki-db/update ()
  "Update the database."
  (pcase minaduki-db/update-method
    ('immediate
     (minaduki-db/update-file (buffer-file-name (buffer-base-buffer))))
    ('idle-timer
     (minaduki-db/mark-dirty))
    (_
     (user-error "Invalid `minaduki-db/update-method'"))))

(provide 'minaduki-db)

;;; minaduki-db.el ends here
