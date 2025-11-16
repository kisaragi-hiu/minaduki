;;; minaduki-db.el --- Cache database -*- lexical-binding: t -*-

;;; Commentary:

;; This is loosely based on `org-roam-db', rewritten to use Emacs 29's SQLite
;; support directly.

;;; Code:

(require 'dash)
(require 'sqlite)

(require 'minaduki-vars)
(require 'minaduki-extract)
(require 'minaduki-utils)

(defconst minaduki-db::version 19)
(defconst minaduki-db::table-schemata
  '((files
     "\"file\" UNIQUE PRIMARY KEY"
     "\"hash\" NOT NULL"
     "\"meta\" NOT NULL"
     "\"tags\""
     "\"titles\"")
    (ids
     "\"id\" UNIQUE PRIMARY KEY"
     "\"file\" NOT NULL REFERENCES files(\"file\") ON DELETE CASCADE"
     "\"point\" INTEGER NOT NULL"
     "\"level\" INTEGER NOT NULL"
     "\"title\"")

    (links
     "\"source\" NOT NULL REFERENCES files(\"file\") ON DELETE CASCADE"
     "\"dest\""
     "\"type\" NOT NULL"
     "\"props\" NOT NULL")

    ;; TODO: "keys" maps keys to literature entry listings; "refs"
    ;; maps keys to note files. These should be in the same table.
    (keys
     "\"key\" UNIQUE NOT NULL"
     "\"file\" NOT NULL REFERENCES files(\"file\") ON DELETE CASCADE"
     "\"point\" INTEGER NOT NULL"
     "\"props\" NOT NULL")

    (refs
     "\"ref\" UNIQUE NOT NULL"
     "\"file\" NOT NULL REFERENCES files(\"file\") ON DELETE CASCADE"
     "\"type\" NOT NULL")))
(defvar minaduki-db::connection nil
  "The \"connection\" to the cache database.
This is the value passed to `sqlite-execute' and the like.
Use the function `minaduki-db' to access this value.")

(defvar minaduki-db::stale t
  "Whether to consider the db stale.
When this is non-nil, `minaduki-db' automatically updates the cache.")

;; Timer-based updating
;; The timer is hooked up in minaduki-mode.el
;; Current name is `minaduki-db/file-update-timer'
(defvar minaduki-db::file-update-dirty nil
  "Whether the database needs to be updated by the timer.")
(defvar minaduki-db::file-update-timer nil
  "Timer for updating the database when dirty.")
(defun minaduki-db::file-update-timer::update-cache ()
  "Update the cache if the database is dirty."
  (when minaduki-db::file-update-dirty
    (minaduki-db:build-cache))
  (setq minaduki-db::file-update-dirty nil))
(defun minaduki-db::file-update-timer::mark-dirty ()
  "Mark the database as dirty for timer-based updating."
  (setq minaduki-db::file-update-dirty t))

(defun minaduki-db::init-and-migrate ()
  "Initialize and migrate the database."
  (let ((should-init (not (file-exists-p minaduki:db-location)))
        db)
    (when should-init
      (make-directory (file-name-directory minaduki:db-location) t))
    (setq db (sqlite-open minaduki:db-location))
    ;; Setting the global variable early allows us to use `minaduki-db-execute'
    ;; etc. during migration without fear for recursive initialization.
    (setq minaduki-db::connection db)
    (when should-init
      (sqlite-pragma db "foreign_keys = 1")
      (sqlite-pragma db (format "user_version = %s" minaduki-db::version))
      (with-sqlite-transaction db
        (pcase-dolist (`(,tbl . ,schemata) minaduki-db::table-schemata)
          (sqlite-execute
           db
           (format "CREATE TABLE \"%s\" (%s);"
                   tbl
                   (string-join schemata ","))))))
    (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
      (cond
       ((> version minaduki-db::version)
        (minaduki-db::close)
        (user-error
         "The cache database was created with a newer Minaduki version. Please update Minaduki"))
       ((< version minaduki-db::version)
        (message "Migrating the cache database from version %d to version %d"
                 version minaduki-db::version)
        ;; Fallback case: rebuild everything
        (minaduki-db:build-cache t))))
    minaduki-db::connection))
(defun minaduki-db::close ()
  "Close the connection."
  ;; There doesn't appear to be a way to distinguish between a closed db and an
  ;; open db other than "try selecting and see what happens", so it seems better
  ;; to just remove the reference (so we don't have to worry about accessing a
  ;; closed db) and let Emacs automatically close it.
  (setq minaduki-db::connection nil))
(defun minaduki-db ()
  "Return the \"connection\" to the cache database.
Performs initialization and migration when required."
  (unless (sqlitep minaduki-db::connection)
    (minaduki-db::init-and-migrate))
  (when minaduki-db::stale
    ;; build-cache sets stale to nil in its first steps
    (minaduki-db:build-cache nil minaduki-db-skip-initial-modification-check))
  minaduki-db::connection)

;; Like `emacsql-escape-scalar'
(defun minaduki-db::escape-value (value)
  "Escape VALUE for insertion into the database.
Strings are stored as raw strings, nil is stored as NULL, and
other values (including collections) are stored as JSON."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t))
    (cond ((null value) "NULL")
          ((stringp value) (->> value
                                (replace-regexp-in-string (rx "'") "''")
                                (format "'%s'")))
          (t (minaduki-db::escape-value
              (json-serialize
               value
               :false-object nil))))))
(defun minaduki-db::parse-value (str)
  "Parse stored STR into a value."
  (when str
    (json-parse-string
     str
     :false-object nil :array-type 'list)))

(defun minaduki-db-insert (table values &optional mode)
  "Insert VALUES into TABLE.
TABLE can be specified as a symbol.
VALUES is a sequence of rows. Each row is a sequence of values
for each column.
MODE can be used to override the insertion keyword. By default it
is \"INSERT\", but it can also be \"INSERT OR REPLACE\". No
checks are performed as to whether MODE is valid."
  (when (> (length values) 0)
    (minaduki-db-execute
     (format "%s into \"%s\" values %s"
             (or mode "insert")
             table
             (--> values
                  (seq-map (lambda (row)
                             (--> row
                                  (seq-map #'minaduki-db::escape-value it)
                                  (string-join it ",")
                                  (format "(%s)" it)))
                           it)
                  (string-join it ","))))))

(defun minaduki-db-execute (sql &rest args)
  "Run `sqlite-execute' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (sqlite-execute (minaduki-db) sql args))
(defun minaduki-db-select (sql &rest args)
  "Run `sqlite-select' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (sqlite-select (minaduki-db) sql args))

(defun minaduki-db::initialized? ()
  "Return whether the cache database has been initialized."
  (and (file-exists-p minaduki:db-location)
       (> (length (minaduki-db-select "select * from files limit 1"))
          0)))
(defun minaduki-db::ensure-built ()
  "Assert that the database has been initialized, and barf otherwise."
  (unless (minaduki-db::initialized?)
    (error "(minaduki) The cache database has not been built yet. Please run `minaduki-db:build-cache to build it")))

;; Clearing
(defun minaduki-db::clear-all ()
  "Clear all entries in the cache database."
  (when (file-exists-p minaduki:db-location)
    (dolist (table (map-keys minaduki-db::table-schemata))
      (minaduki-db-execute (format "delete from \"%s\"" table)))))
(defun minaduki-db::clear-file (file)
  "Clear information related to FILE.
File defaults to the current buffer\\='s file name."
  (unless file
    (setq file (buffer-file-name (buffer-base-buffer))))
  (with-sqlite-transaction (minaduki-db)
    ;; Rows in other tables referencing the file will also be deleted thanks to
    ;; ON DELETE CASCADE.
    (minaduki-db-execute "delete from files where file = ?" file)))

;; Inserting
(defun minaduki-db::insert-meta (&optional update-p hash)
  "Update the metadata of the current buffer into the cache.

If UPDATE-P is non-nil, first remove the meta for the file in the database.
If HASH is non-nil, assume that is the file's hash without recomputing it."
  (let* ((file (minaduki::current-file-name))
         (attr (file-attributes file))
         (mtime (file-attribute-modification-time attr))
         (modified (minaduki-extract--modified))
         (hash (or hash (minaduki::compute-content-hash file t)))
         (tags (minaduki-extract/tags file))
         (titles (minaduki-extract/titles))
         (keys (minaduki-extract/refs)))
    (when update-p
      (minaduki-db-execute
       "delete from \"files\" where file = ?"
       file))
    (minaduki-db-insert
     'files
     (list (vector file
                   hash
                   (list :mtime (vconcat mtime)
                         :modified modified)
                   (vconcat tags)
                   (vconcat titles))))
    (let ((sources (cl-loop
                    for (type . key) in keys
                    when (equal type "website")
                    collect key)))
      (cl-loop
       for (type . key) in keys
       when (equal type "cite")
       do
       (unless (minaduki-db::fetch-lit-entry key)
         (minaduki-db-insert
          'keys
          (list (vector key
                        file
                        1
                        (minaduki-lit/entry
                         :type "file"
                         :title (elt titles 0)
                         :key key
                         :sources sources)))))))))
(defun minaduki-db::insert-note-lit-entries (&optional update-p)
  "Extract a lit entry from the current note, if any, and insert it into the cache.
If UPDATE-P is non-nil, first remove the entries from the file in the database."
  (cl-block nil
    (let ((file (or minaduki::file-name (buffer-file-name)))
          (count 0))
      ;; entries
      (-when-let (entry (minaduki-extract/note-lit-entry))
        ;; This must only happen if there is an entry found already.
        ;; Otherwise bibliographies would have all their entries cleared.
        ;; FIXME: this would still be triggered if you set "author" on a
        ;; bibliography file.
        (when update-p
          (minaduki-db-execute
           "delete from \"keys\" where file = ?"
           file))
        (cl-incf count)
        (minaduki-db-insert
         'keys
         (list (vector (gethash "key" entry)
                       file
                       1
                       entry))
         "insert"))
      count)))
(defun minaduki-db::insert-lit-entries (&optional update-p)
  "Update the lit-entries of the current bibliography buffer into the cache.
If UPDATE-P is non-nil, first remove the entries from the file in the database."
  (cl-block nil
    (let ((file (or minaduki::file-name (buffer-file-name)))
          (count 0))
      (when update-p
        (minaduki-db-execute
         "delete from \"keys\" where file = ?"
         file))
      ;; entries
      (-when-let (entries
                  (cl-loop for (point . entry) in (minaduki-extract/lit-entries)
                           collect
                           (progn
                             (cl-incf count)
                             (vector (gethash "key" entry)
                                     file
                                     point
                                     entry))))
        (condition-case nil
            (minaduki-db-insert
             'keys
             entries
             ;; Entries from bibliographies have priority. If there's an
             ;; existing entry (perhaps from insert-meta), just replace
             ;; it.
             "insert or replace")
          ;; Fall back to trying one by one to detect whose problem is it
          (error
           (cl-loop for (point . entry) in entries
                    do
                    (condition-case nil
                        (minaduki-db-insert
                         'keys
                         (list (vector (gethash "key" entry)
                                       file
                                       point
                                       entry))
                         "insert or replace")
                      (error (minaduki::warn
                                 :error
                               "Malformed entry. key: %s, point: %s, file: %s"
                               (gethash "key" entry)
                               point
                               file)))))))
      count)))
(defun minaduki-db::insert-refs (&optional update-p)
  "Insert the citekeys of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the ref for the file in the database."
  (let ((file (minaduki::current-file-name))
        (count 0))
    (when update-p
      (minaduki-db-execute
       "delete from \"refs\" where file = ?"
       file))
    (when-let ((refs (minaduki-extract/refs)))
      (let ((rows (cl-loop for (type . key) in refs
                           collect (vector key file type))))
        (condition-case nil
            (minaduki-db-insert 'refs rows)
          (error
           (minaduki::warn :error
             "Cannot insert citekeys declared in %s; skipping"
             file)))))
    count))
(defun minaduki-db::insert-links ()
  "Put links from the current buffer into the cache database.
Existing cached link entries from the current buffer are removed
Return the number of rows inserted."
  (let ((file (minaduki::current-file-name)))
    (minaduki-db-execute
     "delete from \"links\" where source = ?"
     file)
    (let ((links (minaduki-extract/links)))
      (minaduki-db-insert 'links links)
      (length links))))
(defun minaduki-db::insert-ids (&optional update-p)
  "Update the ids of the current buffer into the cache.
If UPDATE-P is non-nil, first remove ids for the file in the database.
Returns the number of rows inserted."
  (let ((file (minaduki::current-file-name)))
    (when update-p
      (minaduki-db-execute
       "delete from \"ids\" where file = ?"
       file))
    (if-let ((ids (-some->> (minaduki-extract/ids file)
                    (--map (minaduki::object-to-vector it)))))
        (condition-case nil
            (progn
              (minaduki-db-insert 'ids ids)
              (length ids))
          (error
           (minaduki::warn :error
             "Duplicate IDs in %s, one of:\n\n%s\n\nskipping..."
             (aref (car ids) 1)
             (string-join (mapcar (lambda (hl)
                                    (aref hl 0)) ids) "\n"))
           0))
      0)))

;; Fetching
(defun minaduki-db::file-present? (file)
  "Does FILE exist in the cache DB?"
  (> (length (caar (minaduki-db-select
                    "select * from files where file = ? limit 1"
                    file)))
     0))
(cl-defun minaduki-db::fetch-file (&key title key id nocase?)
  "Return files from the DB.

When NOCASE? is non-nil, match case-insentively.

- ID: return the file containing a headline with ID.
- TITLE: return files with TITLE
- KEY: return the file associated with KEY."
  (let ((maybe-nocase (if nocase? "collate nocase" "")))
    (cond
     (id
      (caar (minaduki-db-select
             (format "select file from ids where id = ? limit 1 %s"
                     maybe-nocase)
             id)))
     (title
      ;; Narrow down possible candidates with SQLite first before
      ;; finding accurate matches in Emacs Lisp; first done in
      ;; `minaduki-db//fetch-tag-references'.
      (let ((possible (minaduki-db-select
                       (format (string-join
                                '("SELECT file, titles FROM files"
                                  "WHERE titles LIKE '%s'"
                                  "%s")
                                "\n")
                               (concat "%" title "%")
                               maybe-nocase)))
            files)
        (pcase-dolist (`(,file ,titles) possible)
          (let ((titles (minaduki-db::parse-value titles)))
            (when (if nocase?
                      (member (downcase title)
                              (mapcar #'downcase titles))
                    (member title titles))
              (push file files))))
        files))
     (key
      (caar (minaduki-db-select
             (format "select file from refs where ref = ? %s"
                     maybe-nocase)
             key))))))
(defun minaduki-db::fetch-id (id)
  "Return a `minaduki-id' object for ID."
  (let ((row (car (minaduki-db-select
                   '("SELECT id, file, point, level, title FROM ids"
                     "WHERE id = ?")
                   id))))
    (when row
      (apply #'record 'minaduki-id row))))
(defun minaduki-db::has-id? (id)
  "Return whether ID is in the cache."
  (not (not (minaduki-db-select '("SELECT id FROM ids WHERE id = ?") id))))

(defun minaduki-db::fetch-lit-entry (key)
  "Return a `minaduki-lit-entry' object for KEY."
  (let ((row (car (minaduki-db-select
                   '("select key, file, point, props from keys"
                     "where key = ?")
                   key))))
    (when row
      (-let (((key file point props) row))
        (record 'minaduki-lit-entry
                key file point (minaduki-db::parse-value props))))))
(defun minaduki-db::fetch-lit-authors ()
  "Fetch all authors in literature entries."
  ;; This approach is the second fastest out of five approaches I've tested,
  ;; The 5 variants are: (total time for 100 runs w/o GC in parens)
  ;; - this one (1.703s)
  ;; - remove the filter and use LIKE '#s(hash-table%author%' (1.714s)
  ;; - remove the filter and use LIKE '#s(hash-table%author%)' (1.75s)
  ;; - remove the filter and the WHERE entirely (2.18s)
  ;; - just remove the filter (1.675s) (faster but less safe)
  (-some->> (minaduki-db-select
             '("select props from keys"
               "where props like '%author%'"))
    (--map (-> (car it)
               minaduki-db::parse-value
               (map-elt "author")))
    -uniq
    (remq nil)))
(defun minaduki-db::fetch-all-files-hash ()
  "Return ((path . content-hash) ...) for all cached files as a hash-table."
  (let* ((current-files (minaduki-db-select "select file, hash from files"))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))
(defun minaduki-db::fetch-title (file)
  "Return the main title of FILE from the cache."
  (-> (minaduki-db-select '("select titles from files"
                             "where file = ?")
                           file)
      caar
      minaduki-db::parse-value
      car))
(defun minaduki-db::fetch-all-tags ()
  "Return all distinct tags from the cache."
  (let ((rows (minaduki-db-select "select distinct tags from files"))
        (acc (make-hash-table :test #'equal)))
    (dolist (row rows)
      (dolist (tag (minaduki-db::parse-value (car row)))
        (unless (gethash tag acc)
          (puthash tag t acc))))
    (hash-table-keys acc)))
(defun minaduki-db::fetch-tag-references (tag)
  "Return files that are tagged with TAG."
  ;; We narrow the list as much as possible in SQLite with the
  ;; string match first, then do the accurate filtering in Emacs
  ;; Lisp afterwards.
  ;;
  ;; This seems to indeed be faster.
  (let ((candidates
         (minaduki-db-select
          `("SELECT file, tags FROM files"
            ,(format "WHERE tags LIKE '%s'"
                     (concat "%" tag "%"))))))
    (cl-loop for cand in candidates
             when (member tag (minaduki-db::parse-value (cadr cand)))
             collect (car cand))))
(defun minaduki-db::fetch-backlinks (targets)
  "Fetch backlinks to TARGETS from the cache.

TARGETS are strings that are either file paths or ref keys. They
correspond to the TO field in the cache DB."
  (unless (listp targets)
    (setq targets (list targets)))
  (let ((conditions
         ;; '("dest = 'target1'" "OR" "dest = 'target2'" "OR" ...)
         (--> targets
              (--map (format "dest = '%s'" it) it)
              (-interpose "OR" it))))
    (--map
     (-let (((source dest props) it))
       (list source dest (minaduki-db::parse-value props)))
     (minaduki-db-select
      `("SELECT DISTINCT source, dest, props FROM links"
        "WHERE" ,@conditions
        "ORDER BY source ASC")))))
(defun minaduki-db::fetch-file-hash (&optional file)
  "Fetch the hash of FILE as stored in the cache."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (caar (minaduki-db-select
         "SELECT hash FROM files WHERE file = ?"
         file)))

(defun minaduki-db::fetch-tags (file)
  "Return the tags of FILE."
  (-some->> (minaduki-db-select "select tags from files where file = ?" file)
    caar
    minaduki-db::parse-value))

(cl-defun minaduki-db--fetch-nodes (&key under-path)
  "Fetch all `minaduki-node' objects for completion.
If UNDER-PATH is non-nil, only return nodes that are under it."
  (let (file-nodes id-nodes rows)
    (setq file-nodes (minaduki-db-select
                      `("SELECT" ,(-> '("files.file"
                                        "files.titles"
                                        "files.tags"
                                        "files.meta"
                                        "refs.ref"
                                        "refs.type")
                                      (string-join ","))
                        "FROM files"
                        "LEFT JOIN refs ON refs.file = files.file"
                        ;; "||" is string concatenation; this is how you put a
                        ;; param into the LIKE value without SQL injection.
                        ;;
                        ;; This doesn't filter out enough but does do so faster
                        ;; than `f-descendant-of?' in Elisp.
                        "WHERE files.file LIKE ? || '%'")
                      (or under-path "")))
    (pcase-dolist (`(,file ,titles ,tags ,meta ,ref ,key-type) file-nodes)
      (dolist (title (minaduki-db::parse-value titles))
        (when (or (not under-path)
                  (minaduki--fast-path-descendant-of? file under-path))
          (push (minaduki-node :path file
                               :title title
                               :tags (minaduki-db::parse-value tags)
                               :meta (minaduki-db::parse-value meta)
                               :key ref
                               :key-type key-type)
                rows))))
    (setq id-nodes (minaduki-db-select
                    `("SELECT" ,(-> '("ids.id"
                                      "ids.title"
                                      "files.titles"
                                      "files.meta"
                                      "ids.file")
                                    (string-join ","))
                      "FROM ids"
                      "LEFT JOIN files ON files.file = ids.file"
                      ;; Same as above. This doesn't filter out enough but does
                      ;; do so faster than `f-descendant-of?' in Elisp.
                      "WHERE files.file LIKE ? || '%'")
                    (or under-path "")))
    (pcase-dolist (`(,id ,title ,file-titles ,meta ,file) id-nodes)
      (when (or (not under-path)
                (minaduki--fast-path-descendant-of? file under-path))
        (push (minaduki-node
               :id id
               :path file
               :title (format "%s/%s"
                              (car (minaduki-db::parse-value file-titles))
                              title)
               :tags nil
               :meta (minaduki-db::parse-value meta))
              rows)))
    (setq rows (sort rows
                     (lambda (a b)
                       (time-less-p
                        (plist-get (oref a meta) :mtime)
                        (plist-get (oref b meta) :mtime)))))
    rows))
(defun minaduki-db::fetch-all-titles ()
  "Return all titles from the cache."
  (let ((rows (minaduki-db-select "select distinct titles from files"))
        (acc nil))
    (dolist (row rows)
      (dolist (title (minaduki-db::parse-value (car row)))
        (push title acc)))
    (-uniq acc)))

;; Updating
(cl-defstruct (minaduki-db::count
               (:copier nil)
               (:constructor minaduki-db::count))
  err modified id link ref lit)

(defun minaduki-db:build-cache::find-modified-files (files db-files skip)
  "Find modified files among FILES by comparing their hashes with DB-FILES.

FILES is a list of file names.
DB-FILES is a hash table, with keys being file names and values
being the corresponding hash value for the file.
If SKIP is non-nil, skip modification check as if nothing was modified.

DB-FILES is modified in place.

Return a list of two items:
- the first item is the modified files, as a hash table shaped like DB-FILES;
- the second item is DB-FILES, containing just the unmodified files."
  (let ((modified-files (make-hash-table :test #'equal)))
    (if skip
        (minaduki::message "Modification check skipped")
      (dolist-with-progress-reporter (file files)
          "(minaduki) Finding modified files"
        (let ((content-hash (minaduki::compute-content-hash file t)))
          (unless (string= content-hash (gethash file db-files))
            (puthash file content-hash modified-files)))
        (remhash file db-files)))
    (list modified-files db-files)))
(defalias 'minaduki-db:refresh 'minaduki-db:build-cache)
(defun minaduki-db:build-cache (&optional force skip-modification-check)
  "Build the cache for all applicable notes.
If FORCE, force a rebuild of the cache from scratch.
If SKIP-MODIFICATION-CHECK, treat all files as unmodified and don't
bother computing new hashes.

Interactively, a \\[universal-argument] stands for FORCE, and
modification check is never skipped. Ideally you should never need to
run this directly, but in cases when you do have the need,
SKIP-MODIFICATION-CHECK would be actively harmful if it is on in some
way."
  (interactive "P")
  (when force (delete-file minaduki:db-location))
  ;; Force a reconnect
  (setq minaduki-db::connection nil)
  ;; This must come before other calls to `minaduki-db' otherwise we've just
  ;; created an infinite loop.
  (setq minaduki-db::stale nil)
  ;; Initialize the database if necessary
  (minaduki-db)
  (let* ((gc-cons-threshold minaduki-db/gc-threshold)
         (org-agenda-files nil)
         (minaduki-extract::file-prop::use-cache t)
         (deleted-count 0)
         dir-files db-files counts modified-files)
    (setq dir-files (minaduki-vault-all-files)
          db-files (minaduki-db::fetch-all-files-hash))
    (setq modified-files
          (car (minaduki-db:build-cache::find-modified-files
                dir-files db-files skip-modification-check)))
    (unless skip-modification-check
      (minaduki::for "Removing deleted files from cache (%s/%s)"
          file (hash-table-keys db-files)
        ;; These files are no longer around, remove from cache...
        (minaduki-db::clear-file file)
        (cl-incf deleted-count)))
    (setq counts (minaduki-db::update-files modified-files force))
    (pcase-let (((cl-struct minaduki-db::count
                            (modified modified-count)
                            (err error-count))
                 counts))
      (cond ((> error-count 0)
             (minaduki::message
              "Updated cache for %s file(s). There are %s errors, please check *Warnings*"
              modified-count
              error-count))
            ((= modified-count 0)
             (minaduki::message "Cache is up to date"))
            (t
             (minaduki::message "Updated cache for %s file(s)" modified-count))))))
(defun minaduki-db:update-file (&optional file-path)
  "Update cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path
                      (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (minaduki::compute-content-hash file-path))
        (db-hash (minaduki-db::fetch-file-hash file-path)))
    (unless (string= content-hash db-hash)
      (let ((files-table (make-hash-table :test #'equal)))
        (puthash file-path content-hash files-table)
        (minaduki-db::update-files files-table))
      (minaduki::message "Updated: %s" file-path))))
(defun minaduki-db::update-files (files-table &optional rebuild)
  "Update cache for files in FILES-TABLE.

FILES-TABLE is a hash table mapping file names to hash values.
REBUILD signals that the DB is empty right now and we should skip
clearning existing file entries.

Returns a `minaduki-db::count' object."
  (let ((files (hash-table-keys files-table))
        (len (hash-table-count files-table))
        (error-count 0)
        (id-count 0)
        (link-count 0)
        (ref-count 0)
        (lit-count 0)
        (modified-count 0))
    ;; Clear existing cache entries so that we can put in new versions
    (unless rebuild
      (dolist-with-progress-reporter (file files)
          "(minaduki) Clearing files"
        (minaduki-db::clear-file file)))
    ;; Process bibliographies first so that keys only present in files can
    ;; also be tracked.
    (minaduki::message "Processing bibliographies...")
    (--each (minaduki-lit:bibliography)
      (when (gethash it files-table)
        (condition-case nil
            (minaduki::with-temp-buffer it
              ;; We need the files to be in the files table first
              ;; before we can reference them
              (minaduki-db::insert-meta)
              (cl-incf lit-count (minaduki-db::insert-lit-entries t)))
          (error
           (cl-incf error-count)
           (minaduki-db::clear-file it)
           (minaduki::warn :warning "Skipping bibliography: %s" it)))))
    (minaduki::message "Processing bibliographies...done")
    ;; Process file metadata (titles, tags) first to allow links to
    ;; depend on titles later; process IDs first so IDs are already
    ;; cached during link extraction
    (let* ((bibliographies (minaduki-lit:bibliography))
           (i 1)
           (rep (make-progress-reporter "(minaduki) Processing file metadata" 1 len)))
      (->> files-table
           (maphash
            (lambda (file contents-hash)
              (progress-reporter-update rep i (format "(%s/%s)" i len))
              (cl-incf i)
              (let ((inhibit-message t))
                (condition-case e
                    (minaduki::with-temp-buffer file
                      (unless (member file bibliographies)
                        (minaduki-db::insert-meta nil contents-hash))
                      (setq id-count
                            (+ id-count
                               (minaduki-db::insert-ids t))))
                  (error
                   (setq error-count
                         (1+ error-count))
                   (minaduki-db::clear-file file)
                   (minaduki::warn :warning "Error processing metadata:\n%s"
                                   (list :file file :error e))))))))
      (progress-reporter-done rep))
    ;; Process links and ref / cite links
    (let ((i 1)
          (rep (make-progress-reporter "(minaduki) Processing links" 1 len)))
      (dolist (file files)
        (progress-reporter-update rep i (format "(%s/%s)" i len))
        (cl-incf i)
        (condition-case e
            (let ((inhibit-message t))
              (minaduki::with-temp-buffer file
                (setq modified-count (1+ modified-count))
                (setq lit-count (+ lit-count (minaduki-db::insert-note-lit-entries t)))
                (setq ref-count (+ ref-count (minaduki-db::insert-refs t)))
                (setq link-count (+ link-count (minaduki-db::insert-links)))))
          (error
           (setq error-count (1+ error-count))
           (minaduki-db::clear-file file)
           (minaduki::warn :warning
             "Error while processing links:\n%s"
             (list :file file :error e)))))
      (progress-reporter-done rep))
    (minaduki-db::count :err error-count
                        :modified modified-count
                        :id id-count
                        :link link-count
                        :ref ref-count
                        :lit lit-count)))
(defun minaduki-db::incremental-update ()
  "Update the database."
  (let ((inhibit-message save-silently))
    (pcase minaduki-db/update-method
      ('immediate
       (minaduki-db:update-file (buffer-file-name (buffer-base-buffer))))
      ('idle-timer
       (minaduki-db::file-update-timer::mark-dirty))
      (_
       (user-error "Invalid `minaduki-db/update-method'")))))

(provide 'minaduki-db)

;;; minaduki-db.el ends here
