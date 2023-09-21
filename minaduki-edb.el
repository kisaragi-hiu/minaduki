;;; minaduki-edb.el --- DB rewrite -*- lexical-binding: t -*-

;;; Commentary:

;; Will be a new implementation of the cache database for Minaduki.
;; The goal is to use Emacs 29's SQLite support directly.

;;; Code:

(require 'dash)
(require 'sqlite)

(require 'minaduki-extract)
(require 'minaduki-utils)

(defconst minaduki-edb::version 17)
(defconst minaduki-edb::table-schemata
  '((files
     "\"file\" UNIQUE PRIMARY KEY"
     "\"hash\" NOT NULL"
     "\"meta\" NOT NULL"
     "\"tags\""
     "\"titles\"")
    (ids
     "\"id\" UNIQUE PRIMARY KEY"
     "\"file\" NOT NULL"
     "\"point\" NOT NULL"
     "\"level\" NOT NULL"
     "\"title\"")

    (links
     "\"source\" NOT NULL"
     "\"dest\""
     "\"type\" NOT NULL"
     "\"properties\" NOT NULL")

    ;; TODO: "keys" maps keys to literature entry listings; "refs"
    ;; maps keys to note files. These should be in the same table.
    (keys
     "\"key\" UNIQUE NOT NULL"
     "\"file\" NOT NULL"
     "\"point\" NOT NULL"
     "\"props\" NOT NULL")

    (refs
     "\"ref\" UNIQUE NOT NULL"
     "\"file\" NOT NULL"
     "\"type\" NOT NULL")))
(defvar minaduki-edb::connection nil
  "The \"connection\" to the cache database.
This is the value passed to `sqlite-execute' and the like.
Use the function `minaduki-edb' to access this value.")
(defcustom minaduki:edb-location
  (expand-file-name "2minaduki.db" user-emacs-directory)
  "Full path to the experimental cache database.

All cache will be saved here regardless of which project a note
file might belong to, and there is no need to change this
per vault."
  :type 'string
  :group 'minaduki)

;; Timer-based updating
;; The timer is hooked up in minaduki-mode.el
;; Current name is `minaduki-db/file-update-timer'
(defvar minaduki-edb::file-update-dirty nil
  "Whether the database needs to be updated by the timer.")
(defvar minaduki-edb::file-update-timer nil
  "Timer for updating the database when dirty.")
(defun minaduki-edb::file-update-timer::update-cache ()
  "Update the cache if the database is dirty."
  (when minaduki-edb::file-update-dirty
    (minaduki-edb::build-cache))
  (setq minaduki-edb::file-update-dirty nil))
(defun minaduki-edb::file-update-timer::mark-dirty ()
  "Mark the database as dirty for timer-based updating."
  (setq minaduki-edb::file-update-dirty t))

(defun minaduki-edb::set-version (version)
  "Set the user_version of the database to VERSION."
  (unless (and (integerp version) (> version 0))
    (error "Invalid version, must be a positive integer"))
  (minaduki-edb-execute (format "PRAGMA user_version = %s" version)))
(defun minaduki-edb::init-and-migrate ()
  "Initialize and migrate the database."
  (let ((should-init (not (file-exists-p minaduki:edb-location)))
        db)
    (when should-init
      (make-directory (file-name-directory minaduki:edb-location) t))
    (setq db (sqlite-open minaduki:edb-location))
    ;; Setting the global variable early allows us to use `minaduki-edb-execute'
    ;; etc. during migration without fear for recursive initialization.
    (setq minaduki-edb::connection db)
    (when should-init
      (with-sqlite-transaction db
        (pcase-dolist (`(,tbl . ,schemata) minaduki-edb::table-schemata)
          (sqlite-execute
           db
           (format "CREATE TABLE \"%s\" (%s);"
                   tbl
                   (string-join schemata ","))))
        (sqlite-pragma db (format "user_version = %s" minaduki-edb::version))))
    (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
      (cond
       ((> version minaduki-edb::version)
        (sqlite-close db)
        (user-error
         "The cache database was created with a newer Minaduki version. "
         "Please update Minaduki"))
       ((< version minaduki-edb::version)
        (message "Migrating the cache database from version %d to version %d"
                 version minaduki-edb::version)
        ;; Fallback case: rebuild everything
        (minaduki-edb::build-cache t))))
    minaduki-edb::connection))
(defun minaduki-edb ()
  "Return the \"connection\" to the cache database.
Performs initialization and migration when required."
  (unless (sqlitep minaduki-edb::connection)
    (minaduki-edb::init-and-migrate))
  minaduki-edb::connection)

;; Like `emacsql-escape-scalar'
(defun minaduki-edb::escape-scalar (scalar)
  "Escape SCALAR for insertion into the database.
Strings are stored as raw strings, nil is stored as NULL, and
other values are stored as JSON."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t))
    (cond ((null scalar) "NULL")
          ((stringp scalar) (->> scalar
                                 (replace-regexp-in-string (rx "'") "''")
                                 (format "'%s'")))
          (t (minaduki-edb::escape-scalar
              (json-encode scalar))))))

(defun minaduki-edb-insert (table values &optional mode)
  "Insert VALUES into TABLE.
TABLE can be specified as a symbol.
VALUES is a sequence of rows. Each row is a sequence of values
for each column.
MODE can be used to override the insertion keyword. By default it
is \"INSERT\", but it can also be \"INSERT OR REPLACE\". No
checks are performed as to whether MODE is valid."
  (minaduki-edb-execute
   (format "%s into \"%s\" values %s"
           (or mode "insert")
           table
           (--> values
                (seq-map (lambda (row)
                           (--> row
                                (seq-map #'minaduki-edb::escape-scalar it)
                                (string-join it ",")
                                (format "(%s)" it)))
                         it)
                (string-join it ",")))))

(defun minaduki-edb-execute (sql &rest args)
  "Run `sqlite-execute' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (sqlite-execute (minaduki-edb) sql args))
(defun minaduki-edb-select (sql &rest args)
  "Run `sqlite-select' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (sqlite-select (minaduki-edb) sql args))

(defun minaduki-edb::initialized? ()
  "Return whether the cache database has been initialized."
  (and (file-exists-p minaduki:edb-location)
       (> (length (minaduki-edb-select "select * from files limit 1"))
          0)))
(defun minaduki-edb::ensure-built ()
  "Assert that the database has been initialized, and barf otherwise."
  (unless (minaduki-edb::initialized?)
    (error "(minaduki) The cache database has not been built yet. Please run `minaduki-edb::build-cache to build it")))

;; Clearing
(defun minaduki-edb::clear-all ()
  "Clear all entries in the cache database."
  (when (file-exists-p minaduki:edb-location)
    (dolist (table (map-keys minaduki-edb::table-schemata))
      (minaduki-edb-execute (format "delete from \"%s\"" table)))))
(defun minaduki-edb::clear-file (file)
  "Clear information related to FILE.
File defaults to the current buffer\\='s file name."
  (unless file
    (setq file (buffer-file-name (buffer-base-buffer))))
  (with-sqlite-transaction (minaduki-edb)
    (dolist (table (map-keys minaduki-edb::table-schemata))
      (let ((col (if (eq table 'links)
                     "source"
                   "file")))
        (minaduki-edb-execute
         (format "delete from \"%s\" where \"%s\" = ?" table col)
         file)))))
(defun minaduki-edb::clear-files (pattern)
  "Clear information coming from files matching PATTERN.
PATTERN is a LIKE pattern."
  (with-sqlite-transaction (minaduki-edb)
    (dolist (table (map-keys minaduki-edb::table-schemata))
      (let ((col (if (eq table 'links)
                     "source"
                   "file")))
        (minaduki-edb-execute
         (format "delete from \"%s\" where \"%s\" LIKE ?" table col)
         pattern)))))

;; Inserting
(defun minaduki-edb::insert-meta (&optional update-p hash)
  "Update the metadata of the current buffer into the cache.

If UPDATE-P is non-nil, first remove the meta for the file in the database.
If HASH is non-nil, assume that is the file's hash without recomputing it."
  (let* ((file (minaduki::current-file-name))
         (attr (file-attributes file))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr))
         (hash (or hash (minaduki::compute-content-hash file)))
         (tags (minaduki-extract/tags file))
         (titles (minaduki-extract/titles))
         (keys (minaduki-extract/refs)))
    (when update-p
      (minaduki-edb-execute
       "delete from \"files\" where file = ?"
       file))
    (minaduki-edb-insert
     'files
     (list (vector file hash (list :atime atime :mtime mtime)
                   tags titles)))
    (let ((sources (cl-loop
                    for (type . key) in keys
                    when (equal type "website")
                    collect key)))
      (cl-loop
       for (type . key) in keys
       when (equal type "cite")
       do
       (unless (minaduki-edb::fetch-lit-entry key)
         (minaduki-edb-insert
          'keys
          (list (vector key
                        file
                        1
                        (minaduki-lit/entry
                         :type "file"
                         :title (elt titles 0)
                         :key key
                         :sources sources)))))))))
(defun minaduki-edb::insert-lit-entries (&optional update-p)
  "Update the lit-entries of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the entries from the file in the database."
  (cl-block nil
    (let ((file (or minaduki::file-name (buffer-file-name)))
          (count 0))
      (when update-p
        (minaduki-edb-execute
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
        (minaduki-edb-insert
         'keys
         entries
         ;; Entries from bibliographies have priority. If there's an
         ;; existing entry (perhaps from insert-meta), just replace
         ;; it.
         "insert or replace"))
      count)))
(defun minaduki-edb::insert-refs (&optional update-p)
  "Insert the citekeys of the current buffer into the cache.
If UPDATE-P is non-nil, first remove the ref for the file in the database."
  (let ((file (minaduki::current-file-name))
        (count 0))
    (when update-p
      (minaduki-edb-execute
       "delete from \"refs\" where file = ?"
       file))
    (when-let ((refs (minaduki-extract/refs)))
      (dolist (ref refs)
        (let ((key (cdr ref))
              (type (car ref)))
          (condition-case nil
              (progn
                (minaduki-edb-insert 'refs (list (vector key file type)))
                (cl-incf count))
            (error
             (minaduki::warn :error
               "Duplicate ref %s in:\n\nA: %s\nB: %s\n\nskipping..."
               key
               file
               (caar (minaduki-edb-select
                      "select file from refs where ref = ?"
                      key))))))))
    count))
(defun minaduki-edb::insert-links (&optional update-p)
  "Update the file links of the current buffer in the cache.
If UPDATE-P is non-nil, first remove the links for the file in the database.
Return the number of rows inserted."
  (let ((file (minaduki::current-file-name)))
    (when update-p
      (minaduki-edb-execute
       "delete from \"links\" where source = ?"
       file))
    (if-let ((links (minaduki-extract/links)))
        (progn
          (minaduki-edb-insert 'links links)
          (length links))
      0)))
(defun minaduki-edb::insert-ids (&optional update-p)
  "Update the ids of the current buffer into the cache.
If UPDATE-P is non-nil, first remove ids for the file in the database.
Returns the number of rows inserted."
  (let ((file (minaduki::current-file-name)))
    (when update-p
      (minaduki-edb-execute
       "delete from \"ids\" where file = ?"
       file))
    (if-let ((ids (-some->> (minaduki-extract/ids file)
                    (--map (minaduki::object-to-vector it)))))
        (condition-case nil
            (progn
              (minaduki-edb-insert 'ids ids)
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
(defun minaduki-edb::file-present? (file)
  "Does FILE exist in the cache DB?"
  (> (length (caar (minaduki-edb-select
                    "select * from files where file = ? limit 1"
                    file)))
     0))
(cl-defun minaduki-edb::fetch-file (&key title key id nocase?)
  "Return files from the DB.

When NOCASE? is non-nil, match case-insentively.

- ID: return the file containing a headline with ID.
- TITLE: return files with TITLE
- KEY: return the file associated with KEY."
  (let ((maybe-nocase (if nocase? "collate nocase" "")))
    (cond
     (id
      (caar (minaduki-edb-select
             (format "select file from ids where id = ? limit 1 %s"
                     maybe-nocase)
             id)))
     (title
      ;; Narrow down possible candidates with SQLite first before
      ;; finding accurate matches in Emacs Lisp; first done in
      ;; `minaduki-db//fetch-tag-references'.
      (let ((possible (minaduki-edb-select
                       (format (string-join
                                '("SELECT file, titles FROM files"
                                  "WHERE titles LIKE %s"
                                  "%s")
                                "\n")
                               (concat "%" title "%")
                               maybe-nocase))))
        (cl-loop for (file titles) in possible
                 when (if nocase?
                          (member (downcase title)
                                  (mapcar #'downcase titles))
                        (member title titles))
                 collect file)))
     (key
      (caar (minaduki-edb-select
             (format "select file from refs where ref = ? %s"
                     maybe-nocase)
             key))))))
(defun minaduki-edb::fetch-id (id)
  "Fetch ID from the DB."
  (-some->> (minaduki-edb-select
             '("SELECT id, file, point, level, title FROM ids"
               "WHERE id = ?")
             id)
    car
    (apply #'record 'minaduki-id)))
(defun minaduki-edb::fetch-lit-entry (key)
  "Fetch the literature entry with KEY in the DB."
  (-some->> (minaduki-edb-select
             '("select key, file, point, props from keys"
               "where key = ?")
             key)
    car
    (apply #'record 'minaduki-lit-entry)))
(defun minaduki-edb::fetch-lit-authors ()
  "Fetch all authors in literature entries."
  ;; This approach is the second fastest out of five approaches I've tested,
  ;; The 5 variants are: (total time for 100 runs w/o GC in parens)
  ;; - this one (1.703s)
  ;; - remove the filter and use LIKE '#s(hash-table%author%' (1.714s)
  ;; - remove the filter and use LIKE '#s(hash-table%author%)' (1.75s)
  ;; - remove the filter and the WHERE entirely (2.18s)
  ;; - just remove the filter (1.675s) (faster but less safe)
  (-some->> (minaduki-edb-select
             '("select props from keys"
               "where props like '%author%'"))
    (-map #'car)
    (-filter #'hash-table-p)
    (--map (map-elt it "author"))
    -uniq
    (remq nil)))
(defun minaduki-edb::fetch-all-files-hash ()
  "Return ((path . content-hash) ...) for all cached files as a hash-table."
  (let* ((current-files (minaduki-edb-select "select file, hash from files"))
         (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))
(defun minaduki-edb::fetch-title (file)
  "Return the main title of FILE from the cache."
  (car
   (caar
    (minaduki-edb-select '("select titles from files"
                           "where file = ?")
                         file))))
(defun minaduki-edb::fetch-all-tags ()
  "Return all distinct tags from the cache."
  (let ((rows (minaduki-edb-select "select distinct tags from files"))
        acc)
    (dolist (row rows)
      (dolist (tag (car row))
        (unless (member tag acc)
          (push tag acc))))
    acc))
(defun minaduki-edb::fetch-tag-references (tag)
  "Return files that are tagged with TAG."
  ;; We narrow the list as much as possible in SQLite with the
  ;; string match first, then do the accurate filtering in Emacs
  ;; Lisp afterwards.
  ;;
  ;; This seems to indeed be faster.
  (let ((candidates
         (minaduki-edb-select
          `("SELECT file, tags FROM files"
            ,(format "WHERE tags LIKE '%s'"
                     (concat "%" tag "%"))))))
    (cl-loop for cand in candidates
             when (member tag (cadr cand))
             collect (car cand))))
(defun minaduki-edb::fetch-backlinks (targets)
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
    (minaduki-edb-select
     `("SELECT source, dest, properties FROM links"
       "WHERE" ,@conditions
       "ORDER BY source ASC"))))
(defun minaduki-edb::fetch-file-hash (&optional file)
  "Fetch the hash of FILE as stored in the cache."
  (setq file (or file (buffer-file-name (buffer-base-buffer))))
  (caar (minaduki-edb-select
         "SELECT hash FROM files WHERE file = ?"
         file)))

;; Updating
(defun minaduki-edb::update-file (&optional file-path)
  "Update cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (setq file-path (or file-path
                      (buffer-file-name (buffer-base-buffer))))
  (if (not (file-exists-p file-path))
      (minaduki-edb::clear-file file-path)
    ;; save the file before performing a database update
    (when-let ((buf (find-buffer-visiting file-path)))
      (with-current-buffer buf
        (save-buffer)))
    (minaduki::with-temp-buffer file-path
      (with-sqlite-transaction (minaduki-edb)
        (when (member file-path (minaduki-lit:bibliography))
          (minaduki-edb::insert-lit-entries 'update))
        (unless (eq major-mode 'bibtex-mode)
          (minaduki-edb::insert-meta 'update)
          (minaduki-edb::insert-refs 'update)
          (minaduki-edb::insert-ids 'update)
          (minaduki-edb::insert-links 'update))))))
(defun minaduki-edb::build-cache (&optional force)
  "Build the cache for all applicable.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (when force (delete-file minaduki/db-location))
  ;; Force a reconnect
  (setq minaduki-edb::connection nil)
  ;; Initialize the database if necessary
  (minaduki-edb)
  (let* ((gc-cons-threshold minaduki-db/gc-threshold)
         (org-agenda-files nil)
         (minaduki-extract::file-prop::use-cache t)
         (deleted-count 0)
         dir-files db-files count-plist modified-files)
    (setq dir-files (minaduki-vault:all-files)
          db-files (minaduki-edb::fetch-all-files-hash))
    (dolist-with-progress-reporter (file dir-files)
        "(minaduki) Finding modified files"
      (let ((content-hash (minaduki::compute-content-hash file)))
        (unless (string= content-hash (gethash file db-files))
          (push (cons file content-hash) modified-files)))
      (remhash file db-files))
    (minaduki::for "Removing deleted files from cache (%s/%s)"
        file (hash-table-keys db-files)
      ;; These files are no longer around, remove from cache...
      (minaduki-edb::clear-file file)
      (setq deleted-count (1+ deleted-count)))
    (setq count-plist (minaduki-edb::update-files modified-files force))
    (let* ((error-count (plist-get count-plist :error-count)))
      (if (> error-count 0)
          (minaduki::message
           "Updated cache for %s file(s). There are %s errors, please check *Warnings*"
           (hash-table-count db-files)
           error-count)
        (minaduki::message
         "Updated cache for %s file(s)"
         (hash-table-count db-files))))))
;; TODO: what's the difference between this and the private version?
(defun minaduki-edb:update-file (file-path)
  "Update cache for FILE-PATH.
If the file does not exist anymore, remove it from the cache.
If the file exists, update the cache with information."
  (let ((content-hash (minaduki::compute-content-hash file-path))
        (db-hash (minaduki-edb::fetch-file-hash file-path)))
    (unless (string= content-hash db-hash)
      (minaduki-edb::update-files (list (cons file-path content-hash)))
      (minaduki::message "Updated: %s" file-path))))
(defun minaduki-edb::update-files (file-hash-pairs &optional rebuild)
  "Update Org-roam cache for FILE-HASH-PAIRS.

FILE-HASH-PAIRS is a list of (file . hash) pairs.
REBUILD signals that the DB is empty right now and we should skip
clearning existing file entries."
  (let* ((gc-cons-threshold minaduki-db/gc-threshold)
         (org-agenda-files nil)
         (error-count 0)
         (id-count 0)
         (link-count 0)
         (ref-count 0)
         (lit-count 0)
         (modified-count 0))
    ;; Clear existing cache entries so that we can put in new versions
    (unless rebuild
      (minaduki::for "Clearing files (%s/%s)..."
          (file . _) file-hash-pairs
        (minaduki-edb::clear-file file)))
    ;; Process bibliographies first so that keys only in files can
    ;; also be tracked.
    (minaduki::message "Processing bibliographies...")
    (cl-loop
     for (file . _) in file-hash-pairs
     when (member file (minaduki-lit:bibliography))
     do (condition-case nil
            (minaduki::with-temp-buffer file
              (cl-incf lit-count (minaduki-edb::insert-lit-entries)))
          (error
           (cl-incf error-count)
           (minaduki-edb::clear-file file)
           (minaduki::warn :warning "Skipping bibliography: %s" file))))
    (minaduki::message "Processing bibliographies...done")
    ;; Process file metadata (titles, tags) first to allow links to
    ;; depend on titles later; process IDs first so IDs are already
    ;; cached during link extraction
    (minaduki::for "Processing file metadata (%s/%s)..."
        (file . contents-hash) file-hash-pairs
      (condition-case e
          (minaduki::with-temp-buffer file
            (minaduki-edb::insert-meta nil contents-hash)
            (setq id-count (+ id-count (minaduki-edb::insert-ids))))
        (error
         (setq error-count (1+ error-count))
         (minaduki-edb::clear-file file)
         (minaduki::warn
             :warning
           "Error processing metadata:
%s"
           (list :file file
                 :error e)))))
    ;; Process links and ref / cite links
    (minaduki::for "Processing links (%s/%s)..."
        (file . _) file-hash-pairs
      (condition-case nil
          (minaduki::with-temp-buffer file
            (setq modified-count (1+ modified-count))
            (setq ref-count (+ ref-count (minaduki-edb::insert-refs)))
            (setq link-count (+ link-count (minaduki-edb::insert-links))))
        (error
         (setq error-count (1+ error-count))
         (minaduki-edb::clear-file file)
         (minaduki::warn
             :warning
           "Skipping unreadable file while building cache: %s" file))))

    (list :error-count error-count
          :modified-count modified-count
          :id-count id-count
          :link-count link-count
          :ref-count ref-count
          :lit-count lit-count)))
(defun minaduki-edb:update ()
  "Update the database."
  (pcase minaduki-db/update-method
    ('immediate
     (minaduki-edb:update-file (buffer-file-name (buffer-base-buffer))))
    ('idle-timer
     (minaduki-edb::file-update-timer::mark-dirty))
    (_
     (user-error "Invalid `minaduki-db/update-method'"))))

(provide 'minaduki-edb)

;;; minaduki-edb.el ends here
