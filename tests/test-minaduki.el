;;; test-minaduki.el --- Tests for Minaduki -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'parsebib) ; necessary for testing bibtex extraction support
(require 'minaduki)
(require 'seq)
(require 'dash)
(require 'ht)

(require 'markdown-mode)

(defun test-minaduki--abs-path (file-path)
  "Get absolute FILE-PATH from `org-directory'."
  (expand-file-name file-path org-directory))

(defvar test-repository (expand-file-name "tests/minaduki-files")
  "Directory containing minaduki test org files.")

(defvar test-lit-directory (expand-file-name "lit" test-repository)
  "Directory containing minaduki-lit test files.")

(defun test-minaduki--init ()
  "."
  (let ((original-dir test-repository)
        (new-dir (expand-file-name (make-temp-name "minaduki") temporary-file-directory))
        (minaduki-verbose nil))
    (copy-directory original-dir new-dir)
    (setq org-directory new-dir)
    (minaduki-mode)
    (minaduki-db/build-cache)))

(defun test-minaduki--teardown ()
  (minaduki-mode -1)
  (delete-file minaduki/db-location)
  (minaduki-db//close))

(defun test-equal-ht (a b)
  "Are A and B the same?

This also treats A and B as the same if they have hash table
members that should be equal."
  (or (equal a b)
      (cond ((and (consp a)
                  (consp b))
             (and (test-equal-ht (car a) (car b))
                  (test-equal-ht (cdr a) (cdr b))))
            ((and (ht? a)
                  (ht? b))
             (ht-equal? a b)))))

(buttercup-define-matcher-for-binary-function :to-equal/ht test-equal-ht)

(describe "minaduki::file-type"
  (it "checks file type"
    (expect (with-temp-buffer
              (setq major-mode 'org-mode)
              (minaduki::file-type))
            :to-be 'org)
    (expect (with-temp-buffer
              (gfm-mode)
              (minaduki::file-type))
            :to-be 'markdown)
    (expect (with-temp-buffer
              (setq major-mode 'markdown-mode)
              (minaduki::file-type))
            :to-be 'markdown)
    (expect (with-temp-buffer
              (setq major-mode 'bibtex-mode)
              (minaduki::file-type))
            :to-be 'bibtex)
    (expect (with-temp-buffer
              (setq major-mode 'json-mode)
              (minaduki::file-type))
            :to-be 'json)
    (expect (with-temp-buffer
              (setq buffer-file-name "hello.json"
                    major-mode 'text-mode)
              (prog1 (minaduki::file-type)
                (setq buffer-file-name nil)))
            :to-be 'json)))

(describe "minaduki-lit"
  (it "parses our own Org-based bibliography format"
    (expect
     ;; This must be in org mode and must have the file name (for the
     ;; default `type')
     (with-current-buffer (find-file-noselect
                           (expand-file-name
                            "entries.org"
                            test-lit-directory))
       (minaduki-lit/parse-entries))
     :to-equal/ht
     '((33 . #s(hash-table
                size 65 test equal rehash-size 1.5 rehash-threshold 0.8125
                data ("tags" ("voice")
                      "type" "entries"
                      "key" "hitomine2013"
                      "author" "大崎ひとみ"
                      "publisher" "t2library課外活動部"
                      "year" "2013"
                      "title" "あたらしい女声の教科書")))
       (202 . #s(hash-table
                 size 65 test equal rehash-size 1.5 rehash-threshold 0.8125
                 data ("sources" ("https://www.w3.org/People/Bos/DesignGuide/designguide.html")
                       "tags" ("webdev" "css" "html")
                       "type" "entries"
                       "key" "bertbos20030306"
                       "url" "https://www.w3.org/People/Bos/DesignGuide/designguide.html"
                       "author" "Bert Bos"
                       "date" "2003-03-06"
                       "title" "An essay on W3C's design principles")))
       (436 . #s(hash-table
                 size 65 test equal rehash-size 1.5 rehash-threshold 0.8125
                 data ("type" "entries"
                       "author" "シャノン"
                       "key" "https://www.nicovideo.jp/watch/sm36143274"
                       "title" "MV「ヨミクダリの灯」 / GUMI"))))))
  (it "parses CSL-JSON"
    (expect
     (with-temp-buffer
       (insert-file-contents (expand-file-name
                              "date_LocalizedDateFormats-zh-TW.json"
                              test-lit-directory))
       (minaduki-lit/parse-entries/csl-json))
     :to-equal/ht
     '((110 . #s(hash-table
                 size 65 test equal rehash-size 1.5
                 rehash-threshold 0.8125
                 data ("key" "ITEM-1"
                       "date" "1998-04-10"
                       "title" "BookA"))))))

  (it "parses a bibtex file"
    (expect
     (with-temp-buffer
       (insert-file-contents
        (expand-file-name "entries.bib" test-lit-directory))
       (minaduki-lit/parse-entries/bibtex))
     :to-equal/ht
     '((8 . #s(hash-table
               size 65 test equal rehash-size 1.5
               rehash-threshold 0.8125
               data ("author" "大崎ひとみ"
                     "type" "manual"
                     "title" "あたらしい女声の教科書"
                     "key" "hitomine2013"
                     "tags" ["voice"]
                     "year" "2013"
                     "publisher" "t2library課外活動部")))
       (160 .
            #s(hash-table
               size 65 test equal rehash-size 1.5
               rehash-threshold 0.8125
               data ("author" "Bert Bos"
                     "type" "manual"
                     "title" "An essay on W3C's design principles"
                     "key" "bertbos20030306"
                     "sources"
                     ["https://www.w3.org/People/Bos/DesignGuide/designguide.html"]
                     "tags" ["webdev" "css" "html"]
                     "date" "2003-03-06")))))))

(describe "minaduki/format-link"
  (it "formats Org links"
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki/format-link :target "file:///tmp/abc.org"))
     :to-equal
     "[[/tmp/abc.org]]")
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki/format-link :target "file:///tmp/abc.org"
                             :desc "ABC"))
     :to-equal
     "[[/tmp/abc.org][ABC]]")
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki/format-link :target "https://kisaragi-hiu.com"
                             :desc "ABC"))
     :to-equal
     "[[https://kisaragi-hiu.com][ABC]]"))
  (it "formats Markdown links"
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'markdown-mode))
       (minaduki/format-link :target "https://kisaragi-hiu.com"
                             :desc "ABC"))
     :to-equal
     "[ABC](https://kisaragi-hiu.com)"))
  (it "formats local absolute links"
    (expect
     (with-temp-buffer
       (let ((minaduki:link-insertion-format 'absolute-in-vault)
             (minaduki/vaults '((:name "tmp" :path "/tmp/")))
             (default-directory "/"))
         (org-mode)
         (minaduki-local-mode) ; this applies `minaduki/vaults'
         (minaduki/format-link :target "file:///tmp/abc.org")))
     :to-equal
     "[[tmp:abc.org]]"))
  (it "formats relative links"
    (expect
     (let ((minaduki:link-insertion-format 'relative)
           (major-mode 'org-mode)
           (default-directory "/"))
       (minaduki/format-link :target "file:///tmp/abc.org"))
     :to-equal
     "[[tmp/abc.org]]")
    (expect
     (let ((minaduki:link-insertion-format 'relative)
           (major-mode 'markdown-mode)
           (default-directory "/"))
       (minaduki/format-link :target "/tmp/abc.md"))
     :to-equal
     "[abc.md](tmp/abc.md)")))

(describe "Utils"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

  (it "converts between calendar.el dates and YYYY-MM-DD date strings"
    (expect (minaduki//date/calendar.el->ymd '(7 17 2019))
            :to-equal
            "2019-07-17")
    (expect (minaduki//date/ymd->calendar.el "2012-01-02")
            :to-equal
            '(1 2 2012)))
  (it "converts a title to a slug"
    (expect (minaduki//title-to-slug "English")
            :to-equal "english")
    (expect (minaduki//title-to-slug "Text with space と漢字")
            :to-equal "text-with-space-と漢字")
    (expect (minaduki//title-to-slug "many____underscores")
            :to-equal "many-underscores")
    ;; Keep diacritics
    (expect (minaduki//title-to-slug "äöü")
            :to-equal "äöü")
    ;; Normalizes to composed from
    (expect (minaduki//title-to-slug (string ?て #x3099))
            :to-equal (string ?で))
    (expect (minaduki//title-to-slug "_starting and ending_")
            :to-equal "starting-and-ending")
    (expect (minaduki//title-to-slug "isn't alpha numeric")
            :to-equal "isn-t-alpha-numeric"))
  (describe "list-files"
    (it "using pure elisp"
      (expect
       (minaduki-vault::list-files/elisp (expand-file-name org-directory))
       :to-have-same-items-as
       (--map
        (test-minaduki--abs-path (f-relative it test-repository))
        (f-files test-repository nil t))))
    (when (executable-find "rg")
      (it "using rg"
        (expect
         (minaduki-vault::list-files/rg (executable-find "rg")
                                  (expand-file-name org-directory))
         :to-have-same-items-as
         (--map
          (test-minaduki--abs-path (f-relative it test-repository))
          (f-files test-repository nil t))))))
  (it "removes Org links from a string"
    (expect
     (minaduki//remove-org-links
      "Abc [[https://gnu.org][Link1]] def [[https://gnu.org][Link2]]")
     :to-equal
     "Abc Link1 def Link2")
    (expect
     (minaduki//remove-org-links
      "Abc [not a link]")
     :to-equal
     "Abc [not a link]")
    (expect
     (minaduki//remove-org-links
      "Abc [[https://google.com]]")
     :to-equal
     "Abc https://google.com")
    (expect
     (minaduki//remove-org-links
      "Abc [[https://google.com][Google]]")
     :to-equal
     "Abc Google")))

(describe "Extraction"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

  ;; Refs
  (cl-flet
      ((test (fn file)
             (let* ((fname (test-minaduki--abs-path file))
                    (buf (find-file-noselect fname)))
               (with-current-buffer buf
                 ;; Unlike tag extraction, it doesn't make sense to
                 ;; pass a filename.
                 (funcall fn)))))
    ;; Enable "cite:" link parsing
    (org-link-set-parameters "cite")
    (it "extracts web keys"
      (expect (test #'minaduki-extract/refs
                    "web_ref.org")
              :to-equal
              '(("website" . "//google.com/"))))
    (it "extracts cite keys"
      (expect (test #'minaduki-extract/refs
                    "cite_ref.org")
              :to-equal
              '(("cite" . "mitsuha2007")))
      (expect (test #'minaduki-extract/refs
                    "cite-ref.md")
              :to-equal
              '(("cite" . "sumire2019"))))
    (it "extracts all keys"
      (expect (test #'minaduki-extract/refs
                    "multiple-refs.org")
              :to-have-same-items-as
              '(("cite" . "orgroam2020")
                ("cite" . "plain-key")
                ("website" . "//www.orgroam.com/")))))
  ;; Title
  (cl-flet
      ((test (fn file)
             (let ((buf (find-file-noselect
                         (test-minaduki--abs-path file))))
               (with-current-buffer buf
                 (funcall fn)))))
    (it "extracts title from title property"
      (expect (test #'minaduki-extract/main-title
                    "titles/title.org")
              :to-equal
              '("Title"))
      (expect (test #'minaduki-extract/main-title
                    "titles/title.md")
              :to-equal
              '("Title in Markdown"))
      (expect (test #'minaduki-extract/main-title
                    "titles/aliases.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/main-title
                    "titles/headline.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/main-title
                    "titles/combination.org")
              :to-equal
              '("TITLE PROP")))

    (it "extracts alias"
      (expect (test #'minaduki-extract/aliases
                    "titles/title.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/aliases
                    "titles/aliases.org")
              :to-equal
              '("roam" "alias" "second" "line"))
      (expect (test #'minaduki-extract/aliases
                    "titles/headline.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/aliases
                    "titles/combination.org")
              :to-equal
              '("roam" "alias")))

    (it "extracts headlines"
      (expect (test #'minaduki-extract/aliases
                    "titles/title.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/first-headline
                    "titles/aliases.org")
              :to-equal
              nil)
      (expect (test #'minaduki-extract/first-headline
                    "titles/headline.org")
              :to-equal
              '("Headline"))
      (expect (test #'minaduki-extract/first-headline
                    "titles/headline.md")
              :to-equal
              '("Headline"))
      (expect (test #'minaduki-extract/first-headline
                    "titles/combination.org")
              :to-equal
              '("Headline")))

    (it "extracts all titles and aliases"
      (expect (test #'minaduki-extract/titles
                    "titles/combination.org")
              :to-equal
              '("TITLE PROP" "roam" "alias"))))
  ;; Links
  (cl-flet
      ((test (fn file)
             (let ((buf (find-file-noselect
                         (test-minaduki--abs-path file))))
               (with-current-buffer buf
                 (funcall fn)))))
    (it "extracts links from Markdown files"
      (expect (->> (test #'minaduki-extract/links
                         "baz.md")
                   (--map (seq-take it 3)))
              :to-have-same-items-as
              `([,(test-minaduki--abs-path "baz.md")
                 ,(test-minaduki--abs-path "nested/bar.org")
                 "file"]
                [,(test-minaduki--abs-path "baz.md")
                 "乙野四方字20180920"
                 "cite"]
                [,(test-minaduki--abs-path "baz.md")
                 "quro2017"
                 "cite"])))
    (it "extracts links from Org files"
      (expect (->> (test #'minaduki-extract/links
                         "foo.org")
                   ;; Drop the link type and properties
                   (--map (seq-take it 2)))
              :to-have-same-items-as
              `([,(test-minaduki--abs-path "foo.org")
                 ,(test-minaduki--abs-path "baz.md")]
                [,(test-minaduki--abs-path "foo.org")
                 "foo@john.com"]
                [,(test-minaduki--abs-path "foo.org")
                 "google.com"]
                [,(test-minaduki--abs-path "foo.org")
                 ,(test-minaduki--abs-path "bar.org")])))
    (xit "extracts Org citations"
      (expect (->> (test #'minaduki-extract//org-citation
                         "org-cite.org")
                   ;; Drop the link properties
                   (--map (seq-take it 3)))
              :to-have-same-items-as
              `([,(test-minaduki--abs-path "org-cite.org")
                 "赤坂アカand横槍メンゴ-oshinoko"
                 "cite"]
                [,(test-minaduki--abs-path "org-cite.org")
                 "フライand森永みるくand伊藤ハチand玄鉄絢and天野しゅにんたand雪子andもちオーレandコダマナオコand吉田丸悠andよしむらかなand黄井ぴかちand郷本andしおやてるこand松崎夏未and川浪いずみ20190511"
                 "cite"]
                [,(test-minaduki--abs-path "org-cite.org")
                 "takeshisu20191228"
                 "cite"]))))
  ;; IDs
  (cl-flet
      ((test (fn file)
             (let* ((fname (test-minaduki--abs-path file))
                    (buf (find-file-noselect fname)))
               (with-current-buffer buf
                 (funcall fn fname)))))
    (it "extracts ids"
      (expect (test #'minaduki-extract/ids
                    "headlines/headline.org")
              :to-have-same-items-as
              (list (minaduki-id :id "e84d0630-efad-4017-9059-5ef917908823"
                                 :file (test-minaduki--abs-path "headlines/headline.org")
                                 :point 22
                                 :level 1
                                 :title "Headline 1")
                    (minaduki-id :id "801b58eb-97e2-435f-a33e-ff59a2f0c213"
                                 :file (test-minaduki--abs-path "headlines/headline.org")
                                 :point 127
                                 :level 1
                                 :title "Headline 2"))))))

(describe "Test roam links"
  (it ""
    (expect (org-roam-link--split-path "")
            :to-equal
            '(title "" "" nil)))
  (it "title"
    (expect (org-roam-link--split-path "title")
            :to-equal
            '(title "title" "" nil)))
  (it "title*"
    (expect (org-roam-link--split-path "title*")
            :to-equal
            '(title+headline "title" "" 5)))
  (it "title*headline"
    (expect (org-roam-link--split-path "title*headline")
            :to-equal
            '(title+headline "title" "headline" 5)))
  (it "*headline"
    (expect (org-roam-link--split-path "*headline")
            :to-equal
            '(headline "" "headline" 0))))

(describe "Accessing the DB"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

  (it "Returns a file from its title"
    ;; There is something messed up going on that makes this run
    ;; *during* the cache build in before-all. Just try without this
    ;; statement and observe how the DB building messages (if you turn
    ;; it back on in `test-minaduki--init') are interleaved with
    ;; Buttercup's output. This would then fail if `list-files'
    ;; doesn't return the file below close enough to the top of the
    ;; list.
    ;;
    ;; We can at least be sure that this call here will 100% finish
    ;; before we actually evaluate the test results.
    (minaduki-db/build-cache)
    (expect (minaduki-db//fetch-file :title "Foo")
            :to-equal
            (list (test-minaduki--abs-path "foo.org"))))
  (it "Returns a list of files with the same title"
    ;; The cache is already built, and the world should be sane again
    ;; from here on out.
    (expect (minaduki-db//fetch-file :title "Headline")
            :to-have-same-items-as
            (-map #'test-minaduki--abs-path
                  '("headlines/headline.org"
                    "titles/headline.md"
                    "titles/headline.org"))))
  (it "Returns a file from an ID"
    (expect (minaduki-db//fetch-file
             :id "e84d0630-efad-4017-9059-5ef917908823")
            :to-equal
            (test-minaduki--abs-path "headlines/headline.org")))
  (it "Returns a nested file from its title"
    (expect (minaduki-db//fetch-file :title "Deeply Nested File")
            :to-equal
            (list (test-minaduki--abs-path "nested/deeply/deeply_nested_file.org")))))

;;; Tests
(xdescribe "minaduki-db/build-cache"
  (before-each
    (test-minaduki--init))

  (after-each
    (test-minaduki--teardown))

  (it "initializes correctly"
    ;; Cache
    (expect (caar (minaduki-db/query [:select (funcall count) :from files])) :to-be 8)
    (expect (caar (minaduki-db/query [:select (funcall count) :from links])) :to-be 5)
    (expect (caar (minaduki-db/query [:select (funcall count) :from files
                                      :where titles :is-null])) :to-be 1)
    (expect (caar (minaduki-db/query [:select (funcall count) :from refs])) :to-be 1)

    ;; Links
    (expect (caar (minaduki-db/query [:select (funcall count) :from links
                                      :where (= source $s1)]
                                     (test-minaduki--abs-path "foo.org"))) :to-be 1)
    (expect (caar (minaduki-db/query [:select (funcall count) :from links
                                      :where (= source $s1)]
                                     (test-minaduki--abs-path "nested/bar.org"))) :to-be 2)

    ;; Links -- File-to
    (expect (caar (minaduki-db/query [:select (funcall count) :from links
                                      :where (= dest $s1)]
                                     (test-minaduki--abs-path "nested/foo.org"))) :to-be 1)
    (expect (caar (minaduki-db/query [:select (funcall count) :from links
                                      :where (= dest $s1)]
                                     (test-minaduki--abs-path "nested/bar.org"))) :to-be 1)
    (expect (caar (minaduki-db/query [:select (funcall count) :from links
                                      :where (= dest $s1)]
                                     (test-minaduki--abs-path "unlinked.org"))) :to-be 0)
    ;; FIXME: titles has been merged into files
    (expect (minaduki-db/query [:select * :from titles])
            :to-have-same-items-as
            (list (list (test-minaduki--abs-path "alias.org")
                        (list "t1" "a1" "a 2"))
                  (list (test-minaduki--abs-path "bar.org")
                        (list "Bar"))
                  (list (test-minaduki--abs-path "foo.org")
                        (list "Foo"))
                  (list (test-minaduki--abs-path "nested/bar.org")
                        (list "Nested Bar"))
                  (list (test-minaduki--abs-path "nested/foo.org")
                        (list "Nested Foo"))
                  (list (test-minaduki--abs-path "no-title.org")
                        (list "Headline title"))
                  (list (test-minaduki--abs-path "web_ref.org") nil)
                  (list (test-minaduki--abs-path "unlinked.org")
                        (list "Unlinked"))))

    (expect (minaduki-db/query [:select * :from refs])
            :to-have-same-items-as
            (list (list "https://google.com/" (test-minaduki--abs-path "web_ref.org") "website")))

    ;; Expect rebuilds to be really quick (nothing changed)
    (expect (minaduki-db/build-cache)
            :to-equal
            (list :files 0 :links 0 :tags 0 :titles 0 :refs 0 :deleted 0))))

(provide 'test-minaduki)

;;; test-minaduki.el ends here
