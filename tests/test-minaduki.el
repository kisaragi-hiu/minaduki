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

(defmacro test-in-file (file &rest body)
  "Run BODY with FILE opened.
In BODY, `fname' refers to the resolved path of FILE."
  (declare (indent 1))
  (cl-with-gensyms (buf)
    `(let* ((fname (test-minaduki--abs-path ,file))
            (,buf (find-file-noselect fname)))
       (ignore fname) ; tell byte-comp not using it is fine
       (with-current-buffer ,buf
         ,@body))))

(defvar test-repository (expand-file-name "tests/minaduki-files")
  "Directory containing minaduki test org files.")

(defvar test-lit-directory (expand-file-name "lit" test-repository)
  "Directory containing minaduki-lit test files.")

(defvar temp-dir
  (expand-file-name (make-temp-name "minaduki") temporary-file-directory))

(defun test-minaduki--init ()
  "."
  (let ((minaduki-verbose nil)
        (inhibit-message t))
    (copy-directory test-repository temp-dir)
    (setq org-directory temp-dir)
    (setq minaduki:db-location (f-join temp-dir "minaduki.db"))
    (setq minaduki/vaults (list temp-dir))
    (setq minaduki-lit/bibliography
          (f-join temp-dir "lit" "entries.org"))
    (minaduki-mode)
    (minaduki-db:build-cache)))

(defun test-minaduki--teardown ()
  (minaduki-mode -1)
  (delete-file minaduki/db-location))

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

(describe "minaduki-vault"
  (before-all
    (setq minaduki/vaults '("/tmp/a" "/tmp/b"))
    (make-directory "/tmp/b/external/hugo" t)
    (with-temp-file "/tmp/b/external/hugo/config.yaml"
      (insert "this is used to test nested vault detection")))
  (it "can detect current vault"
    (expect (minaduki-vault-closest "/tmp/b")
            :to-equal "/tmp/b")
    (expect (minaduki-vault-closest "/tmp/b/more/stuff")
            :to-equal "/tmp/b")
    (expect (minaduki-vault-closest "/tmp/a/more/stuff")
            :to-equal "/tmp/a"))
  (it "can return nil when outside a vault"
    (expect (minaduki-vault-closest "/tmp/c")
            :to-be nil))
  (it "can detect nested vaults"
    (expect (let ((minaduki-nested-vault-search-path '("external"))
                  (minaduki-nested-vault-root-files '(".obsidian" "config.yaml")))
              (minaduki-vault-closest "/tmp/b/external/nothing"))
            :to-equal "/tmp/b")
    (expect (let ((minaduki-nested-vault-search-path '("external"))
                  (minaduki-nested-vault-root-files '(".obsidian" "config.yaml")))
              (minaduki-vault-closest "/tmp/b/external/hugo"))
            :to-equal "/tmp/b/external/hugo")))

(describe "minaduki::file-type"
  (it "does not check unregisted files"
    (expect (with-temp-buffer
              (setq buffer-file-name "hello.whatever")
              (prog1 (minaduki::file-type)
                (setq buffer-file-name nil)))
            :to-be nil)
    (expect (with-temp-buffer
              (setq buffer-file-name "hello.java"
                    major-mode 'java-mode)
              (prog1 (minaduki::file-type)
                (setq buffer-file-name nil)))
            :to-be nil))
  (it "checks registered types"
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
                data ("tags" ["voice"]
                      "type" "entries"
                      "key" "hitomine2013"
                      "author" "大崎ひとみ"
                      "publisher" "t2library課外活動部"
                      "year" "2013"
                      "title" "あたらしい女声の教科書")))
       (202 . #s(hash-table
                 size 65 test equal rehash-size 1.5 rehash-threshold 0.8125
                 data ("sources" ["https://www.w3.org/People/Bos/DesignGuide/designguide.html"]
                       "tags" ["webdev" "css" "html"]
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
     '((1 . #s(hash-table
               size 65 test equal rehash-size 1.5
               rehash-threshold 0.8125
               data ("author" "大崎ひとみ"
                     "type" "manual"
                     "title" "あたらしい女声の教科書"
                     "key" "hitomine2013"
                     "tags" ["voice"]
                     "year" "2013"
                     "publisher" "t2library課外活動部")))
       (153 .
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

(describe "minaduki::format-link"
  (it "formats Org links"
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki::format-link :target "file:///tmp/abc.org"))
     :to-equal
     "[[/tmp/abc.org]]")
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki::format-link :target "file:///tmp/abc.org"
                              :desc "ABC"))
     :to-equal
     "[[/tmp/abc.org][ABC]]")
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'org-mode))
       (minaduki::format-link :target "https://kisaragi-hiu.com"
                              :desc "ABC"))
     :to-equal
     "[[https://kisaragi-hiu.com][ABC]]"))
  (it "formats Markdown links"
    (expect
     (let ((minaduki:link-insertion-format 'absolute)
           (major-mode 'markdown-mode))
       (minaduki::format-link :target "https://kisaragi-hiu.com"
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
         (minaduki::format-link :target "file:///tmp/abc.org")))
     :to-equal
     "[[tmp:abc.org]]"))
  (it "formats relative links"
    (expect
     (let ((minaduki:link-insertion-format 'relative)
           (major-mode 'org-mode)
           (default-directory "/"))
       (minaduki::format-link :target "file:///tmp/abc.org"))
     :to-equal
     "[[file:tmp/abc.org]]")
    (expect
     (let ((minaduki:link-insertion-format 'relative)
           (major-mode 'markdown-mode)
           (default-directory "/"))
       (minaduki::format-link :target "/tmp/abc.md"))
     :to-equal
     "[abc.md](tmp/abc.md)")))

(describe "Utils"
  (before-all
    (test-minaduki--init))

  (it "converts between calendar.el dates and YYYY-MM-DD date strings"
    (expect (minaduki-date::calendar.el->ymd '(7 17 2019))
            :to-equal
            "2019-07-17")
    (expect (minaduki-date::ymd->calendar.el "2012-01-02")
            :to-equal
            '(1 2 2012)))
  (it "converts a title to a slug"
    (expect (minaduki::to-slug "English")
            :to-equal "english")
    (expect (minaduki::to-slug "Text with space と漢字")
            :to-equal "text-with-space-と漢字")
    (expect (minaduki::to-slug "many____underscores")
            :to-equal "many-underscores")
    ;; Keep diacritics
    (expect (minaduki::to-slug "äöü")
            :to-equal "äöü")
    ;; Normalizes to composed from
    (expect (minaduki::to-slug (string ?て #x3099))
            :to-equal (string ?で))
    (expect (minaduki::to-slug "_starting and ending_")
            :to-equal "starting-and-ending")
    (expect (minaduki::to-slug "isn't alpha numeric")
            :to-equal "isn-t-alpha-numeric"))
  (describe "list-files"
    (it "using pure elisp"
      (expect
       (minaduki-vault--list-files/elisp (expand-file-name org-directory))
       :to-have-same-items-as
       (--map
        (test-minaduki--abs-path (f-relative it test-repository))
        (f-files test-repository nil t))))
    (when (executable-find "rg")
      (it "using rg"
        (expect
         (minaduki-vault--list-files/rg (executable-find "rg")
                                        (expand-file-name org-directory))
         :to-have-same-items-as
         (--map
          (test-minaduki--abs-path (f-relative it test-repository))
          (f-files test-repository nil t))))))
  (it "removes Org links from a string"
    (expect
     (minaduki::remove-org-links
      "Abc [[https://gnu.org][Link1]] def [[https://gnu.org][Link2]]")
     :to-equal
     "Abc Link1 def Link2")
    (expect
     (minaduki::remove-org-links
      "Abc [not a link]")
     :to-equal
     "Abc [not a link]")
    (expect
     (minaduki::remove-org-links
      "Abc [[https://google.com]]")
     :to-equal
     "Abc https://google.com")
    (expect
     (minaduki::remove-org-links
      "Abc [[https://google.com][Google]]")
     :to-equal
     "Abc Google")))

(describe "Extraction"
  (before-all
    (test-minaduki--init))

  ;; Refs
  ;; Enable "cite:" link parsing
  (org-link-set-parameters "cite")
  (it "extracts web keys"
    (expect (test-in-file "web_ref.org"
              (minaduki-extract/refs))
            :to-equal
            '(("website" . "//google.com/"))))
  (it "extracts cite keys"
    (expect (test-in-file "cite_ref.org"
              (minaduki-extract/refs))
            :to-equal
            '(("cite" . "mitsuha2007")))
    (expect (test-in-file "cite-ref.md"
              (minaduki-extract/refs))
            :to-equal
            '(("cite" . "sumire2019"))))
  (it "extracts all keys"
    (expect (test-in-file "multiple-refs.org"
              (minaduki-extract/refs))
            :to-have-same-items-as
            '(("cite" . "orgroam2020")
              ("cite" . "plain-key")
              ("website" . "//www.orgroam.com/"))))
  ;; Title
  (it "extracts title from title property"
    (expect (test-in-file "titles/title.org"
              (minaduki-extract/main-title))
            :to-equal
            '("Title"))
    (expect (test-in-file "titles/title.md"
              (minaduki-extract/main-title))
            :to-equal
            '("Title in Markdown"))
    (expect (test-in-file "titles/aliases.org"
              (minaduki-extract/main-title))
            :to-equal
            nil)
    (expect (test-in-file "titles/headline.org"
              (minaduki-extract/main-title))
            :to-equal
            nil)
    (expect (test-in-file "titles/combination.org"
              (minaduki-extract/main-title))
            :to-equal
            '("TITLE PROP")))

  (it "extracts alias"
    (expect (test-in-file "titles/title.org"
              (minaduki-extract/aliases))
            :to-equal
            nil)
    (expect (test-in-file "titles/aliases.org"
              (minaduki-extract/aliases))
            :to-equal
            '("roam" "alias" "second" "line"))
    (expect (test-in-file "titles/headline.org"
              (minaduki-extract/aliases))
            :to-equal
            nil)
    (expect (test-in-file "titles/combination.org"
              (minaduki-extract/aliases))
            :to-equal
            '("roam" "alias")))

  (it "extracts headlines"
    (expect (test-in-file "titles/title.org"
              (minaduki-extract/aliases))
            :to-equal
            nil)
    (expect (test-in-file "titles/aliases.org"
              (minaduki-extract/first-headline))
            :to-equal
            nil)
    (expect (test-in-file "titles/headline.org"
              (minaduki-extract/first-headline))
            :to-equal
            '("Headline"))
    (expect (test-in-file "titles/headline.md"
              (minaduki-extract/first-headline))
            :to-equal
            '("Headline"))
    (expect (test-in-file "titles/combination.org"
              (minaduki-extract/first-headline))
            :to-equal
            '("Headline")))

  (it "extracts all titles and aliases"
    (expect (test-in-file "titles/combination.org"
              (minaduki-extract/titles))
            :to-equal
            '("TITLE PROP" "roam" "alias")))
  ;; Links
  (it "extracts links from Markdown files"
    (expect (test-in-file "baz.md"
              (->> (minaduki-extract/links)
                   (--map (seq-take it 3))))
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
    (expect (test-in-file "foo.org"
              (->> (minaduki-extract/links)
                   ;; Drop the link type and properties
                   (--map (seq-take it 2))))
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
    (expect (test-in-file "org-cite.org"
              (->> (minaduki-extract//org-citation)
                   ;; Drop the link properties
                   (--map (seq-take it 3))))
            :to-have-same-items-as
            `([,(test-minaduki--abs-path "org-cite.org")
               "赤坂アカand横槍メンゴ-oshinoko"
               "cite"]
              [,(test-minaduki--abs-path "org-cite.org")
               "フライand森永みるくand伊藤ハチand玄鉄絢and天野しゅにんたand雪子andもちオーレandコダマナオコand吉田丸悠andよしむらかなand黄井ぴかちand郷本andしおやてるこand松崎夏未and川浪いずみ20190511"
               "cite"]
              [,(test-minaduki--abs-path "org-cite.org")
               "takeshisu20191228"
               "cite"])))
  ;; IDs
  (it "extracts ids"
    (expect (test-in-file "headlines/headline.org"
              (minaduki-extract/ids fname))
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
                               :title "Headline 2")))))

(describe "Test minaduki: wikilinks"
  (it ""
    (expect (minaduki-wikilink::split-path "")
            :to-equal
            '(title "" "" nil)))
  (it "title"
    (expect (minaduki-wikilink::split-path "title")
            :to-equal
            '(title "title" "" nil)))
  (it "title*"
    (expect (minaduki-wikilink::split-path "title*")
            :to-equal
            '(title+headline "title" "" 5)))
  (it "title*headline"
    (expect (minaduki-wikilink::split-path "title*headline")
            :to-equal
            '(title+headline "title" "headline" 5)))
  (it "*headline"
    (expect (minaduki-wikilink::split-path "*headline")
            :to-equal
            '(headline "" "headline" 0))))

(describe "Accessing the DB"
  (before-all
    (test-minaduki--init))

  (it "can check presence"
    (expect (minaduki-db::file-present? (test-minaduki--abs-path "baz.md"))
            :to-be-truthy)
    (expect (minaduki-db::file-present? (test-minaduki--abs-path "no"))
            :to-be nil))

  (describe "fetch-file"
    (it "can fetch id"
      (expect (minaduki-db::fetch-file :id "801b58eb-97e2-435f-a33e-ff59a2f0c213")
              :to-equal
              (test-minaduki--abs-path "headlines/headline.org")))
    (it "can fetch citekey"
      (expect (minaduki-db::fetch-file :key "hitomine2013")
              :to-equal
              (test-minaduki--abs-path "lit/hitomine2013.org")))
    (it "can fetch title"
      (expect (minaduki-db::fetch-file :title "Same title")
              :to-have-same-items-as
              (list (test-minaduki--abs-path "same-title2.org")
                    (test-minaduki--abs-path "same-title.org")))
      (expect (minaduki-db::fetch-file :title "CSL-JSON sample data")
              :to-equal
              (list (test-minaduki--abs-path "lit/README.org")))
      (expect (minaduki-db::fetch-file :title "Foo")
              :to-equal
              (list (test-minaduki--abs-path "foo.org")))
      (expect (minaduki-db::fetch-file :title "Headline")
              :to-have-same-items-as
              (-map #'test-minaduki--abs-path
                    '("headlines/headline.org"
                      "titles/headline.md"
                      "titles/headline.org"))))
    (it "can return a nested file from its title"
      (expect (minaduki-db::fetch-file :title "Deeply Nested File")
              :to-equal
              (list (test-minaduki--abs-path "nested/deeply/deeply_nested_file.org")))))
  (it "can fetch an id object"
    (expect (minaduki-db::fetch-id "801b58eb-97e2-435f-a33e-ff59a2f0c213")
            :to-equal
            (minaduki-id :id "801b58eb-97e2-435f-a33e-ff59a2f0c213"
                         :file (test-minaduki--abs-path "headlines/headline.org")
                         :point 127
                         :level 1
                         :title "Headline 2")))
  (describe "fetch-lit-entry"
    (it "can fetch a lit-entry object"
      (let ((entry (minaduki-db::fetch-lit-entry "orgroam2020")))
        (expect (oref entry file)
                :to-equal
                (test-minaduki--abs-path "multiple-refs.org"))
        (expect (oref entry key)
                :to-equal
                "orgroam2020")
        (expect (oref entry point)
                :to-equal
                1)))
    (it "information from bibliography is used over the note file"
      (let ((entry (minaduki-db::fetch-lit-entry "hitomine2013")))
        (expect (oref entry file)
                :to-equal
                (test-minaduki--abs-path "lit/entries.org"))
        (expect (oref entry key)
                :to-equal
                "hitomine2013")
        (expect (oref entry point)
                :to-equal
                33)
        (expect (oref entry props)
                :to-equal/ht
                (ht<-plist
                 '("tags" ("voice")
                   "type" "entries"
                   "key" "hitomine2013"
                   "author" "大崎ひとみ"
                   "publisher" "t2library課外活動部"
                   "year" "2013"
                   "title" "あたらしい女声の教科書"))))))
  (it "can return all authors"
    (expect (minaduki-db::fetch-lit-authors)
            :to-have-same-items-as
            '("Bert Bos" "シャノン" "大崎ひとみ")))
  (it "can return all tags"
    (expect (minaduki-db::fetch-all-tags)
            :to-have-same-items-as
            `("t3" "t2 with space" "t1" "t4 second-line" "tag3" "tag2" "hello" ,(file-name-base (directory-file-name org-directory)))))
  (it "can return the title of a file"
    (expect (minaduki-db::fetch-title
             (test-minaduki--abs-path "lit/hitomine2013.org"))
            :to-equal
            "あたらしい女声の教科書"))
  (it "can return files that are tagged with a given tag"
    (expect (minaduki-db::fetch-tag-references "hello")
            :to-have-same-items-as
            (list (test-minaduki--abs-path "tags/hugo-style.org"))))
  (it "can fetch backlinks"
    (expect (length (minaduki-db::fetch-backlinks "乙野四方字20180920"))
            :to-be-greater-than 0))
  (it "can get the stored hash of a file"
    (expect (minaduki-db::fetch-file-hash
             (test-minaduki--abs-path "front-matter/json.md"))
            :to-equal
            "8735b00eebf501c1c39dc4d3ba21424df591aec7")))

(provide 'test-minaduki)

;;; test-minaduki.el ends here
