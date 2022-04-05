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
(require 'minaduki)
(require 'seq)
(require 'dash)

(defun test-minaduki--abs-path (file-path)
  "Get absolute FILE-PATH from `org-directory'."
  (expand-file-name file-path org-directory))

(defun test-minaduki//find-file (path)
  "PATH."
  (let ((path (test-minaduki--abs-path path)))
    (make-directory (file-name-directory path) t)
    (find-file path)))

(defvar test-repository (expand-file-name "tests/minaduki-files")
  "Directory containing minaduki test org files.")

(defun test-minaduki--init ()
  "."
  (let ((original-dir test-repository)
        (new-dir (expand-file-name (make-temp-name "minaduki") temporary-file-directory))
        (minaduki-verbose nil))
    (copy-directory original-dir new-dir)
    (setq org-directory new-dir)
    (minaduki-mode)
    (sleep-for 2)))

(defun test-minaduki--teardown ()
  (minaduki-mode -1)
  (delete-file minaduki/db-location)
  (minaduki-db//close))

(describe "minaduki/format-link"
  (it "formats Org links"
    (cl-flet ((test-org (&rest args)
                        (let ((major-mode 'org-mode))
                          (apply #'minaduki/format-link args))))
      (expect
       (test-org :target "file:///tmp/abc.org")
       :to-equal
       "[[/tmp/abc.org]]")
      (expect
       (test-org :target "file:///tmp/abc.org"
                 :desc "ABC")
       :to-equal
       "[[/tmp/abc.org][ABC]]")
      (expect
       (test-org :target "https://kisaragi-hiu.com"
                 :desc "ABC")
       :to-equal
       "[[https://kisaragi-hiu.com][ABC]]")))
  (it "formats Markdown links"
    (cl-flet ((test-markdown (&rest args)
                             (let ((major-mode 'markdown-mode))
                               (apply #'minaduki/format-link args))))
      (expect
       (test-markdown :target "https://kisaragi-hiu.com"
                      :desc "ABC")
       :to-equal
       "[ABC](https://kisaragi-hiu.com)"))))

(describe "Utils"
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
            :to-equal "text_with_space_と漢字")
    (expect (minaduki//title-to-slug "many____underscores")
            :to-equal "many_underscores")
    ;; Keep diacritics
    (expect (minaduki//title-to-slug "äöü")
            :to-equal "äöü")
    ;; Normalizes to composed from
    (expect (minaduki//title-to-slug (string ?て #x3099))
            :to-equal (string ?で))
    (expect (minaduki//title-to-slug "_starting and ending_")
            :to-equal "starting_and_ending")
    (expect (minaduki//title-to-slug "isn't alpha numeric")
            :to-equal "isn_t_alpha_numeric"))
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

(describe "Ref extraction"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

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
                ("website" . "//www.orgroam.com/"))))))

(describe "Title extraction"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

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

    (it "extracts all"
      (expect (test #'org-roam--extract-titles
                    "titles/combination.org")
              :to-equal
              '("TITLE PROP" "roam" "alias")))))

(describe "Link extraction"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

  (cl-flet
      ((test (fn file)
             (let ((buf (find-file-noselect
                         (test-minaduki--abs-path file))))
               (with-current-buffer buf
                 (funcall fn)))))
    (it "extracts links from Markdown files"
      (expect (->> (test #'org-roam--extract-links
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
      (expect (->> (test #'org-roam--extract-links
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
      (expect (->> (test #'minaduki-extract/citation
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
                 "cite"])))))

(describe "Tag extraction"
  :var (minaduki/tag-sources)
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

  (cl-flet
      ((test (fn file)
             (let* ((fname (test-minaduki--abs-path file))
                    (buf (find-file-noselect fname)))
               (with-current-buffer buf
                 (funcall fn fname)))))
    (it "extracts from #+tags[]"
      (expect (test #'org-roam--extract-tags-prop
                    "tags/hugo-style.org")
              :to-equal
              '("hello" "tag2" "tag3")))
    (it "extracts hashtag style tags, but only from frontmatter"
      (expect (test #'minaduki-extract/tags-hashtag-frontmatter
                    "tags/tag.md")
              :to-equal
              '("#abc" "#def" "#ghi")))

    (it "extracts hashtag style tags"
      (expect (test #'minaduki-extract/tags-hashtag
                    "tags/tag.md")
              :to-equal
              '("#abc" "#def" "#ghi" "#not-frontmatter-a" "#not-front-matter-b")))

    (it "extracts from prop"
      (expect (test #'org-roam--extract-tags-prop
                    "tags/tag.org")
              :to-equal
              '("t1" "t2 with space" "t3" "t4 second-line"))
      (expect (test #'org-roam--extract-tags-prop
                    "tags/no_tag.org")
              :to-equal
              nil))

    (it "extracts from all directories"
      (expect (test #'org-roam--extract-tags-all-directories
                    "base.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-tags-all-directories
                    "tags/tag.org")
              :to-equal
              '("tags"))
      (expect (test #'org-roam--extract-tags-all-directories
                    "nested/deeply/deeply_nested_file.org")
              :to-equal
              '("nested" "deeply")))

    (it "extracts from last directory"
      (expect (test #'org-roam--extract-tags-last-directory
                    "base.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-tags-last-directory
                    "tags/tag.org")
              :to-equal
              '("tags"))
      (expect (test #'org-roam--extract-tags-last-directory
                    "nested/deeply/deeply_nested_file.org")
              :to-equal
              '("deeply")))

    (it "extracts from first directory"
      (expect (test #'org-roam--extract-tags-first-directory
                    "base.org")
              :to-equal
              nil)
      (expect (test #'org-roam--extract-tags-first-directory
                    "tags/tag.org")
              :to-equal
              '("tags"))
      (expect (test #'org-roam--extract-tags-first-directory
                    "nested/deeply/deeply_nested_file.org")
              :to-equal
              '("nested")))

    (describe "uses minaduki/tag-sources correctly"
      (it "'(prop)"
        (expect (let ((minaduki/tag-sources '(org-roam--extract-tags-prop)))
                  (test #'org-roam--extract-tags
                        "tags/tag.org"))
                :to-equal
                '("t1" "t2 with space" "t3" "t4 second-line")))
      (it "'(prop all-directories)"
        (expect (let ((minaduki/tag-sources '(org-roam--extract-tags-prop
                                              org-roam--extract-tags-all-directories)))
                  (test #'org-roam--extract-tags
                        "tags/tag.org"))
                :to-equal
                '("t1" "t2 with space" "t3" "t4 second-line" "tags"))))))

(describe "ID extraction"
  (before-all
    (test-minaduki--init))

  (after-all
    (test-minaduki--teardown))

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
              `(["e84d0630-efad-4017-9059-5ef917908823" ,(test-minaduki--abs-path "headlines/headline.org") 1 "Headline 1"]
                ["801b58eb-97e2-435f-a33e-ff59a2f0c213" ,(test-minaduki--abs-path "headlines/headline.org") 1 "Headline 2"])))))

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
    (expect (minaduki-db//fetch-file :title "Foo")
            :to-equal
            (list (test-minaduki--abs-path "foo.org"))))
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
    (expect (caar (minaduki-db/query [:select (funcall count) :from titles])) :to-be 8)
    (expect (caar (minaduki-db/query [:select (funcall count) :from titles
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
    ;; TODO Test titles
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
