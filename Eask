;; -*- lexical-binding: t -*-
;;
;; The fun part where you must duplicate info from your package file
;; exactly correctly
;;
;; WHY DON'T JUST USE THE KNOWN CORRECT VALUES YOU'RE ALREADY READING FROM
;; package-file?
;; IF YOU ALREADY KNOW WHAT THEY SHOULD BE THEN WHY MUST I FILL THEM INTO THIS FILE?
;; package.el isn't going anywhere, nor is this file going to ever replace
;; <name>-pkg.el or package.el headers, these requirements are delusional
;;
;; Have I mentioned I don't like Eask?
(package "minaduki" "0.1" "An extremely opinionated Org-roam v1 fork")
(website-url "https://github.com/kisaragi-hiu/minaduki")
(keywords "org-mode" "convenience")
(package-file "minaduki.el")

(source "gnu")
(source "melpa")
(depends-on "emacs" "29.1")
(depends-on "yaml")
(depends-on "dash")
(depends-on "f")
(depends-on "s")
(depends-on "org")
(depends-on "markdown-mode")
(depends-on "transient")

(development
 (depends-on "undercover")
 (depends-on "ht") ; for ht-equal?, used for testing
 ;; Optional dependencies
 (depends-on "parsebib"))
