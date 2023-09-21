;;; test-minaduki-perf.el --- Performance Tests for Minaduki -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup))

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

(setq minaduki-verbose nil)

(defconst test-minaduki-perf-zip-url "https://github.com/org-roam/test-org-files/archive/master.zip"
  "Path to zip for test org-roam files.")

(defun test-minaduki-perf--init ()
  "."
  (let* ((temp-loc (expand-file-name (make-temp-name "test-org-files-") temporary-file-directory))
         (zip-file-loc (concat temp-loc ".zip"))
         (_ (url-copy-file test-minaduki-perf-zip-url zip-file-loc))
         (_ (shell-command (format "mkdir -p %s && unzip -j -qq %s -d %s" temp-loc zip-file-loc temp-loc))))
    (setq org-directory temp-loc)))

(xdescribe "Cache Build"
  (it "cache build from scratch time to be acceptable"
    (test-minaduki-perf--init)
    (pcase (benchmark-run 1 (minaduki-edb::build-cache t))
      (`(,time ,gcs ,time-in-gc)
       (message "Elapsed time: %fs (%fs in %d GCs)" time time-in-gc gcs)
       (expect time :to-be-less-than 110))))
  (it "builds quickly without change"
    (pcase (benchmark-run 1 (minaduki-edb::build-cache))
      (`(,time ,gcs ,time-in-gc)
       (message "Elapsed time: %fs (%fs in %d GCs)" time time-in-gc gcs)
       (expect time :to-be-less-than 5)))))
