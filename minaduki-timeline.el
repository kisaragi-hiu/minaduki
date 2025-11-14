;;; minaduki-timeline.el --- Timeline commands -*- lexical-binding: t -*-

;;; Commentary:

;; Commands for listing notes on a timeline.
;; TODO: sketch out what we actually want with this
;; * TODO ways to make reading multi-file collections more pleasant
;; ** TODO go to the next/prev file with this tag
;; ** TODO go to the next/prev file with a link to a file
;; ** TODO dedicated view listing backlinks to a file
;; ** TODO dedicated view listing files with a given tag

;;; Code:

(require 's)
(require 'minaduki-utils)
(require 'minaduki-extract)
(require 'f)

(defun minaduki-timeline--get-year (str)
  "Return the year based on STR."
  (-some->> str
    (s-match (rx bol (= 4 digit)))
    car))
(defun minaduki-timeline--get-buffer-year ()
  "Get the year this buffer represents."
  (or (-some->> (minaduki::current-file-name)
        f-base
        minaduki-timeline--get-year)
      (-some->> (car (minaduki--get-file-prop "created"))
        minaduki-timeline--get-year)))

(provide 'minaduki-timeline)

;;; minaduki-timeline.el ends here
