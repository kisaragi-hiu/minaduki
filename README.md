# Kisaragi Hiu's Org-roam v1 fork

I don't want to adapt to Org-roam v2 because I have built a file-based no-ID workflow in my notes and don't want to change it.

I plan to also merge other v1 packages into this repository.

I cannot guarantee that this version will be useful for anybody other than me. There are many itches that I had with Org-roam v1 that conflicted with what other users expected, and those are the changes that I might make in this fork.

## Credits

The vast majority of the work comes from [Org-roam](https://github.com/org-roam/org-roam), © Jethro Kuan and contributors & distributed under GPLv3.

I also merged Org-roam-bibtex ([5236917](https://github.com/org-roam/org-roam-bibtex/commit/5236917e1d8a4f88daadacc690248854f53facb4)) into this repository.

Markdown support was added largely by adapting md-roam logic into extraction functions.

## Development setup

```elisp
(require 'emacsql)
(emacsql-fix-vector-indentation)

(defun k/inside-plist? ()
  "Is point situated inside a plist?

We determine a plist to be a list that starts with a keyword."
  (let ((start (point)))
    (save-excursion
      (beginning-of-defun)
      (let ((sexp (nth 1 (parse-partial-sexp (point) start))))
        (when sexp
          (setf (point) sexp)
          (looking-at (rx "(" (* (syntax whitespace)) ":")))))))

(define-advice calculate-lisp-indent (:around (func &rest args)
                                      plist)
  "Indent plists in a sane way."
  (if (save-excursion
        (beginning-of-line)
        (k/inside-plist?))
      (let ((lisp-indent-offset 1))
        (apply func args))
    (apply func args)))


```
