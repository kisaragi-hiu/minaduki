# Minaduki

I like Org-roam v1, but I have different ideas of what could make it better in my opinion. So this is my fork.

I plan to also merge other v1 packages into this repository.

My goal is to *very* slowly inch towards making this as usable as, say, Obsidian, or Zettel Notes for Android.

## Caveat

I expect myself to be the only user and will ship breaking changes without warning and without much explanation.

Documentation may be out of sync with reality.

## Features

- Clone Obsidian vaults into your notes to be indexed
- Multiple vaults
- Markdown and Org support
- Yet another literature entries system

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
          (goto-char sexp)
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
