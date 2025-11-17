# Minaduki

I like Org-roam v1, but I have different ideas of what I want my notes manager to do. So this is my fork.

My goal is to *very* slowly inch towards making this as usable as, say, Obsidian.

## Badges

I might as well set up code coverage, since I like making much needed changes but I don't like to have to fix my broken code while trying to access my notes on my phone.

[![codecov](https://codecov.io/gh/kisaragi-hiu/minaduki/graph/badge.svg?token=237DXF6TAI)](https://codecov.io/gh/kisaragi-hiu/minaduki)

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
