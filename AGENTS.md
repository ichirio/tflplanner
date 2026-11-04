# Agent / contributor instructions for tflplanner

This repository **is the R package** `tflplanner` (`DESCRIPTION` is at the
root): a 'shiny' app and functions that manage clinical TFL studies on top of
[tflspec](https://github.com/ichirio/tflspec) and
[rtfreporter](https://github.com/ichirio/rtfreporter).  This file only points
to the documents that hold the rules.

## Read these first

| Topic | Document |
|-------|----------|
| The rules for working on the code (dependencies, screens, PRs, tests, files) | `CLAUDE.md` |
| How to contribute: issues, fork and pull request, the checks before a PR | `CONTRIBUTING.md` |
| What the app is made of (studies, specs, screens) | `vignettes/articles/concepts.Rmd` |
| The AI tab's flow | `vignettes/articles/ai-flow.Rmd` |

`CLAUDE.md` is written for any coding agent, not only Claude: read it as this
repository's agent guide.  Where this file and those documents disagree,
they win.

## In short

- Dependencies point one way: rtfreporter <- tflspec <- tflplanner.  A change
  to the table layout belongs in rtfreporter, a change to the spec in tflspec.
- R code is ASCII (`\uXXXX` escapes).  Every text the app shows has its
  Japanese in `inst/i18n/strings/<file>.csv`.
- A pull request never changes `Version` or `NEWS.md`.
- Before a pull request: the tests of what you changed and `lintr` on the
  changed files (see `CONTRIBUTING.md`).
