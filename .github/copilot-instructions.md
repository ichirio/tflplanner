# Copilot instructions for tflplanner

This repository **is the R package** `tflplanner` (`DESCRIPTION` is at the
repository root).

Follow these documents rather than anything duplicated here; where this file
and they disagree, they win:

- **The rules for working on the code** (dependencies, screens, PRs, tests,
  files): [CLAUDE.md](../CLAUDE.md), written for any coding agent.
- **Orientation:** [AGENTS.md](../AGENTS.md)
- **How to contribute** (fork, pull request, the checks before it):
  [CONTRIBUTING.md](../CONTRIBUTING.md)
- **What the app is made of:** `vignettes/articles/concepts.Rmd`
- **Git work for a contributor** (updating the fork, rebasing a topic branch,
  what to do when CI or a review asks): the section of that name in
  [AGENTS.md](../AGENTS.md)

Key rules: dependencies point one way (rtfreporter <- tflspec <- tflplanner);
ASCII-only R code (`\uXXXX` escapes); every text the app shows has its
Japanese in `inst/i18n/strings/<file>.csv`; a pull request never changes
`Version` or `NEWS.md`.
