# Contributing to tflplanner

Thanks for taking the time to contribute! Issues and pull requests are
welcome at <https://github.com/ichirio/tflplanner>.

By participating in this project you agree to abide by its [Code of
Conduct](https://ichirio.github.io/tflplanner/CODE_OF_CONDUCT.md).

## How to contribute

1.  Open or comment on an
    [issue](https://github.com/ichirio/tflplanner/issues) to report a
    bug or propose a change (the issue forms guide you: *Bug report* /
    *Feature request*).
2.  For anything non-trivial, agree the approach on the issue first.
3.  Fork the repository, update your fork’s `main` from upstream, create
    a topic branch, make the change with its tests, and open a pull
    request against `main`. The
    [`next`](https://rdrr.io/r/base/Control.html) branch, the
    `automerge` label and `merge-after:` lines are the maintainers’ own
    workflow; a maintainer adds any label that is needed. The steps for
    updating the fork and rebasing a topic branch are in *Git work for a
    contributor* in `AGENTS.md`.
4.  Address review feedback; once CI is green and a maintainer approves,
    it is merged.

## Before you open a pull request

- `devtools::test()` passes and `devtools::check()` shows no errors or
  warnings.

- `lintr::lint_package()` reports nothing (the configuration is
  `.lintr`).

- R source files are ASCII: write non-ASCII text as `\uXXXX` escapes.
  The app’s Japanese strings live in `inst/i18n/strings/<file>.csv` (the
  R file that shows them; `common.csv` for several).

- **Record the change in the PR description.** A PR never edits
  `NEWS.md`, `CHANGELOG.md` or `Version:`, and there is no separate memo
  file. Fill the template’s *User-visible change (for NEWS)* field
  without being asked: one or two bullets in NEWS style (what a user can
  now do, or what is fixed, named by its function or argument), or
  “none” for an internal change; then “Breaking: no”, or “Breaking: yes”
  with the migration. The maintainers write `NEWS.md` from the merged
  PRs at release. For example:

  ``` markdown
  ## User-visible change (for NEWS)

  - The AI tab's docs field adds a file at once: on Browse, on Enter, or on
    leaving the field.

  Breaking: no
  ```

  The maintainers bump the version when they tag or release.

tflplanner builds on
[rtfreporter](https://github.com/ichirio/rtfreporter); a change the
table layout needs belongs there.
