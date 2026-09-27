# Contributing to tflplanner

Thanks for taking the time to contribute! Issues and pull requests are
welcome at <https://github.com/ichirio/tflplanner>.

By participating in this project you agree to abide by its
[Code of Conduct](CODE_OF_CONDUCT.md).

## How to contribute

1. Open or comment on an [issue](https://github.com/ichirio/tflplanner/issues)
   to report a bug or propose a change (the issue forms guide you:
   *Bug report* / *Feature request*).
2. For anything non-trivial, agree the approach on the issue first.
3. Fork the repository, create a topic branch, make the change with its
   tests, and open a pull request against `main`.
4. Address review feedback; once CI is green and a maintainer approves,
   it is merged.

## Before you open a pull request

- `devtools::test()` passes and `devtools::check()` shows no errors or
  warnings.
- `lintr::lint_package()` reports nothing (the configuration is `.lintr`).
- R source files are ASCII: write non-ASCII text as `\uXXXX` escapes.
  The app's Japanese strings live in `inst/i18n/strings.csv`.
- Add a line to `NEWS.md` for a user-visible change.

tflplanner builds on [rtfreporter](https://github.com/ichirio/rtfreporter);
a change the table layout needs belongs there.
