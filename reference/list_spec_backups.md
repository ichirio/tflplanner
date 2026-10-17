# Restore a study's definition from a backup

Every import (and a change taken in when a study is opened) keeps the
definition files as they were in `spec/.backup/<date>-<time>/`.
`list_spec_backups()` lists them, newest first; `restore_spec_backup()`
reads one like an import (so the same checks, and the study as it is now
is kept in `spec/.backup/` first) – the way to undo a wrong edit.
`preview_spec_backup()` says what restoring would change, taking nothing
in.

## Usage

``` r
list_spec_backups(study, home = tflplanner_home())

preview_spec_backup(study, backup, home = tflplanner_home())

restore_spec_backup(study, backup, parts = NULL, home = tflplanner_home())
```

## Arguments

- study:

  A registered study's id, or an `rtfstudy`.

- home:

  tflplanner's home.

- backup:

  The backup's name (`list_spec_backups()$name`).

- parts:

  The parts to take in; `NULL`: every part that differs.

## Value

`list_spec_backups()`: a data frame (`name`, `time`, `files`).
`preview_spec_backup()`: as
[`preview_spec_import()`](https://ichirio.github.io/tflplanner/reference/export_spec_files.md).
`restore_spec_backup()`: as
[`import_spec_files()`](https://ichirio.github.io/tflplanner/reference/export_spec_files.md).
