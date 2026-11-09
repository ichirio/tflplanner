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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b184e817a2f is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b184e817a2f
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b184e817a2f/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b184e817a2f/studies/SAMPLE-01
# no import yet, so no backup: an empty list
list_spec_backups(s)
#> [1] name  time  files
#> <0 rows> (or 0-length row.names)
# }
options(old)
```
