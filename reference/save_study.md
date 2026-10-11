# Save a study

Saves the study's state in tflplanner's home (the copy
[`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
reads; the one before goes to its history), then writes the study folder
from it: the definition workbooks in `spec/` (with `output_path` and
`program_dir` set to the study's own folders), the report programs,
`autoexec_report.R`, `programs/study_setup.R` (made when missing;
otherwise its tflplanner part only, see
[`study_setup_code()`](https://ichirio.github.io/tflplanner/reference/study_setup_code.md)),
and `study.yml`. The programs are the definition's: each is written from
it whenever it changes. One edited by hand since (its banner's checksum
no longer matches) is written again too, its edited copy first put in
`programs/.edited/` (named after its place and the time) – what it
changed belongs in the definition (the data code, a user-code report, a
custom analysis, where / derive).

## Usage

``` r
save_study(
  study,
  home = tflplanner_home(),
  base = NULL,
  spec = c("check", "overwrite")
)
```

## Arguments

- study:

  An `rtfstudy`.

- home:

  tflplanner's home.

- base:

  The study as it was opened (or last saved) by whoever saves now. Given
  it, the save merges: a part (the study fields, the report list, each
  sheet, each sheet of the ARD definition ...) this person did not
  change keeps what is saved now – someone else may have changed it –
  and a part both changed differently is a conflict that stops the save
  (class `tflplanner_conflict`).

  The definition files are the study's source (#274): a save never
  writes over one changed outside tflplanner since it last wrote or read
  them. Such a save stops (class `tflplanner_spec_changed`, its `files`
  the files changed): open the study again, or
  [`reload_from_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md),
  to take the files in; or write them back from the last save
  ([`write_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)).

- spec:

  `"check"` (the default) stops when a definition file was changed
  outside tflplanner; `"overwrite"` writes over it, after copying it to
  `spec/.rejected/` (what
  [`write_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)
  does).

## Value

The study, invisibly, with `files`: what was written (status `written`,
`rewritten` for a program edited by hand, `unchanged`, ...).
