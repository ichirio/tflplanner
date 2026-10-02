# Save a study

Saves the study's state in tflplanner's home (the copy
[`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
reads; the one before goes to its history), then writes the study folder
from it: the definition workbooks in `spec/` (with `output_path` and
`program_dir` set to the study's own folders), the report programs,
`autoexec_report.R`, and `study.yml`. A program tflplanner wrote and
nobody has touched since (its banner's checksum still matches) follows
the definition and is rewritten when it changes; one edited by hand is
kept unless it is named in `regenerate`.

## Usage

``` r
save_study(
  study,
  regenerate = character(),
  home = tflplanner_home(),
  base = NULL
)
```

## Arguments

- study:

  An `rtfstudy`.

- regenerate:

  Report ids whose program is written anew.

- home:

  tflplanner's home.

- base:

  The study as it was opened (or last saved) by whoever saves now. Given
  it, the save merges: a part (the study fields, the report list, each
  sheet, each sheet of the ARD definition ...) this person did not
  change keeps what is saved now – someone else may have changed it –
  and a part both changed differently is a conflict that stops the save
  (class `tflplanner_conflict`).

## Value

The study, invisibly, with `files`: what was written or kept.
