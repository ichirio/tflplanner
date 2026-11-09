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
  spec = c("check", "overwrite"),
  fill_widths = FALSE
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

- fill_widths:

  `TRUE` (the app's save): a report new or changed in this save that
  says no column widths gets the default ones
  ([`tflspec::tfl_fill_widths()`](https://ichirio.github.io/tflspec/reference/tfl_fill_widths.html)),
  and a table whose columns changed has its old widths replaced.

## Value

The study, invisibly, with `files`: what was written (status `written`,
`rewritten` for a program edited by hand, `unchanged`, ...).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01
s <- save_study(s)  # writes the definition files and the programs
head(s$files)
#>                                                                                      file
#> 1             /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/data.yml
#> 2         /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/defaults.yml
#> 3 /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/outputs/F-14-2-1.yml
#> 4 /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/outputs/F-14-2-2.yml
#> 5 /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/outputs/F-14-2-3.yml
#> 6 /tmp/RtmpTnNlOL/tflplanner-home1b183ddc8519/studies/SAMPLE-01/spec/outputs/F-14-2-4.yml
#>      status
#> 1 unchanged
#> 2 unchanged
#> 3 unchanged
#> 4 unchanged
#> 5 unchanged
#> 6 unchanged
# }
options(old)
```
