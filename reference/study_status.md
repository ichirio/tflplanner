# What each report of a study has produced

One row per report: its program (`missing`; `current`, `todo` or
`generated` – untouched since tflplanner wrote it, `generated` meaning
the next save rewrites it; or `edited` by hand), whether its ARD and RTF
exist and when they were made, and a `status`:

## Usage

``` r
study_status(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame.

## Details

- `no program` – save the study to write it

- `unsaved` – the definition changed since the program was written; save
  the study

- `todo` – the program's data part is still to be written

- `not run` – no RTF yet

- `error` – the last run failed (see its log)

- `outdated` – the report's program, or a file it sources (the figure
  setup, say), changed after the RTF was made; or the report was made
  with another `programs/study_setup.R` than the one there now (its
  program records it in `output/tfl/report_status.csv`); or – a figure
  printing the numbers of an ARD (its own, or a table's) – that ARD's
  definition is not the one it was made from, or that ARD is not made
  from its definition now. The program holds the report's whole
  definition, so a change to the definition reaches the reports it is
  about and no others.

- `ok`

`why` says what made a report `outdated`: `program` (its program or a
file it sources is newer), `setup` (the study setup changed), or
`ard:<id>` (the ARD of `<id>` changed, or is to be made again).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba59ac87 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba59ac87
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba59ac87/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba59ac87/studies/SAMPLE-01
# nothing is saved or run yet
head(study_status(s)[, c("output_id", "status")])
#>   output_id  status
#> 1  T-14-0-1 not run
#> 2  T-14-1-1 not run
#> 3 T-14-1-1S not run
#> 4  T-14-1-2 not run
#> 5  T-14-1-3 not run
#> 6  T-14-1-4 not run
# }
options(old)
```
