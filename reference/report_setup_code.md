# The study's setup of its report programs

`programs/tfl/report_setup.R`, which every report program sources: the
study's setup (`programs/study_setup.R`, see
[`study_setup_code()`](https://ichirio.github.io/tflplanner/reference/study_setup_code.md)),
the reports' font and size (`options(rtfreporter.font = )`), the study's
tokens (`options(rtfreporter.tokens = )`: the company, the analysis, the
protocol ...), its running header and footer (`study_header`,
`study_footer`), written once from `report_spec.xlsx`
([`tflspec::tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.html)).
Each report program then says only its own tokens (`OUTPUT_LABEL`,
`OUTPUT_TITLE` ...), and ends with `record_report()`, which records the
report as made, with the fingerprint of the study setup, in
`output/tfl/report_status.csv`.

## Usage

``` r
report_setup_code(x, date = Sys.Date())
```

## Arguments

- x:

  A `tflplanner`.

- date:

  The date stamped in the banner.

## Value

The program, one element per line.
