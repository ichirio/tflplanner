# The study's setup of its report programs

`programs/tfl/report_setup.R`, which every report program sources: the
study's setup (`programs/study_setup.R`, see
[`study_setup_code()`](https://ichirio.github.io/tflplanner/reference/study_setup_code.md)),
the reports' font and size (`options(rtfreporter.font = )`), the study's
tokens (`options(rtfreporter.tokens = )`: the company, the analysis, the
protocol ...), its running header and footer (`study_header`,
`study_footer`), written once from the study's defaults,
`spec/defaults.yml`
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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1826cf797b is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1826cf797b
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1826cf797b/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1826cf797b/studies/SAMPLE-01
head(report_setup_code(s$planner), 6)
#> [1] "# ============================================================================"  
#> [2] "#  Program    : programs/tfl/report_setup.R"                                     
#> [3] "#  The study's font, header, footer and tokens: every report program sources it."
#> [4] "#  Generated  : tflplanner 0.0.2.9285, 2026-11-09"                               
#> [5] "#"                                                                               
#> [6] "#  Written from spec/defaults.yml: change them there (the study tab),"           
# }
options(old)
```
