# Delete one analysis of a report

Takes an analysis out of a report's ARD definition: one of its own, or
one inside a stack (the stack keeps the others). A stack itself is
deleted with its own Delete button (`stack_remove()`), which says what
becomes of the analyses inside it.

## Usage

``` r
remove_analysis(x, output_id, id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- id:

  The analysis.

## Value

`x` without the analysis.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba2e2c63b7 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba2e2c63b7
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba2e2c63b7/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba2e2c63b7/studies/SAMPLE-01
a <- ard_rows(s$planner, "analyses", "T-14-1-1")$analysis_id
# take the last analysis out
x <- remove_analysis(s$planner, "T-14-1-1", a[length(a)])
ard_rows(x, "analyses", "T-14-1-1")$analysis_id
#> [1] "GROUPN" "CONT"  
# }
options(old)
```
