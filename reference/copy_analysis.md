# Copy one analysis of a report

A new analysis, the same as one of the report's, right after it; a stack
with the analyses inside it (under new ids too). The new id is the old
one with `_2` (or the next number free).

## Usage

``` r
copy_analysis(x, output_id, id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- id:

  The analysis.

## Value

`x` with the copy; its id in `attr(, "copied")`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba796ed914 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba796ed914
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba796ed914/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba796ed914/studies/SAMPLE-01
a <- ard_rows(s$planner, "analyses", "T-14-1-1")$analysis_id
# a copy of the first analysis, right after it
x <- copy_analysis(s$planner, "T-14-1-1", a[1])
attr(x, "copied")
#> [1] "GROUPN_2"
# }
options(old)
```
