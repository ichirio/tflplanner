# Count the subjects per group for a report's column headers

Adds to the report's ARD analyses the subjects per group, which clinical
reporting calls big N (`GROUPN`: the group counted, by nothing) – what
the column headers' `(N={n})` read – with the data and analysis set of
its first analysis.

## Usage

``` r
add_group_n(x, output_id, group)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- group:

  The group column (`TRT01A`).

## Value

The `tflplanner`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba3a5abf7a is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba3a5abf7a
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba3a5abf7a/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba3a5abf7a/studies/SAMPLE-01
x <- add_group_n(s$planner, "T-14-1-1", "TRT01A")  # the subjects per group, for (N=n)
ard_rows(x, "analyses", "T-14-1-1")$analysis_id
#> [1] "GROUPN_" "GROUPN"  "CONT"    "CAT"    
# }
options(old)
```
