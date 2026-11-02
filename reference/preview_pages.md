# The table as it will print

Plans the report's table from the definition as it stands (saved or not)
and the report's normalized data, and gives its pages – what
[`tflspec::tfl_report()`](https://ichirio.github.io/tflspec/reference/tfl_report.html)
lays out – or an HTML rendering of them.

## Usage

``` r
preview_pages(x, output_id, data)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- data:

  Its normalized data (`ard_data()`).

## Value

A list of `rtftable` pages.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1abaf10d58e is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1abaf10d58e
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1abaf10d58e/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1abaf10d58e/studies/SAMPLE-01
s <- save_study(s)
invisible(update_study_ard(s, "T-14-1-1"))  # the report's ARD, made by its program
ard <- rtfreporter::normalize_ard(study_ard(s, "T-14-1-1"))
pages <- preview_pages(s$planner, "T-14-1-1", ard)
head(pages[[1]]$data)
#>          row_label      Drug X     Control
#> 1      Age (years)        <NA>        <NA>
#> 2                n          80          78
#> 3        Mean (SD) 60.1 (9.59) 62.7 (9.76)
#> 4           Median        59.5        62.5
#> 5         Min, Max      39, 82      42, 85
#> 6 Age group, n (%)        <NA>        <NA>
# }
options(old)
```
