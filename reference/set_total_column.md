# A Total column for a report

Switches a table's Total column on or off, in both halves of its
definition at once: the `tables` sheet's `total` (the heading) and
`total_position`, written as `plan_total()`; and `overall = TRUE` on the
report's own analyses grouped by the column variable, which makes each
ARD program run those analyses again without their `by` – cards' own
overall rows, with no group, which the table reads as the column and the
ARS writes as an analysis without the grouping. No `"Total"` value of
the arm is made up in the data.

## Usage

``` r
set_total_column(x, output_id, label = "Total", position = c("last", "first"))
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- label:

  The column's heading (`"Total"`); `NULL` or `""` switches the column
  off.

- position:

  Where it goes among the column variable's values.

## Value

The `tflplanner`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba56d355df is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba56d355df
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba56d355df/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba56d355df/studies/SAMPLE-01
x <- set_total_column(s$planner, "T-14-1-1", "All subjects")  # switch it on
x$sheets$tables[x$sheets$tables$output_id == "T-14-1-1", c("output_id", "total", "total_position")]
#>   output_id        total total_position
#> 2  T-14-1-1 All subjects           <NA>
x <- set_total_column(x, "T-14-1-1", NULL)  # and off again
# }
options(old)
```
