# Where a figure's ARD comes from

Sets the figure's report row `ard_source`: its own analyses (`"own"`), a
table's ARD (`"table:<output_id>"`), or none (`NULL`). An ARD taken in
is
[`use_imported_ard()`](https://ichirio.github.io/tflplanner/reference/use_imported_ard.md).
The figure's program then reads that ARD as `ard`, for its design's
`ard_stats` steps and `ard_number` layers (see
[`tflspec::tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.html)).

## Usage

``` r
set_fig_ard_source(x, output_id, source = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The figure.

- source:

  `"own"`, `"table:<output_id>"` (a table of the study), or `NULL`.

## Value

`x`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba4b1bc1d9 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba4b1bc1d9
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba4b1bc1d9/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba4b1bc1d9/studies/SAMPLE-01
# the figure reads the ARD of a table of the study
p <- set_fig_ard_source(s$planner, "F-14-2-1", "table:T-14-2-1")
p$sheets$report[which(p$sheets$report$output_id == "F-14-2-1"),
                c("output_id", "ard_source")]
#>   output_id     ard_source
#> 9  F-14-2-1 table:T-14-2-1
# }
options(old)
```
