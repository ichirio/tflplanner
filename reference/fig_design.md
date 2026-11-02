# A figure's design (the Plot Designer)

`fig_design()` gives a figure's design, `NULL` when it has none (its
plot is written by hand); `set_fig_design()` sets it, or with `NULL`
drops it. A design is tflspec's
[`tflspec::tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.html)
(see also
[`tflspec::tfl_fig_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.html)).
With a design, the figure's program makes its plot from it
([`program_code()`](https://ichirio.github.io/tflplanner/reference/program_code.md)).

## Usage

``` r
fig_design(x, output_id)

set_fig_design(x, output_id, design)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The figure.

- design:

  A `tfl_fig_design`, or `NULL`.

## Value

`fig_design()`: a `tfl_fig_design` or `NULL`; `set_fig_design()`: `x`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba19ef782a is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba19ef782a
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba19ef782a/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba19ef782a/studies/SAMPLE-01
x <- s$planner
fig_design(x, "F-14-2-1")  # NULL: this figure is written by hand
#> NULL
x <- set_fig_design(x, "F-14-2-1", tflspec::tfl_fig_template("km_simple"))
class(fig_design(x, "F-14-2-1"))
#> [1] "tfl_fig_design"
# }
options(old)
```
