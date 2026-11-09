# The same figure for other parameters

Copies a designed figure once per parameter: each copy is a new figure
report
([`copy_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md):
layout and design) whose design keeps every piece but the parameter (its
`param` step), and whose description says the new parameter where it
said the old one.

## Usage

``` r
copy_fig_to_params(x, output_id, params, pattern = "{id}-{param}")
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The designed figure.

- params:

  The PARAMCDs, one figure each.

- pattern:

  The new IDs: `{id}` = `output_id`, `{param}` = the parameter.

## Value

`x` with the new figures.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1834d9b4d4 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1834d9b4d4
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1834d9b4d4/studies
# \donttest{
# (a few seconds: it makes the sample study)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1834d9b4d4/studies/SAMPLE-01
x <- copy_fig_to_params(s$planner, "F-14-2-3", c("SYSBP", "DIABP"))
grep("F-14-2-3", output_ids(x), value = TRUE)  # the figure and its two copies
#> [1] "F-14-2-3"       "F-14-2-3-DIABP" "F-14-2-3-SYSBP"
# }
options(old)
```
