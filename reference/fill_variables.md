# Fill a report's definition from its ARD

`fill_variables()` gives every column key and analysis variable of the
ARD a `variables` row: its levels in the ARD's order (for a categorical
variable, and a key with no more than `max_levels` levels), its label
when the ARD carries one, and a running `order` for the analysis
variables. Rows already there are kept; only their blank `levels` and
`label` are filled. `fill_tables()` fills the blank cells of the
report's `tables` row: the column key, and the hierarchy (`rows`,
`label`, `sort`) or `group = variable`.

## Usage

``` r
fill_variables(
  x,
  output_id,
  meta,
  max_levels = as.integer(.std_setting("max_levels", "30"))
)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- meta:

  Its
  [`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md).

- max_levels:

  Keys with more levels than this (a preferred term) get no `levels`.

## Value

The `tflplanner`, with attribute `changed` (rows added or filled).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba3b2bb268 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba3b2bb268
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba3b2bb268/studies
# \donttest{
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1")
  x <- add_output(new_planner(), "T-14-1-1", type = "table")
  x <- fill_variables(x, "T-14-1-1", ard_meta(ard))
  x$sheets$variables[c("variable", "levels")]  # the levels, in the ARD's order
}
#>   variable                                               levels
#> 1      ARM Placebo | Xanomeline High Dose | Xanomeline Low Dose
#> 2   AGEGR1                                    65-80 | <65 | >80
# }
options(old)
```
