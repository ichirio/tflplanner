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

fill_tables(x, output_id, meta)
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
