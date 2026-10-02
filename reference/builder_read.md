# Read and write a table the builder way

`builder_read()` describes a report's summary table from its sheets (and
what its ARD holds, for what the sheets do not say yet);
`builder_write()` writes such a description back to the report's own
rows of `tables`, `variables`, `cells` and `col_header`. Rows it does
not manage (a statistic it does not offer, a variable of its own
template) are left as they are.

## Usage

``` r
builder_read(x, output_id, meta = NULL)

builder_write(x, output_id, state)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- meta:

  Its
  [`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md),
  or `NULL`.

- state:

  What `builder_read()` returns, as edited.

## Value

`builder_read()`: a list – `key` (the column variables, outermost
first), `arms` (a named list: each column variable's levels in order),
`variables` (a data frame: `variable`, `kind`, `label`), `levels` (named
list), `stats` (keys of
[`builder_stats()`](https://ichirio.github.io/tflplanner/reference/builder_stats.md)
in order), `decimals`, `cat_format` (`npct`, `nNpct`, `n`),
`pct_decimals`, `header` (`keep` or a name of
[`header_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md)).
`builder_write()`: the `tflplanner`.
