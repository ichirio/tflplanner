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

builder_write(x, output_id, state, was = NULL)
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

- was:

  What `builder_read()` returned before the edit, or `NULL`. Given it,
  only what differs from it is written: what the builder does not show
  (a statistic's own template, a condition, an order of one's own) stays
  as the sheets have it. `NULL` writes the whole description.

## Value

`builder_read()`: a list – `key` (the column variables, outermost
first), `arms` (a named list: each column variable's levels in order),
`variables` (a data frame: `variable`, `kind`, `label`), `levels` (named
list), `rows` (a continuous variable's row labels in order) and their
`templates`, `value` (`stat`: the numbers, rounded by `digits`;
`stat_fmt`: the ARD's text), `digits` (each statistic the rows print:
its decimals), `exceptions` (a variable's own: `variable`, `statistic`,
`digits`), `cat_format` (`npct`, `nNpct`, `n`), `pct_decimals`, `header`
(`keep` or a name of
[`header_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md)),
and for a hierarchy table (SOC / PT) its `hierarchy` (the ARD's terms,
outermost first), `row_keys` (the table's rows, outermost first: by
default the ARD's other groups, then its hierarchy), `row_choices` (the
keys it may take), `row_pages` (the outermost a page each), `sort` (each
row key's order, outermost first – `"alpha"` by name, or a code list's
order; `"freq"` most frequent first, ties by name – or `"own"`: an order
of the sheets' own, left as it is) and `overall_label` (the "any event"
row's name, `tables$overall_label`). `builder_write()`: the
`tflplanner`.
