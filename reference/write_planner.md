# Write the two definition workbooks

`table_spec.xlsx` gets the table sheets and `rounding`
([`tflspec::tfl_write_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.html));
`report_spec.xlsx` the report sheets, `output_path`, `program_dir`
([`tflspec::tfl_write_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_write_table_spec.html))
and the `_tflplanner` sheet (report list and data code). Each holds only
its own half's sheets; what a column means is a comment on its header
cell
([`tflspec::tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.html)).
Both are checked by
[`tflspec::tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.html)
on the way out.

## Usage

``` r
write_planner(
  x,
  dir,
  table_file = "table_spec.xlsx",
  report_file = "report_spec.xlsx",
  books = c("table", "report")
)
```

## Arguments

- x:

  An `tflplanner`.

- dir:

  Destination folder.

- table_file, report_file:

  File names.

- books:

  Which to write: `"table"`, `"report"` (both by default).

## Value

The two paths, invisibly.
