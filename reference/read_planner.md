# Read the definition workbooks

Reads `table_spec.xlsx` and `report_spec.xlsx` (or any rtfreporter
definition workbooks, one or several) through
[`tflspec::tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.html),
so what the app opens is exactly what the report programs will read. The
report list and the data code come from the `_tflplanner` sheet when a
workbook has one; otherwise the list is every report a sheet names.

## Usage

``` r
read_planner(path)
```

## Arguments

- path:

  One or more `.xlsx` files.

## Value

An `tflplanner` object.
