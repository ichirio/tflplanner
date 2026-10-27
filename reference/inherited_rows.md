# The study default rows a report inherits

The study default rows a report inherits

## Usage

``` r
inherited_rows(x, sheet, output_id)
```

## Arguments

- x:

  An `tflplanner`.

- sheet:

  A sheet.

- output_id:

  The report.

## Value

The default rows (blank `output_id`) that still apply to the report:
those it has no row of its own for. On a sheet with one row per report
(`tables`, `layout`, ...) that is the default row, whose cells the
report's own non-blank cells override.
