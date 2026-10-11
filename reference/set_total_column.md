# A Total column for a report

Switches a table's Total column on or off, in both halves of its
definition at once: the `tables` sheet's `total` (the heading) and
`total_position`, written as `plan_total()`; and `overall = TRUE` on the
report's own analyses grouped by the column variable, which makes each
ARD program run those analyses again without their `by` – cards' own
overall rows, with no group, which the table reads as the column and the
ARS writes as an analysis without the grouping. No `"Total"` value of
the arm is made up in the data.

## Usage

``` r
set_total_column(x, output_id, label = "Total", position = c("last", "first"))
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- label:

  The column's heading (`"Total"`); `NULL` or `""` switches the column
  off.

- position:

  Where it goes among the column variable's values.

## Value

The `tflplanner`.
