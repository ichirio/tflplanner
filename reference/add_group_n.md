# Count the subjects per group for a report's column headers

Adds to the report's ARD analyses the subjects per group, which clinical
reporting calls big N (`GROUPN`: the group counted, by nothing) – what
the column headers' `(N={n})` read – with the data and analysis set of
its first analysis.

## Usage

``` r
add_group_n(x, output_id, group)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- group:

  The group column (`TRT01A`).

## Value

The `tflplanner`.
