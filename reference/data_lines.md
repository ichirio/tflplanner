# The data part of a report's program

The code a report runs before the report is laid out: the setup every
report runs, the report's own `data_code` (its ARD) and, for a table,
its `process_code` (normalization and rework, by default
`data <- normalize_ard(ard)`). With no `data_code` it is a TODO that
stops.
[`program_code()`](https://ichirio.github.io/tflplanner/reference/program_code.md)
writes it into the program, and
[`fetch_ard()`](https://ichirio.github.io/tflplanner/reference/fetch_ard.md)
runs it to learn what the ARD holds.

## Usage

``` r
data_lines(x, output_id, todo = TRUE)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- todo:

  `FALSE` returns `NULL` instead of the TODO.

## Value

Code lines.
