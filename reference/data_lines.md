# The data part of a report's program

The code a report runs before the report is laid out: the setup every
report runs, then its data and its content as its definition makes them
or as its own code does
([`step_code()`](https://ichirio.github.io/tflplanner/reference/step_code.md):
the datasets it registers, its ARD as `ard`, the code) – a table's ARD
and its `process_code` (normalization and rework; by default none: the
table's plan normalizes `ard`). With nothing to make it, a TODO that
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
