# Use an ARD taken in for a report, or stop using it

Sets the report row's `ard_source`: `import:<file>` (the file in the
study's `input/ard/`, see
[`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md))
or blank (the report's own ARD definition). The report's program then
reads that file.

## Usage

``` r
use_imported_ard(x, output_id, file = NULL)
```

## Arguments

- x:

  A `tflplanner` (`study$planner`).

- output_id:

  The report.

- file:

  A `file` of
  [`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md);
  `NULL` to go back to the report's own ARD definition.

## Value

The `tflplanner`.
