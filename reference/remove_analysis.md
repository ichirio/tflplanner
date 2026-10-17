# Delete one analysis of a report

Takes an analysis out of a report's ARD definition: one of its own, or
one inside a stack (the stack keeps the others). A stack itself is
deleted with its own Delete button (`stack_remove()`), which says what
becomes of the analyses inside it.

## Usage

``` r
remove_analysis(x, output_id, id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- id:

  The analysis.

## Value

`x` without the analysis.
