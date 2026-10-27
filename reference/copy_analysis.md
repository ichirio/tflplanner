# Copy one analysis of a report

A new analysis, the same as one of the report's, right after it; a stack
with the analyses inside it (under new ids too). The new id is the old
one with `_2` (or the next number free).

## Usage

``` r
copy_analysis(x, output_id, id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- id:

  The analysis.

## Value

`x` with the copy; its id in `attr(, "copied")`.
