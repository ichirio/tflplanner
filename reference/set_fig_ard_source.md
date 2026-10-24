# Where a figure's ARD comes from

Sets the figure's report row `ard_source`: its own analyses (`"own"`), a
table's ARD (`"table:<output_id>"`), or none (`NULL`). An ARD taken in
is
[`use_imported_ard()`](https://ichirio.github.io/tflplanner/reference/use_imported_ard.md).
The figure's program then reads that ARD as `ard`, for its design's
`ard_stats` steps and `ard_number` layers (see
[`tflspec::tfl_fig_parts()`](https://ichirio.github.io/tflspec/reference/tfl_fig_parts.html)).

## Usage

``` r
set_fig_ard_source(x, output_id, source = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The figure.

- source:

  `"own"`, `"table:<output_id>"` (a table of the study), or `NULL`.

## Value

`x`.
