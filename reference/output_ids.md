# Report ids the definition names

The report list first, then any id a sheet or the ARD definition names
that the list lacks.

## Usage

``` r
output_ids(x)
```

## Arguments

- x:

  An `tflplanner`.

## Value

A character vector.

## Examples

``` r
x <- add_output(new_planner(), "T-14-1-1", type = "table")
x <- add_output(x, "F-14-2-1", type = "figure")
output_ids(x)
#> [1] "T-14-1-1" "F-14-2-1"
```
