# A new, empty study definition

A new, empty study definition

## Usage

``` r
new_planner(study = NULL)
```

## Arguments

- study:

  Named study keys (`rounding`, `output_path`, `program_dir`).

## Value

An `tflplanner` object.

## Examples

``` r
p <- new_planner(c(rounding = "sas"))
p <- add_output(p, "DM", description = "Demographics")
p
#> <tflplanner> 1 report(s): DM
```
