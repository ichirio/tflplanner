# A report's own code for a step

A report's data step and its content step are each the definition (the
GUI) or the report's own R: `step_code()` is that code (`NA`: the
definition makes the step), kept in the report sheet's `data_code` /
`content_code` (see
[`tflspec::tfl_check_code()`](https://ichirio.github.io/tflspec/reference/tfl_check_code.html)
for the rules the code follows). `report_datasets()` are the datasets
the report registers – the ones a step of its own code reads (the
program reads them first, by their names in lower case).

## Usage

``` r
step_code(x, output_id, step = c("data", "content"))

set_step_code(x, output_id, step = c("data", "content"), code)

report_datasets(x, output_id)

set_report_datasets(x, output_id, datasets)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  A report.

- step:

  `"data"` or `"content"`.

- code:

  The code, or `NA` / `""` (the definition makes the step).

- datasets:

  Dataset names (`"ADSL"`, `"ADVS"`).

## Value

`step_code()`: the code or `NA`; `report_datasets()`: the names; the
setters: `x`.

## Examples

``` r
p <- add_output(new_planner(), "F-1", type = "figure")
p <- set_report_datasets(p, "F-1", c("ADSL", "ADVS"))
p <- set_step_code(p, "F-1", "content",
                   "content <- ggplot2::ggplot(advs) + ggplot2::geom_point()")
step_code(p, "F-1", "content")
#> [1] "content <- ggplot2::ggplot(advs) + ggplot2::geom_point()"
report_datasets(p, "F-1")
#> [1] "ADSL" "ADVS"
```
