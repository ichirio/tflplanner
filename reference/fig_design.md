# A figure's design (the Plot Designer)

`fig_design()` gives a figure's design, `NULL` when it has none (its
plot is written by hand); `set_fig_design()` sets it, or with `NULL`
drops it. A design is tflspec's
[`tflspec::tfl_fig_design()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.html)
(see also
[`tflspec::tfl_fig_template()`](https://ichirio.github.io/tflspec/reference/tfl_fig_templates.html)).
With a design, the figure's program makes its plot from it
([`program_code()`](https://ichirio.github.io/tflplanner/reference/program_code.md)).

## Usage

``` r
fig_design(x, output_id)

set_fig_design(x, output_id, design)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The figure.

- design:

  A `tfl_fig_design`, or `NULL`.

## Value

`fig_design()`: a `tfl_fig_design` or `NULL`; `set_fig_design()`: `x`.
