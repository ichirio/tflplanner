# The same figure for other parameters

Copies a designed figure once per parameter: each copy is a new figure
report
([`copy_output()`](https://ichirio.github.io/tflplanner/reference/add_output.md):
layout and design) whose design keeps every piece but the parameter (its
`param` step), and whose description says the new parameter where it
said the old one.

## Usage

``` r
copy_fig_to_params(x, output_id, params, pattern = "{id}-{param}")
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The designed figure.

- params:

  The PARAMCDs, one figure each.

- pattern:

  The new IDs: `{id}` = `output_id`, `{param}` = the parameter.

## Value

`x` with the new figures.
