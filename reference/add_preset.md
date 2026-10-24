# Add a preset's rows to a report

Add a preset's rows to a report

## Usage

``` r
add_preset(x, output_id, preset, variable = NULL)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report (`NA` for the study defaults).

- preset:

  A name of
  [`cell_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md)
  or
  [`header_presets()`](https://ichirio.github.io/tflplanner/reference/cell_presets.md).

- variable:

  For a cell preset: the variable its rows are for, instead of the kind
  (`continuous` / `categorical`) that serves every variable of that
  kind.

## Value

The `tflplanner`. A report's own column header replaces the default one
whole, so a header preset replaces the report's header rows.
