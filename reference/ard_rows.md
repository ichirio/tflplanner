# A report's rows of the ARD definition

`ard_rows()` gives the rows of an ARD definition sheet the app shows for
a report (`analyses`: that report's; the other sheets are the study's);
`set_ard_rows()` puts them back, edited.

## Usage

``` r
ard_rows(x, sheet, output_id = "")

set_ard_rows(x, sheet, output_id = "", rows)
```

## Arguments

- x:

  A `tflplanner`.

- sheet:

  `analyses`, `datasets`, `populations` or `study`.

- output_id:

  A report, or `""` / `NA` for every row.

- rows:

  The rows, edited.

## Value

`ard_rows()`: a data frame; `set_ard_rows()`: the `tflplanner`.
