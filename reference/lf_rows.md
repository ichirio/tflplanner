# A listing's or figure's definition

`lf_rows()` gives a report's rows of `listings`, `listing_cols` or
`figures`; `set_lf_rows()` puts them back, edited. `listing_types()` are
the listing types of the company standards (rtfreporter's).

## Usage

``` r
lf_rows(x, sheet, output_id)

set_lf_rows(x, sheet, output_id, rows)

listing_types()
```

## Arguments

- x:

  A `tflplanner`.

- sheet:

  `listings`, `listing_cols` or `figures`.

- output_id:

  The report.

- rows:

  The rows, edited.

## Value

`lf_rows()`: a data frame; `set_lf_rows()`: the `tflplanner`;
`listing_types()`: a data frame (`type`, `label`, `note`).
