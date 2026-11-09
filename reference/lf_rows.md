# A listing's or figure's definition

`lf_rows()` gives a report's rows of `listings`, `listing_cols` or
`figures`; `set_lf_rows()` puts them back, edited. `listing_types()` are
the listing types of the company standards (rtfreporter's).

## Usage

``` r
lf_rows(x, sheet, output_id)

set_lf_rows(x, sheet, output_id, rows)
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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18c1e7b21 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18c1e7b21
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18c1e7b21/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18c1e7b21/studies/SAMPLE-01
rows <- lf_rows(s$planner, "listings", "L-16-2-7")
rows[, c("output_id", "type", "dataset", "max_rows")]
#>   output_id      type dataset max_rows
#> 1  L-16-2-7 multiline    ADAE       22
# change a row and put it back
rows$max_rows <- 30
p <- set_lf_rows(s$planner, "listings", "L-16-2-7", rows)
lf_rows(p, "listings", "L-16-2-7")$max_rows
#> [1] "30"
# }
options(old)
```
