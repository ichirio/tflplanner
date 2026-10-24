# The packages tflplanner needs, and their versions

Lists the packages tflplanner needs (rtfreporter, tflspec, tflplanner
itself) and the ones its programs use when they are there (cards, cardx,
dplyr, ggplot2, ...), with the version installed and – when `check` –
the newest one.

## Usage

``` r
tflplanner_packages(check = TRUE, channel = NULL)
```

## Arguments

- check:

  Look up the newest versions (needs the internet; a version that cannot
  be found is `NA`).

- channel:

  For rtfreporter, tflspec and tflplanner: `"release"` (CRAN when the
  package is there, else its latest GitHub release) or `"dev"` (GitHub
  main). `NULL` uses the channel last used by
  [`update_tflplanner()`](https://ichirio.github.io/tflplanner/reference/update_tflplanner.md).

## Value

A data frame: `package`, `role` (`"required"` / `"suggested"`),
`installed`, `latest`, `status` (`"missing"`, `"update"`, `"ok"`, or
`NA` when not checked).

## Examples

``` r
tflplanner_packages(check = FALSE)
#>          package      role   installed latest status
#> 1    rtfreporter  required  0.8.2.9097   <NA>   <NA>
#> 2        tflspec  required 0.0.24.9234   <NA>   <NA>
#> 3     tflplanner  required  0.0.2.9282   <NA>   <NA>
#> 4          arrow suggested      25.0.1   <NA>   <NA>
#> 5          broom suggested      1.0.13   <NA>   <NA>
#> 6  broom.helpers suggested      1.23.0   <NA>   <NA>
#> 7          cards suggested       0.9.0   <NA>   <NA>
#> 8          cardx suggested       0.3.4   <NA>   <NA>
#> 9       chromote suggested       0.5.1   <NA>   <NA>
#> 10         dplyr suggested       1.2.1   <NA>   <NA>
#> 11       ggplot2 suggested       4.0.3   <NA>   <NA>
#> 12     ggsurvfit suggested       1.2.1   <NA>   <NA>
#> 13         haven suggested       2.5.5   <NA>   <NA>
#> 14        httpuv suggested      1.6.17   <NA>   <NA>
#> 15         logrx suggested       0.4.0   <NA>   <NA>
#> 16     patchwork suggested       1.3.2   <NA>   <NA>
#> 17      survival suggested       3.8-6   <NA>   <NA>
#> 18         tcltk suggested       4.6.1   <NA>   <NA>
#> 19          xml2 suggested       1.6.0   <NA>   <NA>
```
