# Try one of the company's ARD functions

Loads its file and runs
[`tflspec::tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.html)
on `data`, with the arguments an analysis row would give.

## Usage

``` r
check_company_ard_function(
  name,
  data,
  ...,
  stat_names = NULL,
  home = tflplanner_home()
)
```

## Arguments

- name:

  The function.

- data:

  The data to try it on.

- ...:

  Its other arguments (`by = TRT01A, variables = AGE`).

- stat_names:

  The statistics it should give.

- home:

  The tflplanner home.

## Value

The problems found
([`tflspec::tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.html)).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b183ce4770c is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b183ce4770c
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b183ce4770c/studies
# \donttest{
if (requireNamespace("cards", quietly = TRUE)) {
  d <- file.path(tflplanner_home(), "standards", "ard_functions")
  dir.create(d, recursive = TRUE)
  tflspec::tfl_ard_function_template("ard_cv_geomean", type = "summary",
                                     file = file.path(d, "ard_cv_geomean.R"))
  # no problem found: no rows
  check_company_ard_function("ard_cv_geomean", cards::ADSL,
                             by = ARM, variables = AGE)
}
#> [1] level   check   message
#> <0 rows> (or 0-length row.names)
# }
options(old)
```
