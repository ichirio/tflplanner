# The company's own ARD functions

The functions defined in the R files of the standards folder's
`ard_functions/` (`<home>/standards/ard_functions/*.R`), read – not run
– with
[`tflspec::tfl_ard_function_info()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_info.html):
a plain `name <- function` and one written as
[`cards::as_cards_fn()`](https://pharmaverse.github.io/cards/latest-tag/reference/as_cards_fn.html)
(the templates of
[`tflspec::tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.html))
alike; test files (`test-*.R`) are not read. An analysis names one as
its `method` once the study uses it
([`use_company_ard_function()`](https://ichirio.github.io/tflplanner/reference/use_company_ard_function.md)).

## Usage

``` r
company_ard_functions(home = tflplanner_home())
```

## Arguments

- home:

  The tflplanner home.

## Value

A data frame, one row a function: `name`, `file`, `title`, `description`
(from the roxygen block above it), `stat_names` (the statistics it
declares, `|` between them), `keywords` (the words the ARD form's search
finds it by: the comment lines
`# tflplanner-keywords: odds ratio, PROC LOGISTIC` right above it;
`", "` between them).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18740c92b0 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18740c92b0
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18740c92b0/studies
# write a function from tflspec's template into the company's folder
d <- file.path(tflplanner_home(), "standards", "ard_functions")
dir.create(d, recursive = TRUE)
tflspec::tfl_ard_function_template("ard_cv_geomean", type = "summary",
                                   file = file.path(d, "ard_cv_geomean.R"))
company_ard_functions()[c("name", "title", "stat_names")]
#>             name                                        title    stat_names
#> 1 ard_cv_geomean Statistics of one's own on numeric variables cv | geo_mean
options(old)
```
