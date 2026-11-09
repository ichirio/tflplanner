# A study's own ARD functions

The functions defined in the R files the study's ARD programs load (its
ARD definition's study key `source`) and in its
`programs/ard/functions/` (loaded or not), read – not run – with
[`tflspec::tfl_ard_function_info()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_info.html).

## Usage

``` r
study_ard_functions(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame, one row a function: `name`, `file` (relative to the study
folder), `loaded`, `title`, `description`, `stat_names`, `keywords` (as
[`company_ard_functions()`](https://ichirio.github.io/tflplanner/reference/company_ard_functions.md)).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18522dc3ac is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18522dc3ac
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18522dc3ac/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18522dc3ac/studies/SAMPLE-01
study_ard_functions(s)[, c("name", "file", "loaded")]
#>             name                                    file loaded
#> 1 ard_cv_geomean programs/ard/functions/ard_cv_geomean.R   TRUE
# }
options(old)
```
