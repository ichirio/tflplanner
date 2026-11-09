# Use one of the company's ARD functions in a study

Copies the file that defines it into the study's
`programs/ard/functions/` – unless the study has a file of that name,
which wins – and adds it to the ARD definition's study key `source`, so
the study's ARD programs load it. Save the study afterwards.

## Usage

``` r
use_company_ard_function(study, name, home = tflplanner_home())
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function.

- home:

  The tflplanner home.

## Value

The study, its ARD definition's `source` updated.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18b03ba73 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18b03ba73
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18b03ba73/studies
# \donttest{
# (a few seconds: it makes a study)
d <- file.path(tflplanner_home(), "standards", "ard_functions")
dir.create(d, recursive = TRUE)
tflspec::tfl_ard_function_template("ard_cv_geomean", type = "summary",
                                   file = file.path(d, "ard_cv_geomean.R"))
s <- use_company_ard_function(create_study("DEMO-01"), "ard_cv_geomean")
s$planner$ard$study  # its key `source` names the copy in the study
#>      key                                   value
#> 1     id                                 USUBJID
#> 2 output                      output/ard/ard.rds
#> 3 source programs/ard/functions/ard_cv_geomean.R
# }
options(old)
```
