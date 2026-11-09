# What went wrong inside the analyses while a study's ARD was made

The errors and warnings the study ARD (`output/ard/ard.rds`, as last
made) keeps, and those of the ARDs reports take in (`input/ard/`), one
row each, errors first
([`tflspec::tfl_ard_conditions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_conditions.html)).
An analysis with an error has no statistics in the ARD: its cells are
blank in the table.

## Usage

``` r
study_ard_conditions(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame (`output_id`, `analysis_id`, `variable`, `groups`, `level`,
`message`, `statistics`, `source`: `""` for the study ARD, the file's
name for an ARD taken in); no rows when nothing went wrong or there is
no ARD yet.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b187083dd4b is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b187083dd4b
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b187083dd4b/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b187083dd4b/studies/SAMPLE-01
study_ard_conditions(s)  # no rows: nothing went wrong, or no ARD yet
#> [1] output_id   analysis_id variable    groups      level       message    
#> [7] statistics  source     
#> <0 rows> (or 0-length row.names)
# }
options(old)
```
