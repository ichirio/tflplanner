# Where each output's ARD stands

For every output the ARD definition has analyses for: `built` (its rows
are in the study ARD, made from the definition as it is now), `outdated`
(made from an earlier definition, or with another
`programs/study_setup.R` than the one there now), `not built`, or
`error` (its last update failed). Many people may work on one study: the
study ARD is updated output by output
([`update_study_ard()`](https://ichirio.github.io/tflplanner/reference/update_study_ard.md)),
and a table is made from whatever of it is there.

## Usage

``` r
ard_status(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame: `output_id`, `analyses`, `state`, `rows`, `built`,
`error`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba7d779ffc is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba7d779ffc
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba7d779ffc/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba7d779ffc/studies/SAMPLE-01
head(ard_status(s)[, c("output_id", "analyses", "state")], 3)  # not built yet
#>   output_id analyses     state
#> 1  T-14-0-1        1 not built
#> 2  T-14-1-1        3 not built
#> 3 T-14-1-1S        3 not built
# }
options(old)
```
