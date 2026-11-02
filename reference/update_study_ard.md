# Preview one output's ARD

Runs the output's saved ARD program (`programs/ard/<output_id>.R`) on
its own, from the study folder: it replaces the output's rows of the
study's working ARD, so its table can be made, and keeps no log. A
program that fails leaves the ARD alone and records its error
([`ard_status()`](https://ichirio.github.io/tflplanner/reference/ard_status.md)).
The official run is
[`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md).

## Usage

``` r
update_study_ard(study, output_id, timeout = 600)
```

## Arguments

- study:

  An `rtfstudy` (saved: the program runs from disk).

- output_id:

  The output.

- timeout:

  Seconds to allow.

## Value

A list: `ok`, `rows`, `error`, `output` (what the program printed).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1abaea4571b is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1abaea4571b
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1abaea4571b/studies
# \donttest{
# (about 10 seconds: it saves the sample study and runs one ARD program)
s <- save_study(create_sample_study(run = FALSE))
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1abaea4571b/studies/SAMPLE-01
r <- update_study_ard(s, "T-14-1-1")
r$ok
#> [1] TRUE
r$rows  # its rows in the study's working ARD
#> [1] 74
# }
options(old)
```
