# Start an ARD function of one's own from a template

Writes
[`tflspec::tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.html)'s
skeleton (and its test, with `test = TRUE`) into the study's
`programs/ard/functions/` – and adds it to the study key `source`, so
the ARD programs load it – or into the company standards'
`ard_functions/`. Edit it afterwards (in RStudio ...), then try it.

## Usage

``` r
new_ard_function(
  study,
  name,
  type = c("summary", "test", "free"),
  where = c("study", "company"),
  test = TRUE,
  home = tflplanner_home()
)
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function's name (`ard_...`).

- type:

  `"summary"`, `"test"` or `"free"`.

- where:

  `"study"` or `"company"`.

- test:

  Write a testthat file next to it.

- home:

  The tflplanner home.

## Value

The study (its `source` updated when written for the study).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18833cbdb is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18833cbdb
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18833cbdb/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18833cbdb/studies/SAMPLE-01
s <- new_ard_function(s, "ard_my_mean", type = "summary", test = FALSE)
study_ard_functions(s)[, c("name", "file")]  # edit the file, then try it
#>             name                                    file
#> 1 ard_cv_geomean programs/ard/functions/ard_cv_geomean.R
#> 2    ard_my_mean    programs/ard/functions/ard_my_mean.R
# }
options(old)
```
