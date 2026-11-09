# Run a report's analyses, or the whole study's

Runs
[`tflspec::tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.html)
in its own R process from the study folder and reads back the ARD it
makes.

## Usage

``` r
run_ard(study, output_id = NULL, timeout = 600)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  A report, or `NULL` for the study (saved where the definition's
  `output` says).

- timeout:

  Seconds to allow.

## Value

A list: `ard` (or `NULL`), `error`, `log`, `seconds`, `code`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18309a793b is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18309a793b
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18309a793b/studies
# \donttest{
# (a few seconds: it runs in its own R process)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18309a793b/studies/SAMPLE-01
r <- run_ard(s, "T-14-1-2")
r$seconds
#> [1] 0.8
head(ard_view(r$ard), 3)
#>   output_id analysis_id group1 group1_level variable variable_level  context
#> 1  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#> 2  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#> 3  T-14-1-2      GROUPN                       TRT01A         Drug X tabulate
#>   stat_name stat_label     stat stat_fmt
#> 1         n          n       80       80
#> 2         N          N      158      158
#> 3         p          % 0.506329     50.6
# }
options(old)
```
