# Try one of a study's (or the company's) ARD functions

In a separate R process, so a function that hangs or fails does not stop
the app: loads the R files the study's ARD programs load (`source`, in
their order; the company's file too when the study does not have the
function), reads the data (a dataset of the ARD definition, of an
analysis set;
[`cards::ADSL`](https://pharmaverse.github.io/cards/latest-tag/reference/adam.html)
when none is given), and runs
[`tflspec::tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.html)
with `args` as an analysis row would give them. The result is kept
(`programs/ard/functions/.checks.json`).

## Usage

``` r
try_ard_function(
  study,
  name,
  dataset = NA_character_,
  population_id = NA_character_,
  args = "",
  home = tflplanner_home(),
  timeout = 60
)
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function.

- dataset, population_id:

  The data: a dataset of the ARD definition and an analysis set (either
  may be `NA`); both `NA`:
  [`cards::ADSL`](https://pharmaverse.github.io/cards/latest-tag/reference/adam.html).

- args:

  Its arguments as R (`"by = TRT01A, variables = AGE"`).

- home:

  The tflplanner home.

- timeout:

  Seconds to allow.

## Value

A list: `problems` (data frame: `level`, `check`, `message`), `ard` (the
first rows of the ARD it gave, or `NULL`), `error` (when it could not be
tried), `seconds`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1877189b42 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1877189b42
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1877189b42/studies
# \donttest{
# (a few seconds: it runs in its own R process)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1877189b42/studies/SAMPLE-01
s <- new_ard_function(s, "ard_my_mean", test = FALSE)
res <- try_ard_function(s, "ard_my_mean", args = "by = TRT01A, variables = AGE")
res$problems  # what the checks found
#> [1] level   check   message
#> <0 rows> (or 0-length row.names)
# }
options(old)
```
