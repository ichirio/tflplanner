# The official-run programs of a study

`batch_code()` is `programs/batch.R`: the study's programs (the ARD
programs, the report programs, what each report makes) and the runner
that runs them into a batch folder. `autoexec_all_code()` is
`programs/autoexec_all.R` (the ARD, then the reports); see also
[`ard_autoexec_code()`](https://ichirio.github.io/tflplanner/reference/ard_setup_code.md)
and
[`autoexec_code()`](https://ichirio.github.io/tflplanner/reference/autoexec_code.md).

## Usage

``` r
batch_code(x, date = Sys.Date(), root = ".")

autoexec_all_code(date = Sys.Date())
```

## Arguments

- x:

  A `tflplanner`.

- date:

  The date stamped in the banner.

- root:

  The study folder (the definitions' fingerprints read the files the ARD
  programs name from it).

## Value

The program, one element per line.

## Examples

``` r
x <- add_output(new_planner(), "T-14-1-1", type = "table")
head(batch_code(x), 5)
#> [1] "# ============================================================================"
#> [2] "#  Program    : programs/batch.R"                                              
#> [3] "#  The runner of the study's official runs: programs/ard/autoexec_ard.R,"      
#> [4] "#  programs/tfl/autoexec_report.R and programs/autoexec_all.R source it."      
#> [5] "#  Each run makes a batch folder runs/<date>_<time>_<what>/ with the"          
```
