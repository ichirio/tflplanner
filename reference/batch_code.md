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
batch_code(x, date = Sys.Date())

autoexec_all_code(date = Sys.Date())
```

## Arguments

- x:

  A `tflplanner`.

- date:

  The date stamped in the banner.

## Value

The program, one element per line.
