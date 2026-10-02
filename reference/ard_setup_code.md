# The ARD programs of a study

`ard_setup_code()` is `programs/ard/ard_setup.R`, which every ARD
program sources: cards, the statistics tflplanner computes
([`tflspec::tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.html),
the company standards' catalog), the stat_fmt formats, and
`.save_output()`, which replaces one output's rows of the study ARD and
records the build. `ard_program_code()` is one output's program,
`programs/ard/<output_id>.R`. `ard_autoexec_code()` is
`programs/ard/autoexec_ard.R`, which runs them from the study folder –
all, or the ones named (`Rscript programs/ard/autoexec_ard.R T-14-1-1`)
– each in its own R process with its log in `logs/ard/`.

## Usage

``` r
ard_setup_code(spec, date = Sys.Date())

ard_program_code(spec, output_id, date = Sys.Date(), dir = ".")

ard_autoexec_code(spec, date = Sys.Date())
```

## Arguments

- spec:

  An
  [`tflspec::tfl_ard_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard_spec.html)
  (or the path of one).

- date:

  The date stamped in the banner.

- output_id:

  The output.

- dir:

  The study folder: the fingerprint recorded with the ARD reads the
  study's own analysis functions (its key `source`) from it.

## Value

The code, one element per line.
