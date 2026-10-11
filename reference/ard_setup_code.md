# The ARD programs of a study

`ard_setup_code()` is `programs/ard/ard_setup.R`, which every ARD
program sources: the study's setup (`programs/study_setup.R`, see
[`study_setup_code()`](https://ichirio.github.io/tflplanner/reference/study_setup_code.md)),
cards, the statistics tflplanner computes
([`tflspec::tfl_ard_statistics()`](https://ichirio.github.io/tflspec/reference/tfl_ard_statistics.html),
the company standards' catalog), the stat_fmt formats, and the option
that makes `save_ard()` (the study's `programs/study_helpers.R`, see
[`study_helpers_code()`](https://ichirio.github.io/tflplanner/reference/study_helpers_code.md))
record the study setup each ARD was built with. `ard_program_code()` is
one output's program, `programs/ard/<output_id>.R`.
`ard_autoexec_code()` is `programs/ard/autoexec_ard.R`, which runs them
from the study folder – all, or the ones named
(`Rscript programs/ard/autoexec_ard.R T-14-1-1`) – each in its own R
process with its log in `logs/ard/`.

## Usage

``` r
ard_setup_code(spec, date = Sys.Date())

ard_program_code(
  spec,
  output_id,
  date = Sys.Date(),
  dir = ".",
  codelists = NULL
)

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

- codelists:

  The reports' code lists (the table definition's `codelists` sheet,
  every row a report's), or `NULL`: each column the report's analyses
  read that its code lists list becomes a factor in their order before
  the analyses, so the ARD keeps the order and counts a value no record
  has (0)
  ([`tflspec::tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.html)).
  They are part of the fingerprint.

## Value

The code, one element per line.
