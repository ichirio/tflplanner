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
