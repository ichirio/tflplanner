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
