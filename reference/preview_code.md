# Run a report's own code and see what it leaves

Runs the report's program up to its report
([`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md):
the datasets it registers and its ARD read, its data step and its
content step, each its own code or as its definition makes it) in a
fresh R process from the study folder – the app does not run the code
itself – and gives what it left as `content`, for a preview.

## Usage

``` r
preview_code(study, output_id, timeout = 300)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report (a step of it its own code,
  [`step_code()`](https://ichirio.github.io/tflplanner/reference/step_code.md)).

- timeout:

  Seconds to allow.

## Value

A list: `content` (or `NULL`), `error` (the code's error, or `NULL`),
`code_step` and `code_line` (the step and the line of its code the error
is on, when it is in the report's own code) and `log`.
