# Run a user-code report's code and see what it leaves

Runs the report's data part
([`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md):
the datasets it reads, its ARD when it reads one, its own code) in a
fresh R process from the study folder – the app does not run the code
itself – and gives what it left as `content`, for a preview.

## Usage

``` r
preview_user(study, output_id, timeout = 300)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report (type `user`).

- timeout:

  Seconds to allow.

## Value

A list: `content` (or `NULL`), `error` (the code's error, or `NULL`) and
`log`.
