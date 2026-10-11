# Start an ARD function of one's own from a template

Writes
[`tflspec::tfl_ard_function_template()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_template.html)'s
skeleton (and its test, with `test = TRUE`) into the study's
`programs/ard/functions/` – and adds it to the study key `source`, so
the ARD programs load it – or into the company standards'
`ard_functions/`. Edit it afterwards (in RStudio ...), then try it.

## Usage

``` r
new_ard_function(
  study,
  name,
  type = c("summary", "test", "free"),
  where = c("study", "company"),
  test = TRUE,
  home = tflplanner_home()
)
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function's name (`ard_...`).

- type:

  `"summary"`, `"test"` or `"free"`.

- where:

  `"study"` or `"company"`.

- test:

  Write a testthat file next to it.

- home:

  The tflplanner home.

## Value

The study (its `source` updated when written for the study).
