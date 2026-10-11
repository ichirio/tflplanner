# A study's own ARD functions

The functions defined in the R files the study's ARD programs load (its
ARD definition's study key `source`) and in its
`programs/ard/functions/` (loaded or not), read – not run – with
[`tflspec::tfl_ard_function_info()`](https://ichirio.github.io/tflspec/reference/tfl_ard_function_info.html).

## Usage

``` r
study_ard_functions(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame, one row a function: `name`, `file` (relative to the study
folder), `loaded`, `title`, `description`, `stat_names`, `keywords` (as
[`company_ard_functions()`](https://ichirio.github.io/tflplanner/reference/company_ard_functions.md)).
