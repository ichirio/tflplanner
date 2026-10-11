# Try one of the company's ARD functions

Loads its file and runs
[`tflspec::tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.html)
on `data`, with the arguments an analysis row would give.

## Usage

``` r
check_company_ard_function(
  name,
  data,
  ...,
  stat_names = NULL,
  home = tflplanner_home()
)
```

## Arguments

- name:

  The function.

- data:

  The data to try it on.

- ...:

  Its other arguments (`by = TRT01A, variables = AGE`).

- stat_names:

  The statistics it should give.

- home:

  The tflplanner home.

## Value

The problems found
([`tflspec::tfl_check_ard_function()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard_function.html)).
