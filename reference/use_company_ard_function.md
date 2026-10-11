# Use one of the company's ARD functions in a study

Copies the file that defines it into the study's
`programs/ard/functions/` – unless the study has a file of that name,
which wins – and adds it to the ARD definition's study key `source`, so
the study's ARD programs load it. Save the study afterwards.

## Usage

``` r
use_company_ard_function(study, name, home = tflplanner_home())
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function.

- home:

  The tflplanner home.

## Value

The study, its ARD definition's `source` updated.
