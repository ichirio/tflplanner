# Replace a study's copy of an ARD function with the company's

The study's file of that function is overwritten with the company's
(when the company's is newer, say). The study's results change only when
its ARD is made again.

## Usage

``` r
take_company_ard_function(study, name, home = tflplanner_home())
```

## Arguments

- study:

  An `rtfstudy`.

- name:

  The function.

- home:

  The tflplanner home.

## Value

The study file's path, invisibly.
