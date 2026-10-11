# What went wrong inside the analyses while a study's ARD was made

The errors and warnings the study ARD (`output/ard/ard.rds`, as last
made) keeps, and those of the ARDs reports take in (`input/ard/`), one
row each, errors first
([`tflspec::tfl_ard_conditions()`](https://ichirio.github.io/tflspec/reference/tfl_ard_conditions.html)).
An analysis with an error has no statistics in the ARD: its cells are
blank in the table.

## Usage

``` r
study_ard_conditions(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame (`output_id`, `analysis_id`, `variable`, `groups`, `level`,
`message`, `statistics`, `source`: `""` for the study ARD, the file's
name for an ARD taken in); no rows when nothing went wrong or there is
no ARD yet.
