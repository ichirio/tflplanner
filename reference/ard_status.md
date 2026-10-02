# Where each output's ARD stands

For every output the ARD definition has analyses for: `built` (its rows
are in the study ARD, made from the definition as it is now), `outdated`
(made from an earlier definition), `not built`, or `error` (its last
update failed). Many people may work on one study: the study ARD is
updated output by output
([`update_study_ard()`](https://ichirio.github.io/tflplanner/reference/update_study_ard.md)),
and a table is made from whatever of it is there.

## Usage

``` r
ard_status(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame: `output_id`, `analyses`, `state`, `rows`, `built`,
`error`.
