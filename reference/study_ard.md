# One report's ARD, wherever it comes from

The rows a report's table reads: from the ARD taken in when the report
row says `ard_source = import:<file>`, else from the study ARD the ARD
definition makes (`output/ard/ard.rds`). The id columns are dropped, as
[`tflspec::tfl_ard_for()`](https://ichirio.github.io/tflspec/reference/tfl_ard_for.html)
does.

## Usage

``` r
study_ard(study, output_id)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report.

## Value

The ARD, or `NULL` when there is none yet.
