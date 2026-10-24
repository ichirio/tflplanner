# The dataset names with more than one data file

A name several files make (`adsl.rds` and `adsl.xpt`, or `adam/adsl.rds`
and `other/adsl.rds`): the catalog's row uses one, and this says which
others there are.

## Usage

``` r
catalog_name_clashes(x, files)
```

## Arguments

- x:

  A `tflplanner`.

- files:

  [`study_files()`](https://ichirio.github.io/tflplanner/reference/study_files.md)
  of the study.

## Value

A data frame: `dataset`, `path` (every file of that name), `used` (the
catalog's row's path, `NA` when it has none).
