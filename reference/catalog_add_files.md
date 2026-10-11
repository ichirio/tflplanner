# Put a study's data files into its data catalog

Every data file of the ADaM and SDTM folders
([`study_files()`](https://ichirio.github.io/tflplanner/reference/study_files.md))
that the ARD definition's `datasets` sheet does not have yet – by path,
or by name – gets a row: `dataset` the file name in capitals, `level`
the folder's (`ADaM`, `SDTM`), `path` relative to the study folder. Rows
already there are left as they are.

## Usage

``` r
catalog_add_files(x, files)
```

## Arguments

- x:

  A `tflplanner`.

- files:

  [`study_files()`](https://ichirio.github.io/tflplanner/reference/study_files.md)
  of the study.

## Value

The `tflplanner`, with attribute `added` (the names added).
