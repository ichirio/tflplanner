# Put a study's data files into its data catalog

Every data file of the data folders (`data/adam`, `data/sdtm`,
`data/other`, subfolders too;
[`study_files()`](https://ichirio.github.io/tflplanner/reference/study_files.md))
that the ARD definition's `datasets` sheet does not have yet – by path,
or by name – gets a row: `dataset` the file name in capitals, `level`
the folder's (`ADaM`, `SDTM`; blank for `other`), `path` relative to the
study folder. Rows already there are left as they are. When one name has
files in several formats (`adsl.rds`, `adsl.xpt`), the row is made for
the format read first (`rds`, `rda`, `parquet`, `csv`, `xpt`,
`sas7bdat`); the others stay files with no row of their own, and
[`catalog_name_clashes()`](https://ichirio.github.io/tflplanner/reference/catalog_name_clashes.md)
says which names have more than one file.

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
