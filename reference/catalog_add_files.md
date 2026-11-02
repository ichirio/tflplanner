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
`catalog_name_clashes()` says which names have more than one file.

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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba4ed3dac6 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba4ed3dac6
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba4ed3dac6/studies
# \donttest{
# (needs the sample data; a few seconds)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba4ed3dac6/studies/SAMPLE-01
# a new data file in the study's data folder ...
saveRDS(data.frame(USUBJID = 1, PARAMCD = "ALT"), file.path(s$path, "data/adam/adlb.rds"))
# ... gets its row in the data catalog
p <- catalog_add_files(s$planner, study_files(s))
attr(p, "added")
#> [1] "ADLB"
# }
options(old)
```
