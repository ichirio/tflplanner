# The data an official run would keep

The data files the programs of a run read (by the data catalog, see
[`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)'s
`data`), with their size, as the study folder has them now: what
`run_batch(data = TRUE)` would copy into the batch folder.

## Usage

``` r
batch_data_plan(study, parts = c("ard", "tfl"), output_ids = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- parts:

  `"ard"`, `"tfl"` or both.

- output_ids:

  The reports of the run; `NULL` for every report.

## Value

A data frame: `path` (as the catalog gives it), `size` (bytes; `NA` when
the file is not there), `programs` (the run's programs that read it, one
string).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b181263c568 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b181263c568
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b181263c568/studies
# \donttest{
# (a few seconds: it saves the sample study)
s <- save_study(create_sample_study(run = FALSE))
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b181263c568/studies/SAMPLE-01
batch_data_plan(s)[c("path", "size")]  # the data files a run would keep
#>                  path  size
#> 1  data/adam/adsl.rds  8790
#> 2  data/adam/advs.rds 10690
#> 3 data/adam/adtte.rds  9719
#> 4  data/adam/adae.rds 17463
# }
options(old)
```
