# Files in a study folder

Files in a study folder

## Usage

``` r
study_files(study, folder = "data")
```

## Arguments

- study:

  An `rtfstudy`.

- folder:

  A folder of
  [`study_layout()`](https://ichirio.github.io/tflplanner/reference/study_layout.md)
  (`"adam"`, `"tfl"`, ...), or `"data"` for all three data folders.

## Value

A data frame: `folder`, `file`, `size_kb`, `modified`, `path`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba4eddbcae is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba4eddbcae
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba4eddbcae/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba4eddbcae/studies/SAMPLE-01
head(study_files(s)[, c("folder", "file", "size_kb")])
#>      folder      file size_kb
#> 1 data/adam  adae.rds    17.1
#> 2 data/adam  adsl.rds     8.6
#> 3 data/adam adtte.rds     9.5
#> 4 data/adam  advs.rds    10.4
# }
options(old)
```
