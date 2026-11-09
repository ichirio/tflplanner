# Export the study's analyses as CDISC ARS

Writes the study's ARD definition – with its table and report
definitions: levels, titles, footnotes, files – as a CDISC Analysis
Results Standard reporting event
([`tflspec::tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.html)):
the ARS JSON (the form to exchange), CDISC's Excel template of it (to
read), and `ars_check.csv`, what
[`tflspec::tfl_check_ars()`](https://ichirio.github.io/tflspec/reference/tfl_check_ars.html)
finds and what the ARS does not say
([`tflspec::tfl_ars_unmapped()`](https://ichirio.github.io/tflspec/reference/tfl_ars_unmapped.html)).
An analysis needs its `purpose` (a column of the ARD definition's
analyses) for the ARS to be complete; a blank one is listed.

## Usage

``` r
export_ars(study, dir, profile = c("cdisc", "siera"))
```

## Arguments

- study:

  An `rtfstudy`.

- dir:

  Destination folder.

- profile:

  `"cdisc"`, or `"siera"` for a reporting event siera can run (see
  [`tflspec::tfl_ars()`](https://ichirio.github.io/tflspec/reference/tfl_ars.html)).

## Value

The paths written, invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b186c08038d is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b186c08038d
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b186c08038d/studies
# \donttest{
# (a few seconds: it writes the ARS files)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b186c08038d/studies/SAMPLE-01
# the ARS files go to a folder in the temporary folder
files <- export_ars(s, file.path(tempdir(), "ars"))
basename(files)
#> [1] "SAMPLE-01_ars.json" "SAMPLE-01_ars.xlsx" "ars_check.csv"     
# }
options(old)
```
