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
