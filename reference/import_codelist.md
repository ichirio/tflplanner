# A report's code lists: copy one in

A code list is a report's: every row of the `codelists` sheet names its
report. `import_codelist()` copies another report's code lists into one
(all, or those of some variables); `standard_codelists()` gives the
company standards' (their `codelists` sheet: one row a value, `set`
naming each list), for
[`set_codelist()`](https://ichirio.github.io/tflplanner/reference/read_codelist.md).
The rows copied are the report's own: nothing ties them to where they
came from.

## Usage

``` r
import_codelist(x, from_output, output_id, variables = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- from_output:

  The report copied from.

- output_id:

  The report copied into.

- variables:

  Only these variables' code lists; `NULL` for all.

## Value

The `tflplanner`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba7771d8e0 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba7771d8e0
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba7771d8e0/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba7771d8e0/studies/SAMPLE-01
# the code list of SEX in one report is copied into another
p <- import_codelist(s$planner, "T-14-1-1", "T-14-1-3", variables = "SEX")
sum(p$sheets$codelists$output_id %in% "T-14-1-3")
#> [1] 6
# }
options(old)
```
