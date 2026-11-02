# ARDs taken into a study

The record of every ARD taken in
([`import_ard()`](https://ichirio.github.io/tflplanner/reference/import_ard.md)):
`import_id`, the `file` in the study's `input/ard/` folder, the
`original` file, its `source` (who made it), `format`, when it was
`imported` and by which `user`, its `md5`, its `rows`, the `outputs` it
has rows for (its `output_id` column), the result of the `check`, and
its `state` (`"in use"`, or `"removed"` – a removed one stays on the
record).

## Usage

``` r
ard_imports(study)
```

## Arguments

- study:

  An `rtfstudy`
  ([`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)).

## Value

A data frame, one row per ARD taken in.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba5c7d1ed2 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba5c7d1ed2
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba5c7d1ed2/studies
# \donttest{
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba5c7d1ed2/studies/SAMPLE-01
ard_imports(s)  # none taken in yet
#>  [1] import_id file      original  source    format    imported  user     
#>  [8] md5       rows      outputs   check     state    
#> <0 rows> (or 0-length row.names)
if (requireNamespace("cards", quietly = TRUE)) {
  f <- tempfile(fileext = ".rds")
  saveRDS(cards::ard_continuous(mtcars, variables = "mpg"), f)
  import_ard(s, f, source = "example")
  ard_imports(s)[, c("import_id", "source", "rows", "state")]
}
#>   import_id  source rows  state
#> 1    IMP001 example    8 in use
# }
options(old)
```
