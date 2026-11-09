# What an ARD holds

Reads the keys (the column and hierarchy variables) with their levels,
the analysis variables with their kind, levels, statistics and labels,
and the contexts and statistic names, from a normalized ARD (`data`,
what
[`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
is given) or, failing that, from the raw cards ARD.

## Usage

``` r
ard_meta(ard = NULL, data = NULL)
```

## Arguments

- ard:

  A cards ARD, or `NULL`.

- data:

  The normalized (and reworked) ARD, or `NULL`.

## Value

A list: `keys` (named list of levels), `by` (the column keys),
`hierarchy` (the hierarchy keys, outermost first), `variables` (a data
frame: `variable`, `kind`, `levels`, `n_levels`, `stats`, `label`),
`contexts`, `stats`, `columns`; from a normalized ARD also `overall`
(rows over all subjects, which a Total column reads), `factor_vars` (the
variables whose levels keep a factor's order), `factor_keys` (the key
columns that are factors: their levels' order) and `comparisons` (the
comparison records – tests, differences – by context: their statistics).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18796dd709 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18796dd709
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18796dd709/studies
# \donttest{
if (requireNamespace("cards", quietly = TRUE)) {
  # a cards ARD: age groups by treatment arm
  ard <- cards::ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1")
  m <- ard_meta(ard)
  m$by
  m$variables[c("variable", "kind", "levels")]
}
#>   variable        kind            levels
#> 1   AGEGR1 categorical 65-80 | <65 | >80
# }
options(old)
```
