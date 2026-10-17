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
