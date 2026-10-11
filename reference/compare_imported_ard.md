# Compare a report's own ARD with the one taken in

Double programming: the report's rows of the study ARD (from its ARD
definition) against the ARD taken in for it, by
[`cards::compare_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html).

## Usage

``` r
compare_imported_ard(study, output_id, file = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report.

- file:

  The ARD taken in (a `file` of
  [`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md));
  `NULL`: the one the report row names.

## Value

The comparison
([`cards::compare_ard()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html));
[`cards::is_ard_equal()`](https://pharmaverse.github.io/cards/latest-tag/reference/compare_ard.html)
says whether they agree.
