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

standard_codelists(sets = NULL, home = tflplanner_home())
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

- sets:

  Only these sets of the standards; `NULL` for all.

- home:

  tflplanner's home.

## Value

`import_codelist()`: the `tflplanner`; `standard_codelists()`: a data
frame (`set`, `variable`, `value`, `label`, `order`, `note`).
