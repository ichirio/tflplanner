# Read a code list into a report

`read_codelist()` reads a code list – one row a value of a variable:
`variable`, `value`, `label` (what it prints as) and `order` (its place)
– from an `.xlsx` (its first sheet) or a `.csv` file. `set_codelist()`
puts it into a report's rows of the definition's `codelists` sheet: a
value the report has already for the same variable is replaced, the
others are kept. A code list is a report's (tflspec: every row names its
report); to use one in several reports, copy it
([`import_codelist()`](https://ichirio.github.io/tflplanner/reference/import_codelist.md),
[`standard_codelists()`](https://ichirio.github.io/tflplanner/reference/import_codelist.md)).

## Usage

``` r
read_codelist(path)

set_codelist(x, output_id, rows)
```

## Arguments

- path:

  An `.xlsx` or `.csv` file.

- x:

  A `tflplanner`.

- output_id:

  The report.

- rows:

  A data frame with `variable` and `value` (`label` and `order`
  optional): what `read_codelist()` returns.

## Value

`read_codelist()`: a data frame; `set_codelist()`: the `tflplanner`.
