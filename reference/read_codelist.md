# Read a study's code list into its definition

`read_codelist()` reads a code list – one row a value of a variable:
`variable`, `value`, `label` (what it prints as) and `order` (its place)
– from an `.xlsx` (its first sheet) or a `.csv` file. `set_codelist()`
puts it into the definition's `codelists` sheet as the study's defaults
(every table uses them): a value already there for the same variable is
replaced, the others are kept. A report's own rows replace the defaults
for that report (tflspec's table spec).

## Usage

``` r
read_codelist(path)

set_codelist(x, rows)
```

## Arguments

- path:

  An `.xlsx` or `.csv` file.

- x:

  A `tflplanner`.

- rows:

  What `read_codelist()` returns.

## Value

`read_codelist()`: a data frame; `set_codelist()`: the `tflplanner`.
