# Read a code list into a report

`read_codelist()` reads a code list – one row a value of a variable:
`variable`, `value`, `label` (what it prints as), `order` (its place)
and `under` (a nested count's inner value: the value, as the data have
it, of the variable one level out it sits under, for
`complete_hierarchy()`) – from an `.xlsx` (its first sheet) or a `.csv`
file. `set_codelist()` puts it into a report's rows of the definition's
`codelists` sheet: a value the report has already for the same variable
is replaced, the others are kept (rows with no `under` column keep the
`under` the report's values have). A code list is a report's (tflspec:
every row names its report); to use one in several reports, copy it
([`import_codelist()`](https://ichirio.github.io/tflplanner/reference/import_codelist.md),
`standard_codelists()`).

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

  A data frame with `variable` and `value` (`label`, `order` and `under`
  optional): what `read_codelist()` returns.

## Value

`read_codelist()`: a data frame; `set_codelist()`: the `tflplanner`.

## Examples

``` r
f <- tempfile(fileext = ".csv")
writeLines(c("variable,value,label",
             "SEX,F,Female",
             "SEX,M,Male"), f)
cl <- read_codelist(f)
cl
#>   variable value  label order under
#> 1      SEX     F Female  <NA>  <NA>
#> 2      SEX     M   Male  <NA>  <NA>
# put it into a report
x <- add_output(new_planner(), "T-14-1-1", type = "table")
x <- set_codelist(x, "T-14-1-1", cl)
```
