# The rows the builder offers for a continuous variable

Each is a row label and its template: one statistic a row (`Mean`,
`{mean}`) or several in one (`Mean (SD)`, `{mean} ({sd})`). Their
decimals are the statistics' (the `digits` sheet, see
[`tflspec::tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.html)).

## Usage

``` r
builder_stats()
```

## Value

A data frame: `key`, `row`, `template`.
