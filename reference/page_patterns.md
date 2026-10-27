# Page patterns

A study's named page patterns: sets of the report half's defaults
(report, page, header, footer, titles, footnotes, tokens) a report
chooses from. Standard is the blank rows; another pattern – the rows
whose `output_id` is `@` and its name – says only what differs from
Standard. `report_pattern()` is a report's choice (`NA`: Standard),
`set_report_pattern()` sets it: the report's own rows stay over it.

## Usage

``` r
page_patterns(x)

report_pattern(x, output_id)

set_report_pattern(x, output_id, pattern)
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  A report.

- pattern:

  A pattern's name, or `NA` / `"Standard"` for Standard.

## Value

`page_patterns()`: the names, in alphabetical order (Standard not among
them). `report_pattern()`: one name, or `NA`. `set_report_pattern()`:
`x`.
