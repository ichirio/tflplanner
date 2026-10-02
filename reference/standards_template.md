# Company standards

tflplanner's own defaults and code lists – dropdowns, presets,
statistics and decimals, ARD methods, listing types, the analysis sets,
data catalog and study-default rows a new study starts with – are a
company's standards. `standards_template()` writes them to a workbook to
edit (the built-in draft, or the standards in use); `read_standards()`
reads one;
[`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)`(standards = )`
installs one in tflplanner's home, and `company_standards()` is what
applies: the installed workbook, else the built-in draft.

## Usage

``` r
standards_template(path, from = c("builtin", "current"))

read_standards(path)

company_standards(home = tflplanner_home())
```

## Arguments

- path:

  A workbook.

- from:

  `"builtin"` for the draft, `"current"` for the standards in use.

- home:

  tflplanner's home.

## Value

`standards_template()`: `path`, invisibly. `read_standards()` and
`company_standards()`: a named list of data frames.

## Details

A sheet the workbook leaves out keeps the built-in one.
