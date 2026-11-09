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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18608a8c08 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18608a8c08
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18608a8c08/studies
f <- tempfile(fileext = ".xlsx")
standards_template(f)  # the built-in draft, to edit
file.exists(f)
#> [1] TRUE
options(old)
```
