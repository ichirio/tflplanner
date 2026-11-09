# Code templates

The code tflplanner writes where a report says none: the company
standards' sheet `code_templates` (see
[`company_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)).
`table_data` takes a table's rows of the study ARD, `table_process`
normalizes them (and shows where to rework them), `figure_plot` is the
plot a figure starts with, `setup` the code every report of a new study
runs first. In a template, `{OUTPUT_ID}`, `{ARD}` (the study ARD),
`{ARD_PROGRAM}` (the output's ARD program), `{PROGRAM}` and `{STUDY_ID}`
stand for the report's.

## Usage

``` r
code_templates(name = NULL)
```

## Arguments

- name:

  A template's name; `NULL` for all.

## Value

A named character vector.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b186446f206 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b186446f206
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b186446f206/studies
names(code_templates())  # the templates the company standards have
#> [1] "table_data"    "table_process" "figure_plot"   "setup"        
code_templates("table_data")
#>                                                                                                                                                                                                                                                                                                         table_data 
#> "# this report's rows of the study ARD (made by {ARD_PROGRAM})\nard <- readRDS(\"{ARD}\") |>\n  dplyr::filter(output_id == report_id) |>\n  dplyr::select(-c(output_id, analysis_id, population_id))\nif (!nrow(ard)) {\n  stop(\"The study ARD has no rows for \", report_id, \": run {ARD_PROGRAM} first.\")\n}" 
options(old)
```
