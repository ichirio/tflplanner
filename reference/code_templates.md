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
