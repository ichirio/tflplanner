# The R program for one report

A program runs with the study folder as its working directory (see
[`study_layout()`](https://ichirio.github.io/tflplanner/reference/study_layout.md))
and names every file relative to it. What it does depends on the
report's `type`:

## Usage

``` r
program_code(x, output_id, date = Sys.Date())
```

## Arguments

- x:

  An `tflplanner`.

- output_id:

  The report.

- date:

  The date stamped in the banner.

## Value

The program, one element per line.

## Details

- `table` – the data part leaves `data`, the normalized ARD; the program
  saves it to `output/ard/<output_id>.rds` (the deliverable data) and
  plans the table as `table_spec.xlsx` defines it, written out as
  `table_plan() |> plan_*()`
  ([`tflspec::tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.html)).

- `listing`, `figure` – the data part leaves `content`: `rtftable` pages
  for a listing, the figures for a figure.

The report around it – page, header, footer, titles, footnotes – is
written out from `report_spec.xlsx` for every type, as rtfreporter calls
([`tflspec::tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.html)).
The program does not read the workbooks: generate it again after
changing them.
