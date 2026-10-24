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

- `table` – the data part leaves `ard`, this report's ARD (or `data`,
  when its process code or the company's template normalizes it); the
  program plans the table as its file (`spec/outputs/<id>.yml`) defines
  it, written out as
  `ard |> normalize_ard() |> table_plan() |> plan_*()` from its
  content's items
  ([`tflspec::tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.html);
  or `table_plan(data)`).

- `listing`, `figure` – the data part leaves `content`: `rtftable` pages
  for a listing, the figures for a figure.

The report around it – page, header, footer, titles, footnotes – is
written out from the report's `page:` (its own, its pattern's, the
study's defaults) for every type, as rtfreporter calls
([`tflspec::tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.html)).
The program does not read the spec: generate it again after changing it.
(A planner without a YAML spec – a new study not yet saved – or with
edits not yet saved: from its sheets, the same code.)
