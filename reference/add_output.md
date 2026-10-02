# Edit the report list

`add_output()` puts a report on the list; `copy_output()` gives a new
report every row of an existing one (the quickest start for a report
like one already defined); `rename_output()` changes an id everywhere;
`remove_output()` takes a report and all its rows out.

## Usage

``` r
add_output(
  x,
  output_id,
  description = NA_character_,
  data_code = NA_character_,
  type = "table",
  process_code = NA_character_
)

copy_output(x, from, to)

rename_output(x, from, to)

remove_output(x, output_id)
```

## Arguments

- x:

  An `tflplanner`.

- output_id, from, to:

  Report ids.

- description:

  A short description shown in the report list.

- data_code:

  R code that makes the report's ARD, `ard` (for a listing or figure:
  its `content`); `NA` writes a TODO.

- type:

  The report's type, one of
  [`report_types()`](https://ichirio.github.io/tflplanner/reference/table_sheets.md);
  anything but `"table"` is written on the `report` sheet.

- process_code:

  R code that turns `ard` into `data`, what
  [`rtfreporter::table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.html)
  is given:
  [`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)
  and any rework after it (`mutate()` ...). `NA` means
  `data <- normalize_ard(ard)`, unless `data_code` makes `data` itself.

## Value

The updated `tflplanner`.
