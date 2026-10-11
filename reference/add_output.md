# Edit the report list

`add_output()` puts a report on the list; `copy_output()` gives a new
report every row of an existing one (the quickest start for a report
like one already defined); `rename_output()` changes an id everywhere;
`remove_output()` takes a report and all its rows out; `sort_outputs()`
puts the list in its ids' order.

## Usage

``` r
add_output(
  x,
  output_id,
  description = NA_character_,
  data_code = NA_character_,
  type = "table",
  process_code = NA_character_,
  section = NA_character_,
  population = NA_character_,
  at = c("natural", "end")
)

sort_outputs(x)

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

- section:

  The section of the TOC the report is under (its heading, "14.1
  Demographics"); `NA`: from its ID's numbers.

- population:

  The report's analysis set, a population_id (see
  [`set_report_population()`](https://ichirio.github.io/tflplanner/reference/set_report_population.md),
  which also makes its analysis data).

- at:

  Where the new report goes: `"natural"` (where its id sorts among the
  others) or `"end"`.

## Value

The updated `tflplanner`.

## Details

The list's order is the order the reports are made in (the official run
too). A new report goes where its id sorts among the others – by the
numbers in the id, as numbers (`T-14-1-2` after `T-14-1-1` and before
`T-14-1-10`; `T-14-0-1` before `F-14-2-1`, the sections' order), then
its letters (`T-14-1-1` before `T-14-1-1S`) – before the first report
whose id sorts after it, so a list kept in id order stays so; one
ordered by hand keeps that order. `at = "end"` puts it last (a TOC's
reports come in the TOC's order).

## Examples

``` r
p <- add_output(add_output(new_planner(), "T-14-1-10"), "T-14-1-1")
p <- add_output(p, "T-14-1-2")
p$outputs$output_id
#> [1] "T-14-1-1"  "T-14-1-2"  "T-14-1-10"
sort_outputs(add_output(p, "T-14-0-1", at = "end"))$outputs$output_id
#> [1] "T-14-0-1"  "T-14-1-1"  "T-14-1-2"  "T-14-1-10"
```
