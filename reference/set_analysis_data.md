# Add, change or remove an analysis data of the ARD definition

The analysis data are named data the analyses read (tflspec's sheet
`analysis_data`): made `from` a dataset or an analysis data above, kept
to a population's subjects and the records `where` keeps, with columns
of the population's data added (`add`), columns derived (`derive`) and
one row per set of values (`distinct`) – or, when the columns cannot say
it, made by R of its own (`code`, the other columns but `from` blank).
An analysis names one in `data` (instead of `dataset` /
`population_id`), or as its `denominator`.

## Usage

``` r
set_analysis_data(
  x,
  output_id,
  data_id,
  from,
  population_id = NA,
  where = NA,
  add = NA,
  derive = NA,
  distinct = NA,
  label = NA,
  old = NULL,
  subjects = NA,
  keep = NA,
  code = NA
)

remove_analysis_data(x, output_id, data_id)

import_analysis_data(x, from_output, output_id, data_ids = NULL)

name_analysis_data(x, output_id, dataset, population_id, data_id, label = NA)

copy_analysis_data(x, output_id, data_id)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- data_id:

  The name (lower case; also the object's name in the ARD program).

- from:

  A dataset, or an analysis data above.

- population_id, subjects, where, add, derive, keep, distinct, label:

  The other columns (`subjects`: an analysis data above whose subjects
  it keeps, instead of `population_id`; `add`, `keep`, `distinct`:
  several columns with `" | "` between them).

- old:

  The name of the one to replace; `NULL`: a new one.

- code:

  R that makes the data itself (its value is the data); with it the
  other columns but `from` stay blank.

- from_output:

  The report whose analysis data are copied.

- data_ids:

  Which of them (`NULL`: all), with what they are made from and kept to,
  when those are analysis data as well.

- dataset:

  The dataset its analyses read (`NA`: the analysis set's own).

## Value

The `tflplanner`.

## Details

`set_analysis_data()` adds one, or replaces the one named `old` (a new
name is followed in the analyses and the analysis data made from it).
`remove_analysis_data()` takes one out, refused while it is used.
`name_analysis_data()` gives a report's data – a dataset x analysis set
its analyses read – a name: the analysis data is made, those analyses
read it, and a condition all of them have moves into it.

`import_analysis_data()` copies a report's analysis data into another
report, under the same names (a name the report has already gets `_1`,
`_2` ...); attribute `copied` names the rows added.
