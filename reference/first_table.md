# Start a summary table from the data

Writes what a summary table (columns = a group, rows = variables) needs,
from the answers the app's "first table" form asks: the dataset in the
data catalog (added when missing), the analysis set (`population` flag
`== "Y"`, added when missing), one analysis per variable (numbers
summarized, the rest counted), the report (a Table, added when missing)
and its `tables` row (the group as columns, one group per variable) and
`variables` rows (their order, and the data's labels).

## Usage

``` r
first_table(
  x,
  output_id,
  path,
  data,
  population,
  group,
  variables,
  description = NA_character_
)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- path:

  The data file, relative to the study folder.

- data:

  Its data (the kinds and labels of its columns).

- population:

  The analysis set flag column (`SAFFL`).

- group:

  The column of the groups (`TRT01A`).

- variables:

  The variables of the rows, in order.

- description:

  The report's description.

## Value

The `tflplanner`.
