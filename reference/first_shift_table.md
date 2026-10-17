# Start a shift table from the data

The third kind of table the app's "first table" form starts: subjects by
their baseline category (columns, under each group) and their worst
category after baseline (rows) – a laboratory normal range (`BNRIND` /
`ANRIND`) or toxicity grade (`BTOXGR` / `ATOXGR`) – one page a
parameter, the percents of each column's subjects. It writes what such a
table made by hand has (tflspec's LB example):

## Usage

``` r
first_shift_table(
  x,
  output_id,
  path,
  data,
  subjects_path,
  subjects,
  population,
  group,
  params,
  baseline = "BNRIND",
  post = "ANRIND",
  worst = NULL,
  where = NA_character_,
  total = NA_character_,
  description = NA_character_
)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- path:

  The records' data file (`data/adam/adlb.rds`).

- data:

  Its data.

- subjects_path:

  The subject-level data file (`data/adam/adsl.rds`).

- subjects:

  Its data.

- population:

  The analysis set flag column of the subject-level data (`SAFFL`).

- group:

  The column of the groups (`TRT01A`).

- params:

  The parameters (`PARAMCD` values), one page each.

- baseline, post:

  The baseline and the post-baseline category columns (`BNRIND`,
  `ANRIND`).

- worst:

  The post-baseline categories from best to worst; `NULL`: normal ranges
  `NORMAL, LOW, HIGH`, else the categories' order.

- where:

  The records counted (`ONTRTFL == "Y"`); `NA` all of them.

- total:

  The heading of a column of each group's subjects whatever their
  baseline (`"Total"`); `NA` none.

- description:

  The report's description.

## Value

The `tflplanner`.

## Details

- the analysis set and its subjects (`adsl_saf`), and the parameters'
  records counted, kept to them (`adlb_saf`);

- each subject's worst record of a parameter (`adlb_saf_worst`, R of its
  own): the last of `worst`, the post-baseline categories from best to
  worst;

- one categorical analysis by parameter, group and baseline; with
  `total`, over the records and the same again with every baseline as
  `total` (a column of all the group's subjects);

- the categories' order (code lists), the table (one page a parameter),
  and the column header (each group over its baselines; no N, which
  differs from one parameter's page to the next).
