# Start a hierarchical table from the data

The other kind of table the app's "first table" form starts: counts of
subjects by a hierarchy of terms – adverse events by system organ class
and preferred term (`AEBODSYS | AEDECOD`), and the same for medications
or medical history by their own – in the columns of a group, the
percents of the analysis set's subjects. It writes the rows every sheet
says them in, as a table made by hand has them:

## Usage

``` r
first_hierarchy_table(
  x,
  output_id,
  path,
  data,
  subjects_path,
  subjects,
  population,
  group,
  hierarchy = c("AEBODSYS", "AEDECOD"),
  where = NA_character_,
  total = NA_character_,
  grade = NA_character_,
  description = NA_character_
)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- path:

  The records' data file (`data/adam/adae.rds`), relative to the study
  folder.

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

- hierarchy:

  The terms, outermost first (`c("AEBODSYS", "AEDECOD")`).

- where:

  The records counted (`TRTEMFL == "Y"`); `NA` all of them.

- total:

  The heading of a Total column (`"Total"`); `NA` none.

- grade:

  A severity or grade column (`AESEV`, `ATOXGR`) to count by too, in the
  columns under each group; `NA` none. Each subject is counted once, at
  the worst: the grade's levels are written in order (a code list), and
  cards counts a subject at the last level it has – of the SOC for the
  SOC row, of the PT for the PT row. Not with a Total column.

- description:

  The report's description.

## Value

The `tflplanner`.

## Details

- the datasets (added to the data catalog when missing) and the analysis
  set (`population` flag `== "Y"` of the subject-level data);

- two analysis data of the report: the analysis set's subjects
  (`adsl_saf`), and the records kept to them and to `where`
  (`adae_saf`);

- one `hierarchical` analysis by the group, over the hierarchy, its
  denominator the analysis set (`over_variables = TRUE`: the "any event"
  row, named in the table's `overall_label`: `"Any TEAE"`);

- the table: the group as columns, the outer terms as groups and the
  innermost as rows, by frequency (most first) within each group;

- the column header (the standards' `"Arm (N=n) n (%), SOC / PT"`), a
  page not splitting a group (22 rows a page, unless the study says),
  and a Total column when `total` is given.
