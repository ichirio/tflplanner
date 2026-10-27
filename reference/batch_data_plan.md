# The data an official run would keep

The data files the programs of a run read (by the data catalog, see
[`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)'s
`data`), with their size, as the study folder has them now: what
`run_batch(data = TRUE)` would copy into the batch folder.

## Usage

``` r
batch_data_plan(study, parts = c("ard", "tfl"), output_ids = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- parts:

  `"ard"`, `"tfl"` or both.

- output_ids:

  The reports of the run; `NULL` for every report.

## Value

A data frame: `path` (as the catalog gives it), `size` (bytes; `NA` when
the file is not there), `programs` (the run's programs that read it, one
string).
