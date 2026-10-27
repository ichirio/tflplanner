# Official runs of a study

`run_batch()` runs the study's official-run program – the ARD
(`programs/ard/autoexec_ard.R`), the reports
(`programs/tfl/autoexec_report.R`) or both (`programs/autoexec_all.R`) –
which makes a batch folder `runs/<date>_<time>_<what>/` with each
program's log, what the run made, the R and package versions
(`session_info.txt`, and the study's `renv.lock` when it has one), with
`code = TRUE` the programs and definition workbooks it ran, and with
`data = TRUE` the data files its programs read. `list_batches()` lists a
study's batch folders.

## Usage

``` r
run_batch(
  study,
  parts = c("ard", "tfl"),
  code = TRUE,
  data = FALSE,
  only = NULL,
  exclude = NULL,
  batch = NULL,
  wait = TRUE
)

list_batches(study)
```

## Arguments

- study:

  An `rtfstudy`.

- parts:

  `"ard"`, `"tfl"` or both.

- code:

  Keep the code in the batch folder.

- data:

  Keep the data files the programs read in the batch folder.

- only:

  Programs or outputs to run; `NULL` for all.

- exclude:

  Reports to leave out (their ARD program and their report program), by
  output id or program name; `NULL` for none. The batch folder's
  `batch.txt` lists the programs left out.

- batch:

  A named batch
  ([`batch_sets()`](https://ichirio.github.io/tflplanner/reference/batch_sets.md)):
  only its reports run, and the batch folder
  (`runs/<date>_<time>_<what>_<batch>/`) and its `batch.txt` name it.
  `NULL`: every report (or `only` / `exclude`).

- wait:

  `FALSE` returns the running
  [processx::process](http://processx.r-lib.org/reference/process.md) at
  once. The run goes on by itself: closing the app, or R, does not stop
  it, and a study runs one official run at a time.

## Value

`run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the batch
folder), `result` (its run.csv), `output` (what the run printed);
otherwise the process. `list_batches()`: a data frame, one row a batch
folder, the newest first, with its `state` – `"done"` (every program
without error), `"failed"` (`errors` of `programs`), or `"incomplete"`
(no record of its end yet: still running, or stopped) – and when it
`finished`.

## Details

The data kept are the files of the data catalog the run's programs read
([`batch_data_plan()`](https://ichirio.github.io/tflplanner/reference/batch_data_plan.md)):
each ARD program its report's datasets, each report program its own (a
listing's, a figure's) or the ARD taken in it reads. They go to the
batch folder's `data/` under the path they have in the study folder
(`data/data/adam/adsl.rds`; a file outside it under `data/external/`),
listed in `data_manifest.csv` with their size, md5 and time. With the
code and the logs, that is everything the run read: a run that can be
made again.

The programs are the saved ones: save the study first.
