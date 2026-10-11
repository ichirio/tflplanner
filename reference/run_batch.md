# Official runs of a study

`run_batch()` runs the study's official-run program – the ARD
(`programs/ard/autoexec_ard.R`), the reports
(`programs/tfl/autoexec_report.R`) or both (`programs/autoexec_all.R`) –
which makes a batch folder `runs/<date>_<time>_<what>/` with each
program's log, what the run made and, with `code = TRUE`, the programs
and definition workbooks it ran. `list_batches()` lists a study's batch
folders.

## Usage

``` r
run_batch(
  study,
  parts = c("ard", "tfl"),
  code = TRUE,
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
  once.

## Value

`run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the batch
folder), `result` (its run.csv), `output` (what the run printed);
otherwise the process. `list_batches()`: a data frame.

## Details

The programs are the saved ones: save the study first.
