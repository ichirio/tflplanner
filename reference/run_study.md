# Preview a study's reports

Runs report programs on their own (a preview), from the study folder –
every report or the ones named – each in its own `Rscript` process. They
write the working RTFs (output/tfl/); what each printed is kept in
`logs/preview/` until the next preview, and no log of record is made.
The official run is
[`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md).

## Usage

``` r
run_study(study, output_id = NULL, wait = TRUE)
```

## Arguments

- study:

  An `rtfstudy` (saved: the programs run from disk).

- output_id:

  Reports to run; `NULL` runs all.

- wait:

  `FALSE` returns the running
  [processx::process](http://processx.r-lib.org/reference/process.md) at
  once.

## Value

With `wait = TRUE`,
[`study_status()`](https://ichirio.github.io/tflplanner/reference/study_status.md)
after the run; otherwise the process.
