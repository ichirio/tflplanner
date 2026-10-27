# Compare two official runs

What changed between two batch folders of a study
([`list_batches()`](https://ichirio.github.io/tflplanner/reference/run_batch.md)):
their inputs – the data, by the md5 `data_manifest.csv` records, and the
code, by the md5 of the files of `code/`, where both runs kept them –
and their results (`output/`). After a fix to the data or the programs,
it says whether only what was fixed changed.

## Usage

``` r
compare_batches(study, batch, against = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- batch:

  A batch folder's name (`list_batches()$batch`): the later run.

- against:

  The earlier one; `NULL`: the run of the same kind (ARD, reports, both)
  before `batch`.

## Value

A data frame, `kind` (`"data"`, `"code"`, `"output"`), `file`, `result`;
with `attr(, "batches")` (`against`, `batch`) and `attr(, "changed")` (a
list: the `inputs` and the `outputs` that changed).

## Details

A result is `"identical"` (the same bytes), `"same but the time"` (a
report or a status file the same but the date and time it prints or
records), `"same object"` (an `.rds` read the same), `"different"`,
`"not made"` (the earlier run made it, this one did not) or `"new"`. An
input is `"added"`, `"removed"` or `"changed"` (one the same is not
listed); `"not kept"` when either run did not keep that input.

## Examples

``` r
# \donttest{
home <- tempfile()
old <- options(tflplanner.home = home)
setup_tflplanner(studies_root = file.path(home, "studies"))
#> tflplanner home: /tmp/RtmpJoPVce/file1add578f1fc8
#> new studies go to: /tmp/RtmpJoPVce/file1add578f1fc8/studies
s <- create_study("CMP", planner = add_output(new_planner(), "T-1"))
d <- file.path(s$path, "runs")
# two batch folders as two official runs leave them (the second made a
# different RTF)
for (b in c("20260101_100000_report", "20260102_100000_report")) {
  dir.create(file.path(d, b, "output", "tfl"), recursive = TRUE)
  writeLines(b, file.path(d, b, "output", "tfl", "T-1.rtf"))
  writeLines(c("part,program,status", "tfl,programs/tfl/T-1.R,OK"),
             file.path(d, b, "run.csv"))
  writeLines("Finished : 2026-01-01 10:00:00", file.path(d, b, "batch.txt"))
}
compare_batches(s, "20260102_100000_report")
#>     kind               file    result
#> 1   data                     not kept
#> 2   code                     not kept
#> 3 output output/tfl/T-1.rtf different
options(old)
# }
```
