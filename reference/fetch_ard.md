# Run a report's data part and keep what its ARD holds

Runs the study's setup code, the report's ARD code and its normalization
and rework
([`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md))
in a fresh R process, from the study folder, and reads
[`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md)
from the result: the normalized `data` when the rework ran, the raw
`ard` otherwise. A report whose ARD its ARD definition makes reads its
rows of the study ARD when they are made from the definition as it is
now
([`ard_status()`](https://ichirio.github.io/tflplanner/reference/ard_status.md):
built); else its ARD is made here, in a folder of its own, and the study
ARD (`output/ard/ard.rds`) is not written – making the study ARD is
[`update_study_ard()`](https://ichirio.github.io/tflplanner/reference/update_study_ard.md)'s,
and the official run's. The data frames the code loaded (`adsl`, ...)
add what the ARD lacks: each variable's label, and the order of its
values (a factor's levels, or the order of a numeric companion
`<name>N`: `TRT01A` by `TRT01AN`). The result is kept in tflplanner's
home, so it is there the next time the study is opened.

## Usage

``` r
fetch_ard(study, output_id, timeout = 300, home = tflplanner_home())

ard_info(study, output_id, home = tflplanner_home())

ard_data(study, output_id, home = tflplanner_home())

ard_raw(study, output_id, rows = FALSE, home = tflplanner_home())

ard_preview_state(
  study,
  output_id,
  meta = ard_info(study, output_id, home),
  home = tflplanner_home()
)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The report.

- timeout:

  Seconds to allow.

- home:

  tflplanner's home. `ard_info()` returns the metadata kept,
  `ard_data()` the normalized data kept with it (what the builder's
  preview is planned from), `ard_raw()` the ARD before its normalization
  (`rows = TRUE`: the report's rows as the study ARD has them, with
  their ids), and `ard_preview_state()` whether what is kept is still
  the report's ARD: `"missing"`, `"outdated"` (its definition, its code,
  the study's setup or the data have changed since) or `"current"`.

- rows:

  `TRUE`: the report's rows with their ids, as the study ARD has them
  (the ARD definition's reports; `NULL` for the others).

- meta:

  What was kept (`ard_info()`); by default, read.

## Value

The metadata
([`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md))
with `fetched` (the time), `source` (`"data"` or `"ard"`), `error` (the
rework's error, if it failed), `made` (`"here"`: made for the report;
`"study"`: read from the study ARD), `print` (what it was made from,
`ard_preview_state()`) and `log`, invisibly.
