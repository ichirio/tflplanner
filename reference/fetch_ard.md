# Run a report's data part and keep what its ARD holds

Runs the study's setup code, the report's ARD code and its normalization
and rework
([`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md))
in a fresh R process, from the study folder, and reads
[`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md)
from the result: the normalized `data` when the rework ran, the raw
`ard` otherwise. The data frames the code loaded (`adsl`, ...) add what
the ARD lacks: each variable's label, and the order of its values (a
factor's levels, or the order of a numeric companion `<name>N`: `TRT01A`
by `TRT01AN`). The result is kept in tflplanner's home, so it is there
the next time the study is opened.

## Usage

``` r
fetch_ard(study, output_id, timeout = 300, home = tflplanner_home())

ard_info(study, output_id, home = tflplanner_home())

ard_data(study, output_id, home = tflplanner_home())
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
  preview is planned from).

## Value

The metadata
([`ard_meta()`](https://ichirio.github.io/tflplanner/reference/ard_meta.md))
with `fetched` (the time), `source` (`"data"` or `"ard"`), `error` (the
rework's error, if it failed) and `log`, invisibly.
