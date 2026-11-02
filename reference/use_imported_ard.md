# Use an ARD taken in for a report, or stop using it

Sets the report row's `ard_source`: `import:<file>` (the file in the
study's `input/ard/`, see
[`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md))
or blank (the report's own ARD definition). The report's program then
reads that file.

## Usage

``` r
use_imported_ard(x, output_id, file = NULL)
```

## Arguments

- x:

  A `tflplanner` (`study$planner`).

- output_id:

  The report.

- file:

  A `file` of
  [`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md);
  `NULL` to go back to the report's own ARD definition.

## Value

The `tflplanner`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba3f37e466 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba3f37e466
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba3f37e466/studies
# \donttest{
# (a few seconds: it writes and runs the ARD program)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba3f37e466/studies/SAMPLE-01
s <- save_study(s)  # the report's ARD program is written
r <- run_ard(s, "T-14-1-2")  # the report's ARD, made in its own R process
f <- tempfile(fileext = ".rds")
saveRDS(r$ard, f)  # (here the same ARD stands for one made elsewhere)
imp <- import_ard(s, f, output_id = "T-14-1-2", source = "CRO")
s$planner <- use_imported_ard(s$planner, "T-14-1-2", imp$file)
s$planner$sheets$report$ard_source[s$planner$sheets$report$output_id == "T-14-1-2"]
#> [1] "import:file1aba5f6e4bac.rds" NA                           
s$planner <- use_imported_ard(s$planner, "T-14-1-2")  # back to its own ARD
# }
options(old)
```
