# Stop using an ARD taken in

Marks it `"removed"` in the record
([`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md)):
the file stays in `input/ard/` and on the record (what a report was made
from is not forgotten), and a report still pointing at it is named in an
error when its program runs.

## Usage

``` r
remove_imported_ard(study, import_id)
```

## Arguments

- study:

  An `rtfstudy`.

- import_id:

  The import.

## Value

The record, invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b187ced51c0 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b187ced51c0
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b187ced51c0/studies
# \donttest{
# (a few seconds: it writes and runs the ARD program)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b187ced51c0/studies/SAMPLE-01
s <- save_study(s)  # the report's ARD program is written
r <- run_ard(s, "T-14-1-2")  # the report's ARD, made in its own R process
f <- tempfile(fileext = ".rds")
saveRDS(r$ard, f)  # (here the same ARD stands for one made elsewhere)
imp <- import_ard(s, f, output_id = "T-14-1-2", source = "CRO")
remove_imported_ard(s, imp$import_id)  # the file stays; the record says so
ard_imports(s)[, c("import_id", "state")]
#>   import_id   state
#> 1    IMP001 removed
# }
options(old)
```
