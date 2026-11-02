# Take an ARD made elsewhere into a study

Copies `path` into the study's `input/ard/` folder (read-only there),
reads it
([`tflspec::tfl_read_ard()`](https://ichirio.github.io/tflspec/reference/tfl_read_ard.html):
rds, the JSON / YAML of
[`tflspec::tfl_write_ard()`](https://ichirio.github.io/tflspec/reference/tfl_write_ard.html),
XPT, CSV), checks it
([`tflspec::tfl_check_ard()`](https://ichirio.github.io/tflspec/reference/tfl_check_ard.html))
– against the table definition of each report it is for, when
`output_id` is given – and records it
([`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md)).
It is then used by a report whose report row says
`ard_source = import:<file>` (see
[`use_imported_ard()`](https://ichirio.github.io/tflplanner/reference/use_imported_ard.md)).

## Usage

``` r
import_ard(study, path, output_id = NULL, source = NA_character_, name = NULL)
```

## Arguments

- study:

  An `rtfstudy`.

- path:

  The ARD file.

- output_id:

  The report(s) it is for; `NULL` takes the ARD's own `output_id`
  column, if it has one.

- source:

  Who made it (a CRO, a program), for the record.

- name:

  The file name in `input/ard/`; `NULL`: the original's, made unique.

## Value

The new row of
[`ard_imports()`](https://ichirio.github.io/tflplanner/reference/ard_imports.md),
invisibly, with the check's problems as the attribute `check`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba158aa2dd is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba158aa2dd
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba158aa2dd/studies
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmphQvIXi/tflplanner-home1aba158aa2dd/studies/SAMPLE-01
ard_imports(s)  # none taken in yet
#>  [1] import_id file      original  source    format    imported  user     
#>  [8] md5       rows      outputs   check     state    
#> <0 rows> (or 0-length row.names)
# an ARD made elsewhere (here, with cards)
if (requireNamespace("cards", quietly = TRUE)) {
  f <- tempfile(fileext = ".rds")
  saveRDS(cards::ard_continuous(mtcars, variables = "mpg"), f)
  import_ard(s, f, source = "example")
  ard_imports(s)
}
#>   import_id                 file                             original  source
#> 1    IMP001 file1aba36d9bf9b.rds /tmp/RtmphQvIXi/file1aba36d9bf9b.rds example
#>   format            imported   user                              md5 rows
#> 1    rds 2026-11-02 02:13:50 runner b7b1e2209470af317912f3690978550f    8
#>   outputs check  state
#> 1    <NA>    ok in use
# }
options(old)
```
