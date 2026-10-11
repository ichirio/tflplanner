# The study's definition files: read them, compare them, write them back

A study's definition is its folder's files: `spec/` (the table and
report workbooks, the ARD definition, the listing and figure workbook,
the figure designs) and `study.yml`. tflplanner keeps a copy of them in
its home, with a fingerprint of each file;
[`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)
reads the files that changed since, so an edit made directly in them (in
Excel, by a script) is what the study opens with. A file that does not
read (a sheet tflspec does not know, a broken row) stops nothing: the
study opens as last saved, and cannot be saved until one of these is
done.

## Usage

``` r
reload_from_spec(study, home = tflplanner_home())

write_spec(study, home = tflplanner_home())

spec_status(study, home = tflplanner_home())
```

## Arguments

- study:

  A registered study's id, or an `rtfstudy`.

- home:

  tflplanner's home.

## Value

`reload_from_spec()` and `write_spec()`: the study (its `spec` says what
was read). `spec_status()`: a data frame, `file`, `status` (`same`,
`changed`, `added`, `removed`), `size_was`, `size_now`.

## Details

- `reload_from_spec()` reads every definition file and takes them in
  (after fixing them, or to take in workbooks copied into `spec/` by
  hand). It fails with class `tflplanner_spec_invalid` (its `problems`
  the file, sheet, row and message of each) when they do not read.

- `write_spec()` writes the files again from the study as last saved;
  the files it replaces are copied to `spec/.rejected/` first.

- `spec_status()` says which files changed since tflplanner last wrote
  or read them, without reading them.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmppeoQLT/tflplanner-home1a4960959197 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmppeoQLT/tflplanner-home1a4960959197
#> new studies go to: /tmp/RtmppeoQLT/tflplanner-home1a4960959197/workspace
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_study("ABC-101", title = "A phase 2 study")
spec_status("ABC-101")               # nothing changed outside tflplanner
#>                       file status size_was size_now
#> 1                study.yml   same      154      154
#> 2     spec/table_spec.xlsx   same    47166    47166
#> 3    spec/report_spec.xlsx   same    35278    35278
#> 4 spec/ard_definition.json   same     1898     1898
s <- reload_from_spec("ABC-101")
# }
options(old)
```
