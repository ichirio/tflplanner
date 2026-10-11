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
