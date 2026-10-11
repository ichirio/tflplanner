# Definition workbooks in and out

The study's definition is its folder's `spec/` (the table and report
workbooks the programs read; the ARD definition as
`ard_definition.json`), written on every save and read back on open when
it changed
([`reload_from_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)).
`export_spec()` writes the workbooks anywhere else – with
`ard_spec.xlsx`, the ARD definition, to edit in Excel or keep;
`import_spec()` replaces the study's definition with what a set of
workbooks says (an `ard_spec.xlsx` among them replaces the ARD
definition; save the study to keep it).

## Usage

``` r
export_spec(study, dir)

import_spec(study, path)
```

## Arguments

- study:

  An `rtfstudy`.

- dir:

  Destination folder.

- path:

  One or more `.xlsx` workbooks.

## Value

`export_spec()` the paths written; `import_spec()` the study.
