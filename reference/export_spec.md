# Definition workbooks in and out

The study's definition lives in tflplanner's home and is written to the
study folder's `spec/` on every save (the table and report workbooks the
programs read; the ARD definition as `ard_definition.json`).
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
