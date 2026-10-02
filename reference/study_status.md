# What each report of a study has produced

One row per report: its program (`missing`; `current`, `todo` or
`generated` – untouched since tflplanner wrote it, `generated` meaning
the next save rewrites it; or `edited` by hand), whether its ARD and RTF
exist and when they were made, and a `status`:

## Usage

``` r
study_status(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame.

## Details

- `no program` – save the study to write it

- `unsaved` – the definition changed since the program was written; save
  the study

- `todo` – the program's data part is still to be written

- `not run` – no RTF yet

- `error` – the last run failed (see its log)

- `outdated` – the program or a definition workbook changed after the
  RTF was made

- `ok`
