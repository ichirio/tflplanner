# ARDs taken into a study

The record of every ARD taken in
([`import_ard()`](https://ichirio.github.io/tflplanner/reference/import_ard.md)):
`import_id`, the `file` in the study's `input/ard/` folder, the
`original` file, its `source` (who made it), `format`, when it was
`imported` and by which `user`, its `md5`, its `rows`, the `outputs` it
has rows for (its `output_id` column), the result of the `check`, and
its `state` (`"in use"`, or `"removed"` – a removed one stays on the
record).

## Usage

``` r
ard_imports(study)
```

## Arguments

- study:

  An `rtfstudy`
  ([`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)).

## Value

A data frame, one row per ARD taken in.
