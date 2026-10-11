# TOCs taken into a study

`toc_imports()`: the record of every TOC taken in
(`input/toc/imports.csv`: `import_id`, `file`, `original`, `imported`,
`user`, `md5`, `reports`, `new`, `changed`, `missing`). `toc_last()`:
what the last TOC taken in said, by report
([`toc_snapshot()`](https://ichirio.github.io/tflplanner/reference/toc_snapshot.md)),
or `NULL`.

## Usage

``` r
toc_imports(study)

toc_last(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame; a list or `NULL`.
