# TOCs taken into a study

`toc_imports()`: the record of every TOC taken in
(`input/toc/imports.csv`: `import_id`, `file`, `original`, `imported`,
`user`, `md5`, `reports`, `new`, `changed`, `missing`; for one taken in
by the company's rules also `rule_set`, `standards_md5` – the company
standards' md5 then, blank for the built-in ones –, `sources`, `unread`,
`warnings`; several files `|` between them). `toc_last()`: what the last
TOC taken in said, by report
([`toc_snapshot()`](https://ichirio.github.io/tflplanner/reference/toc_snapshot.md)),
or `NULL`. `toc_shell_links()`: each report's shell sheet
(`input/toc/shells.csv`: `output_id`, `file` – the copy in `input/toc`
–, `original`, `sheet`, `phase`, `md5`, `import_id`). `toc_profile()`:
what the study's last import chose (`input/toc/profile.yml`: the rule
set, the id rule, the overrides, each source's sheet and header row), or
`NULL`.

## Usage

``` r
toc_imports(study)

toc_last(study)

toc_shell_links(study)

toc_profile(study)
```

## Arguments

- study:

  An `rtfstudy`.

## Value

A data frame; a list or `NULL`.
