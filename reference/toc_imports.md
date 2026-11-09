# TOCs taken into a study

`toc_imports()`: the record of every TOC taken in
(`input/toc/imports.csv`: `import_id`, `file`, `original`, `imported`,
`user`, `md5`, `reports`, `new`, `changed`, `missing`; for one taken in
by the company's rules also `rule_set`, `standards_md5` – the company
standards' md5 then, blank for the built-in ones –, `sources`, `unread`,
`warnings`; several files `|` between them). `toc_last()`: what the last
TOC taken in said, by report (`toc_snapshot()`), or `NULL`.
`toc_shell_links()`: each report's shell sheet (`input/toc/shells.csv`:
`output_id`, `file` – the copy in `input/toc` –, `original`, `sheet`,
`phase`, `md5`, `import_id`). `toc_profile()`: what the study's last
import chose (`input/toc/profile.yml`: the rule set, the id rule, the
overrides, each source's sheet and header row), or `NULL`.

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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b185e8e4034 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b185e8e4034
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b185e8e4034/studies
# \donttest{
# (a few seconds: it makes the sample study)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b185e8e4034/studies/SAMPLE-01
toc_imports(s)  # no TOC taken in yet: no rows
#>  [1] import_id     file          original      imported      user         
#>  [6] md5           reports       new           changed       missing      
#> [11] rule_set      standards_md5 sources       unread        warnings     
#> <0 rows> (or 0-length row.names)
toc_last(s)     # NULL: nothing said by a last TOC
#> NULL
# }
options(old)
```
