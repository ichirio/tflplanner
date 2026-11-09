# Take a TOC in by the company's rules

`toc_import_read()` reads one TOC or several (a TFL workbook and a
listing workbook of the same layout) by the company's rules: each source
read once, its TOC sheet and header row found (`toc_find_sheet()`), the
rule set picked (`toc_pick_rules()`), the columns mapped
(`toc_map_for()`), the output ids made by the set's rule (or the one
that gives unique ids for the most rows, `toc_id_candidates()`), one
report a phase (`toc_expand_phases()`), the shell sheets linked
(`toc_link_shells()`), the Topline batch (`toc_batches()`) and what is
wrong (`toc_problems()`). Nothing is written.

## Usage

``` r
toc_import_read(
  sources,
  rules = toc_rules(),
  profile = NULL,
  choices = NULL,
  names = NULL
)
```

## Arguments

- sources:

  The TOC files (paths) or `toc_source`s.

- rules:

  What
  [`toc_rules()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)
  returned.

- profile:

  The study's TOC profile (`toc_profile()`) or `NULL`.

- choices:

  A list (see above) or `NULL`.

- names:

  The sources' names as the user knows them (paths: their base names).

## Value

A `toc_import`: `sources`, `detection` (a row a source: `file`, `sheet`,
`header_row`, `how` – `"name"`, `"score"`, `"rows"`, `"chosen"`,
`"profile"` –, `rows`, `shells`, `topline_sheet`, `kind`, `note`),
`sheets` (each source's `toc_find_sheet()`), `rule_set` (the set in
use), `rule_sets` (the scores), `map` (+ `attr(, "missing")`),
`id_rule`, `id_candidates`, `type_from`, `rows`, `reports`, `unread`,
`batches`, `problems`, `profile`, `notes` (what was not as the profile
said: `message`).

## Details

`toc_import_spec()` gives the reports as
[`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
gives a TOC, for
[`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md)
and
[`toc_apply()`](https://ichirio.github.io/tflplanner/reference/toc_apply.md).

What the user chose in the dialog (`choices`), else what the study's
profile remembers (`profile`), wins over what is found: `rule_set`,
`sheet` and `header_row` (named by the file's name), `map`, `id_rule`,
`type_from`, `overrides` (the set's settings changed for the study).

Two example TOCs come with the package, made from the sample study
(SAMPLE-01):
`system.file("extdata", "SAMPLE-01_TOC.xlsx", package = "tflplanner")` –
its reports as a company TOC (the header on row 2, section heading rows,
Shell Number, Phase, Topline, Title1-3, Population, Footnote1-2, a shell
sheet per report) – and `SAMPLE-01_TOC_changed.xlsx`, the same with two
rows changed (a title, a footnote). Taken into the sample study, the
first gives its reports their sections (and the Topline batch) and,
taken in again, changes nothing; the second, taken in after it, shows
those two.

## Examples

``` r
f <- tempfile(fileext = ".csv")
writeLines(c("Output ID,Title 1,Title 2,Population",
             "T-14-1-1,Demographics,,Safety Population",
             "T-14-2-1,Adverse events,Overview,Safety Population"), f)
r <- toc_import_read(f, toc_rules(home = tempfile()))
r$reports[c("output_id", "label")]
#>   output_id label
#> 1  T-14-1-1  <NA>
#> 2  T-14-2-1  <NA>

# the example TOC made from the sample study
f <- system.file("extdata", "SAMPLE-01_TOC.xlsx", package = "tflplanner")
r <- toc_import_read(f, toc_rules(home = tempfile()))
r$id_rule
#> [1] "column"
utils::head(r$reports[c("output_id", "label", "shell_sheet")])
#>   output_id         label shell_sheet
#> 1  T-14-0-1  Table 14.0.1    T-14-0-1
#> 2  T-14-1-1  Table 14.1.1    T-14-1-1
#> 3 T-14-1-1S Table 14.1.1S   T-14-1-1S
#> 4  T-14-1-2  Table 14.1.2    T-14-1-2
#> 5  T-14-1-3  Table 14.1.3    T-14-1-3
#> 6  T-14-1-4  Table 14.1.4    T-14-1-4
```
