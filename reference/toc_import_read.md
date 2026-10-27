# Take a TOC in by the company's rules

`toc_import_read()` reads one TOC or several (a TFL workbook and a
listing workbook of the same layout) by the company's rules: each source
read once, its TOC sheet and header row found
([`toc_find_sheet()`](https://ichirio.github.io/tflplanner/reference/toc_source.md)),
the rule set picked
([`toc_pick_rules()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)),
the columns mapped
([`toc_map_for()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)),
the output ids made by the set's rule (or the one that gives unique ids
for the most rows,
[`toc_id_candidates()`](https://ichirio.github.io/tflplanner/reference/toc_id_candidates.md)),
one report a phase
([`toc_expand_phases()`](https://ichirio.github.io/tflplanner/reference/toc_expand_phases.md)),
the shell sheets linked
([`toc_link_shells()`](https://ichirio.github.io/tflplanner/reference/toc_link_shells.md)),
the Topline batch
([`toc_batches()`](https://ichirio.github.io/tflplanner/reference/toc_batches.md))
and what is wrong (`toc_problems()`). Nothing is written.

## Usage

``` r
toc_problems(x, lang = "en")

toc_import_read(
  sources,
  rules = toc_rules(),
  profile = NULL,
  choices = NULL,
  names = NULL
)

toc_import_spec(x)
```

## Arguments

- x:

  What `toc_import_read()` returned.

- lang:

  The language of the messages (`"en"`, `"ja"`).

- sources:

  The TOC files (paths) or `toc_source`s.

- rules:

  What
  [`toc_rules()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)
  returned.

- profile:

  The study's TOC profile
  ([`toc_profile()`](https://ichirio.github.io/tflplanner/reference/toc_imports.md))
  or `NULL`.

- choices:

  A list (see above) or `NULL`.

- names:

  The sources' names as the user knows them (paths: their base names).

## Value

`toc_problems()`: the problems as rows of the review's shape (#288:
`output_id`, `level` – `error`, `check`, `hand` –, `area` `"toc"`,
`sheet`, `row`, `field`, `message`, `hint`, `rule`, `draft`), errors
first.

A `toc_import`: `sources`, `detection` (a row a source: `file`, `sheet`,
`header_row`, `how` – `"name"`, `"score"`, `"rows"`, `"chosen"`,
`"profile"` –, `rows`, `shells`, `topline_sheet`, `kind`, `note`),
`sheets` (each source's
[`toc_find_sheet()`](https://ichirio.github.io/tflplanner/reference/toc_source.md)),
`rule_set` (the set in use), `rule_sets` (the scores), `map` (+
`attr(, "missing")`), `id_rule`, `id_candidates`, `type_from`, `rows`,
`reports`, `unread`, `batches`, `problems`, `profile`, `notes` (what was
not as the profile said: `message`).

`toc_import_spec()`: what
[`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
returns for the reports (a table spec with the report sheets and their
attributes); `attr(, "guessed")` the reports whose type no type column
said (read from the id or the title: to check).

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
toc_import_spec(r)$report
#>   output_id  type file program auto_section section_align auto_title
#> 1  T-14-1-1 table <NA>    <NA>         <NA>          <NA>       <NA>
#> 2  T-14-2-1 table <NA>    <NA>         <NA>          <NA>       <NA>
#>   title_align table_font_size_half_points title_font_size_half_points
#> 1        <NA>                        <NA>                        <NA>
#> 2        <NA>                        <NA>                        <NA>
#>   footnote_font_size_half_points page_header page_footer watermark page_break
#> 1                           <NA>        <NA>        <NA>      <NA>       <NA>
#> 2                           <NA>        <NA>        <NA>      <NA>       <NA>
#>   space_before first_rows cont_title figure_width_in figure_height_in
#> 1         <NA>       <NA>       <NA>            <NA>             <NA>
#> 2         <NA>       <NA>       <NA>            <NA>             <NA>
#>   ard_source pattern data_code content_code ard_object note
#> 1       <NA>    <NA>      <NA>         <NA>       <NA> <NA>
#> 2       <NA>    <NA>      <NA>         <NA>       <NA> <NA>

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
