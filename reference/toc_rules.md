# The company's rules for taking in a TOC

A company keeps one rule set or several for its TOCs, in the company
standards workbook
([`company_standards()`](https://ichirio.github.io/tflplanner/reference/standards_template.md)):
the sheet `toc_map` (an item's candidate column names, `|` between them,
by `rule_set`; blank is `standard`) and the sheet `toc_rules` (a row a
set: its sheet names, header rows, phases, suffixes, Topline flag, shell
sheet names, how the output id is made; a blank takes the default). A
workbook written before either reads as one set, `standard`, with the
defaults.

## Usage

``` r
toc_rules(home = tflplanner_home(), standards = NULL)
```

## Arguments

- home:

  tflplanner's home.

- standards:

  The company standards (a list of sheets); by default those in use.

## Value

`toc_rules()`: a named list of `toc_rule_set`s (`name`, `label`,
`settings`, `map`: `item`, `columns`).

## Details

- `toc_rules()`: every rule set, its settings with the defaults filled.

- `toc_rule_set()`: one, by name.

- `toc_pick_rules()`: the sets scored on a TOC's column names: a set
  whose title item has no column is out; the most items matched wins,
  then the set's order.

- `toc_map_for()`: which of a TOC's columns is what, by a rule set.

## Examples

``` r
r <- toc_rules(home = tempfile())
names(r)
#> [1] "standard"  "two-phase"
```
