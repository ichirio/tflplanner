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

toc_rule_set(rules, name = NULL, overrides = NULL)

toc_map_for(headers, std = company_standards()$toc_map, rule_set = NULL)

toc_pick_rules(headers, rules)
```

## Arguments

- home:

  tflplanner's home.

- standards:

  The company standards (a list of sheets); by default those in use.

- rules:

  What `toc_rules()` returned.

- name:

  A rule set's name.

- overrides:

  Settings a study changes (its profile): a named list (`number_suffix`,
  `title_suffix`, `title_line`, ...).

- headers:

  A TOC's column names.

- std:

  A rule set, what `toc_rules()` returned, or the standards' `toc_map`
  sheet.

- rule_set:

  The rule set's name (`std` a list of sets or the `toc_map` sheet); by
  default `standard`.

## Value

`toc_rules()`: a named list of `toc_rule_set`s (`name`, `label`,
`settings`, `map`: `item`, `columns`).

`toc_rule_set()`: a `toc_rule_set`.

`toc_map_for()`: a named list, item -\> the TOC's column (an item: the
first of its candidates the TOC has; `title` and `footnote`: every one,
in the TOC's order); `attr(, "missing")`: the items the set names that
the TOC has no column for.

`toc_pick_rules()`: a data frame, the best set first: `rule_set`,
`label`, `required_ok`, `matched` (items with a column), `named` (items
the set names), `items`.

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
toc_pick_rules(c("Shell Number", "Title1", "Title2", "g-", "Topline"), r)
#>    rule_set      label required_ok matched named
#> 1 two-phase Two phases        TRUE       4    17
#> 2  standard   Standard       FALSE       1    11
#>                              items
#> 1 output_id, title, phase, topline
#> 2                            title
```
