# One report a phase

A TOC row whose phase value means two phases (`both`) gives two reports,
one that means one phase gives one, each with the phase's suffix on its
output id and label (`number_suffix`) and on one title line
(`title_suffix` on `title_line`); the analysis set is not changed. No
phase column: one report a row, no suffix. A blank phase is
`phase_blank` (`none`: one report, no suffix; `all`: every phase; or a
value of `phase_values`). A value the rules do not know leaves the row
out (listed in `attr(, "unread")`).

## Usage

``` r
toc_expand_phases(rows, rule_set)
```

## Arguments

- rows:

  The TOC's rows with their ids:
  [`toc_rows()`](https://ichirio.github.io/tflplanner/reference/toc_source.md)
  plus `output_id`, `type`, `label`, `raw_id`.

- rule_set:

  A rule set (with a study's overrides).

## Value

The reports: a row a report, `output_id`, `base_id`, `phase` (1, 2, ...
or `NA`), `label`, `titles` and `footnotes` (list columns), the rest of
the row; `attr(, "unread")`: the rows left out.
