# Each report's shell sheet

Across every source, the sheet named as the report's shell (its `shell`
column), its id column as it is or its output id – the phase's
`shell_phase_suffix` first (`t.14.1.1-1`), else without – matched
ignoring case; then the same by key (`T-14-1-1` is `t.14.1.1`). Only the
link is kept: nothing of the sheet is read.

## Usage

``` r
toc_link_shells(reports, sources, rule_set, skip = NULL)
```

## Arguments

- reports:

  What
  [`toc_expand_phases()`](https://ichirio.github.io/tflplanner/reference/toc_expand_phases.md)
  returned.

- sources:

  The `toc_source`s.

- rule_set:

  A rule set.

- skip:

  Sheets that are not shells: a data frame `source`, `sheet` (the TOC
  sheets, the Topline sheet).

## Value

`reports` with `shell_file`, `shell_sheet` (`NA`: none);
`attr(, "unlinked")`: sheets named like a report number no report links
to (`source`, `sheet`).
