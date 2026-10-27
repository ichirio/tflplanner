# The ways a TOC's output id can be made, scored

Each rule of `toc_id_rules` (an id column as it is; normalised to the
set's `id_pattern`; a type column and a number column; the number in the
first title, "Table 14.1.1") is tried on the rows: the one that gives
unique, non-empty ids for the most rows is proposed (on a tie, an id
column as it is, unless its ids carry no type and the normalised ones
do).

## Usage

``` r
toc_id_candidates(rows, rule_set, type_from = NULL)
```

## Arguments

- rows:

  What
  [`toc_rows()`](https://ichirio.github.io/tflplanner/reference/toc_source.md)
  returned.

- rule_set:

  A rule set.

- type_from:

  Where the type is read (`"column"`, `"id"`, `"title"`); by default the
  set's, else the source that reads the most rows.

## Value

A data frame, the proposed first: `rule`, `ok` (rows with a unique id),
`filled` (rows with an id), `preview` (the first ids), `labels` (the
labels they print).
