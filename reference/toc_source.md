# A TOC's source file, read once

`toc_source()` reads a workbook's sheet names and the first rows of
every sheet (a `.csv` is one sheet); the rest of a sheet is read when it
is needed, once. `toc_find_header()` finds the header row of a sheet's
first rows; `toc_find_sheet()` the TOC sheet of a source; `toc_rows()`
reads the TOC's rows under its header through a map.

## Usage

``` r
toc_source(path, header_rows = 10L, name = basename(path))

toc_find_header(head, rule_set)

toc_find_sheet(source, rules)

toc_rows(source, sheet, header_row, map)
```

## Arguments

- path:

  An `.xlsx`, `.xls` or `.csv` file.

- header_rows:

  How many rows from the top are read at first (the header is looked for
  in them).

- name:

  The file's name as the user knows it (a file chosen in a browser has a
  temporary path).

- head:

  A sheet's first rows (a character matrix, as `source$head` holds
  them).

- rule_set:

  A rule set
  ([`toc_rule_set()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md))
  or a list of them (the candidates of all are used).

- source:

  A `toc_source`.

- rules:

  What
  [`toc_rules()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)
  returned (every set's candidates are used), or one rule set.

- sheet:

  The TOC sheet.

- header_row:

  Its header row (as the sheet numbers its rows).

- map:

  Which column is what: a named list, item -\> column name(s) of the
  header
  ([`toc_map_for()`](https://ichirio.github.io/tflplanner/reference/toc_rules.md)).

## Value

`toc_source()`: a `toc_source`: `path`, `name`, `md5`, `kind`
(`"excel"`, `"csv"` or `"other"`), `sheets`, `head` (each sheet's first
rows as a character matrix).

`toc_find_header()`: a list: `row` (`NA` when no row matches), `score`
(the cells that match a column name), `items` (the items they are),
`toc` (`TRUE` when the row is a TOC's header: a title column and one
item more).

`toc_find_sheet()`: a data frame, a row a sheet, the chosen first:
`sheet`, `role` (`"toc"`, `"topline"`, `"other"` – another TOC-like
sheet –, `"shell"` – named like a report number – or `""`), `name_hit`,
`header_row`, `score`, `items`, `rows` (rows under the header),
`shell_like`, `why`; `attr(, "kind")`: `"toc"`, `"shells"` (no TOC,
shell sheets) or `"none"`.

`toc_rows()`: a data frame, a row a non-blank row under the header:
`.source`, `.sheet`, `.row` (the sheet's row number), a column an item
(`NA` when not mapped), `titles` and `footnotes` as list columns (a
title column's cell each, `NA` kept, so a title column's place is
known).

## Details

Sheet: a sheet named as the rule set names a TOC sheet (`sheet_names`)
is taken; else the sheet whose best row of the first `header_rows`
matches the most column names (a TOC has a title column and one item
more at least), ties broken by more rows, then by more sheets named like
its report numbers (shells). A second TOC-like sheet – named as a
Topline sheet, or whose reports are some of the TOC's – is the Topline
cross-check, never the source.
