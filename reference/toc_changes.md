# What taking in a TOC would change

Compares the report sheets a TOC gives
([`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html))
with the study's definition and with what the last TOC taken in said
(`last`, see
[`toc_snapshot()`](https://ichirio.github.io/tflplanner/reference/toc_snapshot.md)).

## Usage

``` r
toc_changes(x, spec, last = NULL, title_offset = toc_title_offset(x))
```

## Arguments

- x:

  A `tflplanner`.

- spec:

  What
  [`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
  returned.

- last:

  The snapshot of the TOCs taken in before, or `NULL` the first time.

- title_offset:

  The study's own title lines (see `toc_title_offset()`): a TOC's title
  line 1 is the line after them.

## Value

A list: `reports` (one row a report: `output_id`, `status` – `"new"`,
`"changed"`, `"same"` or `"missing"` (taken in from a TOC before, not in
this one) –, `type_now`, `type_toc`, `guessed`), `lines` (one row a
title or footnote line that changes: `output_id`, `sheet`, `line` (its
place among the TOC's lines), `now`, `toc`, `action` – `"add"`,
`"update"`, `"remove"` (the TOC dropped a line not edited here), `"ask"`
(edited here and changed in the TOC) or `"move"` (a line added here, now
after the TOC's lines: `to`) – and `to`, the line it will be), `kept`
(the lines edited here the TOC did not change: kept unless chosen),
`info` (one row an item of a report's information that differs:
`output_id`, `name` (`"OUTPUT_LABEL"`, `"OUTPUT_TITLE"`,
`"OUTPUT_POPULATION"`, `"OUTPUT_SECTION"`), `now`, `toc`, `action` –
`"add"`, `"update"`, `"keep"` (edited or cleared here, the TOC
unchanged) or `"ask"`); and the `last` and `title_offset` it was given.

## Details

A report's title and footnote lines are of two kinds: those the TOC gave
(the lines the last TOC had; the first time, the lines where this TOC
has one) and those added here. The TOC's lines are compared line by
line: one not edited here takes the TOC's new text; one edited here
keeps its text unless the TOC's is chosen (`use_toc` of
[`toc_apply()`](https://ichirio.github.io/tflplanner/reference/toc_apply.md)),
when the TOC did not change it as when it did. The lines added here are
not compared: they stay, after the TOC's lines. The report's information
– its label, title and analysis set text (those the study's pages say)
and its section – is compared the same way.
