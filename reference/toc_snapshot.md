# What a TOC said, kept for the next time one is taken in

What a TOC said, kept for the next time one is taken in

## Usage

``` r
toc_snapshot(spec, title_offset = 0L, last = NULL)
```

## Arguments

- spec:

  What
  [`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
  returned.

- title_offset:

  As
  [`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md).

- last:

  The snapshot of the TOCs taken in before: the reports they gave that
  this TOC has not are kept (`in_toc = FALSE`), so a report once taken
  in from a TOC is said to be missing every time.

## Value

A list by output id: its `titles` and `footnotes` rows (as the study
numbers them), `type` and `in_toc`.
