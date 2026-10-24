# Put what a TOC read by the rules says besides the spec into a study

The reports' batches (the Topline flag,
[`toc_batches()`](https://ichirio.github.io/tflplanner/reference/toc_batches.md))
go into the report list's `batches` column: a report flagged gets the
batch, one the last TOC flagged and this one does not loses it; a batch
given here is kept, and nothing changes when the TOC has no flag column.

## Usage

``` r
toc_apply_extras(x, result, last = NULL)
```

## Arguments

- x:

  A `tflplanner`, after
  [`toc_apply()`](https://ichirio.github.io/tflplanner/reference/toc_apply.md).

- result:

  What
  [`toc_import_read()`](https://ichirio.github.io/tflplanner/reference/toc_import_read.md)
  returned.

- last:

  The snapshot of the TOCs taken in before
  ([`toc_last()`](https://ichirio.github.io/tflplanner/reference/toc_imports.md)).

## Value

The `tflplanner`.
