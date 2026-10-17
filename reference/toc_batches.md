# The Topline batch of a TOC

The reports whose Topline flag is one of the set's `topline_values` (a
row of two phases gives both reports) form the batch `topline_batch`. No
flag column: no batch. The full run is every report and is not a batch.
A Topline sheet, when there is one, is compared: a report flagged but
not on it, or on it but not flagged, is listed (a check, never an error;
the flag decides).

## Usage

``` r
toc_batches(reports, rule_set, topline_ids = NULL)
```

## Arguments

- reports:

  What
  [`toc_link_shells()`](https://ichirio.github.io/tflplanner/reference/toc_link_shells.md)
  (or
  [`toc_expand_phases()`](https://ichirio.github.io/tflplanner/reference/toc_expand_phases.md))
  returned.

- rule_set:

  A rule set.

- topline_ids:

  The output ids (before the phases) of a Topline sheet, or `NULL`.

## Value

A data frame `batch`, `output_id`; `attr(, "differ")`: `id`, `what`
(`"flagged_not_on_sheet"`, `"on_sheet_not_flagged"`).
