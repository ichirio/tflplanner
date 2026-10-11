# Put a TOC into a study's definition

Adds the TOC's new reports (their type, program, file, note, titles and
footnotes) and, for the reports already there, puts in what the TOC
holds by the rule
[`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md)
describes: a line added, updated or removed; one edited here and changed
in the TOC only when `use_toc` says so; the lines added here after the
TOC's. A report's type is not changed, nor anything the TOC does not
hold; a report no longer in the TOC stays.

## Usage

``` r
toc_apply(
  x,
  spec,
  changes,
  use_toc = character(),
  types = character(),
  last = changes$last,
  title_offset = changes$title_offset %||% toc_title_offset(x),
  populations = character(),
  make_data = FALSE,
  again = FALSE
)
```

## Arguments

- x:

  A `tflplanner`.

- spec:

  What
  [`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
  returned.

- changes:

  What
  [`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md)
  returned.

- use_toc:

  The `"ask"` lines to take from the TOC, as
  `"<output_id>|<sheet>|<line>"`.

- types:

  The types of new reports, named by output id (a guessed type
  corrected); the TOC's otherwise.

- last, title_offset:

  As
  [`toc_changes()`](https://ichirio.github.io/tflplanner/reference/toc_changes.md)
  was given them (kept in `changes`).

- populations:

  The reports' analysis sets, a population_id named by report (see
  [`toc_populations()`](https://ichirio.github.io/tflplanner/reference/toc_populations.md)):
  each one that differs from the report's is set with
  [`set_report_population()`](https://ichirio.github.io/tflplanner/reference/set_report_population.md)
  (its data made, its analyses moved).

- make_data:

  Make the analysis data of each new report's datasets (the TOC's
  `datasets`): `<dataset>_<set>`, kept to the subjects of its analysis
  set (`adsl_<set>`), found or made.

- again:

  With `make_data`, also for the reports taken in before (those deleted
  since are made again).

## Value

The `tflplanner`.
