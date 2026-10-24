# A figure's own analyses, written from its design

A design made from a template that brings its analyses – the forest
plot's hazard ratios, `attr(design, "analyses")` from
[`tflspec::tfl_fig_forest_analyses()`](https://ichirio.github.io/tflspec/reference/tfl_fig_forest_analyses.html)
– writes them to the figure's ARD definition: its `analysis_data` and
`analyses` rows (a row of the same id is replaced, the others kept) and
the report row's `ard_source = "own"`. The study's ARD then has to be
made again for the figure.

## Usage

``` r
set_fig_own_analyses(x, output_id, analyses, population_id = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The figure.

- analyses:

  A list of two data frames, `analysis_data` and `analyses` (without
  `output_id`), as a design's attribute `analyses`.

- population_id:

  The report's population, by its id, put on the analysis data (a
  template names the population's flag, not the study's id for it);
  `NULL` keeps what `analyses` says.

## Value

The `tflplanner`, with attribute `written`: the ids of the analyses
written.
