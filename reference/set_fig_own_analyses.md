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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18111c43bf is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18111c43bf
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18111c43bf/studies
# \donttest{
# (a few seconds: the sample study and its data)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18111c43bf/studies/SAMPLE-01
# the forest plot's hazard ratios, written as the figure's own analyses
design <- tflspec::tfl_fig_forest_analyses()
p <- set_fig_own_analyses(s$planner, "F-14-2-2", design, population_id = "SAF")
attr(p, "written")
#> [1] "HR"        "HR_SEX"    "HR_AGEGR1"
# }
options(old)
```
