# The analysis set each report of a TOC names

The TOC's population column, by report: its text (`text`, "Safety
Population") and the study's analysis set it is (`population_id`: its
id, its label, a usual word for its flag, or its id in the text; `NA`
when none).

## Usage

``` r
toc_populations(x, path, id_col, pop_col, sheet = NULL, skip = 0L, data = NULL)
```

## Arguments

- x:

  A `tflplanner`.

- path, sheet, skip:

  The TOC, as
  [`tflspec::tfl_read_toc()`](https://ichirio.github.io/tflspec/reference/tfl_read_toc.html)
  reads it.

- id_col, pop_col:

  Its columns of the report IDs and the populations.

- data:

  The data of the analysis sets (ADSL), for its flags' labels.

## Value

A data frame: output_id, text, population_id.
