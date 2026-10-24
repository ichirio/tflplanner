# A report's analysis set

The one value the report list, the TOC and step 1 share:
`set_report_population()` writes it, makes the analysis data of the
set's subjects (`adsl_<set>`) when there is none, and moves the report's
analyses from the set it had to the new one: the data of the old set's
subjects, and the data kept to them, to the new set's (found, or made
with the same definition); an analysis of a dataset and the old set to
the new set. An analysis data of another kind (made from another
analysis data, written as R) is left as it is and named in attribute
`left`. Analysis data are the study's: none is removed.

## Usage

``` r
set_report_population(x, output_id, population)
```

## Arguments

- x:

  A `tflplanner`.

- output_id:

  The report.

- population:

  A population_id of the study (`NA`: none).

## Value

The `tflplanner`, with attributes `made` (the analysis data added) and
`left`.
