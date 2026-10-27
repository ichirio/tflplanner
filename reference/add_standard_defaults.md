# Add the company's study defaults a study lacks

For a study made without them (before a new empty study started from the
company standards): each definition sheet with no study-default rows
gets the company's (`default_<sheet>`), and the analysis sets and data
catalog get the company's they do not have (by id). Nothing already
there is changed.

## Usage

``` r
add_standard_defaults(x, study_id)
```

## Arguments

- x:

  A `tflplanner`.

- study_id:

  The study's id (`{STUDY_ID}` in the defaults).

## Value

The `tflplanner`, with attribute `added` (the sheets added to).
