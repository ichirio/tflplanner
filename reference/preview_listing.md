# The rows of a listing, as they will print

Runs a listing's data part
([`data_lines()`](https://ichirio.github.io/tflplanner/reference/data_lines.md))
from the study folder and gives its first pages, for a preview.

## Usage

``` r
preview_listing(study, output_id)
```

## Arguments

- study:

  An `rtfstudy`.

- output_id:

  The listing.

## Value

A list of `rtftable` pages.
