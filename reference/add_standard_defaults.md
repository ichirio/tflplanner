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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba69aded32 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba69aded32
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba69aded32/studies
x <- add_standard_defaults(new_planner(), "STUDY-1")
# the sheets that got the company's defaults
attr(x, "added")
#>  [1] "cells"       "digits"      "layout"      "columns"     "style"      
#>  [6] "col_header"  "report"      "header"      "footer"      "tokens"     
#> [11] "populations" "datasets"   
options(old)
```
