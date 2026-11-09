# The rows of a listing, as they will print

Runs a listing's data part (`data_lines()`) from the study folder and
gives its first pages, for a preview.

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

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18baf7fd5 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18baf7fd5
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18baf7fd5/studies
# \donttest{
# (a few seconds: it runs the listing's data part)
s <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b18baf7fd5/studies/SAMPLE-01
# runs the listing's data part in a fresh R process
pages <- preview_listing(s, "L-16-2-7")
length(pages)
#> [1] 19
# }
options(old)
```
