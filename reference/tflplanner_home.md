# Where tflplanner keeps its settings and the studies' saved state

Where tflplanner keeps its settings and the studies' saved state

## Usage

``` r
tflplanner_home()
```

## Value

The home folder (it may not exist yet: see
[`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)).

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmphQvIXi/tflplanner-home1aba12ff73db is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmphQvIXi/tflplanner-home1aba12ff73db
#> new studies go to: /tmp/RtmphQvIXi/tflplanner-home1aba12ff73db/studies
tflplanner_home()
#> [1] "/tmp/RtmphQvIXi/tflplanner-home1aba12ff73db"
options(old)
```
