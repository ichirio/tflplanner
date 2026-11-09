# tflplanner's settings

tflplanner's settings

## Usage

``` r
tflplanner_config(home = tflplanner_home())

studies_root(home = tflplanner_home())
```

## Arguments

- home:

  The home folder.

## Value

A list: `studies_root`, `last_study`, `language`, and when set `port`,
`update_channel`, `check_updates`.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b186ae2779e is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b186ae2779e
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b186ae2779e/studies
tflplanner_config()$last_study
#> NULL
options(old)
```
