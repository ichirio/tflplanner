# Copy how a study reads its TOC to another study

Copies the study's TOC profile (`input/toc/profile.yml`: the rule set,
the way the report IDs are made, the sheets and header rows, the phase
settings) to another study, as copying a study in the app does. Not the
TOCs it took in nor what they said (`last.json`): the other study's TOC
is its own, and its first one is compared with its own lines.

## Usage

``` r
toc_copy_profile(from, to)
```

## Arguments

- from, to:

  `rtfstudy` objects
  ([`open_study()`](https://ichirio.github.io/tflplanner/reference/create_study.md)).

## Value

`TRUE` when there was a profile to copy, invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b1872832b6a is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b1872832b6a
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b1872832b6a/studies
# \donttest{
# (a few seconds: it makes two studies)
from <- create_sample_study(run = FALSE)
#> The sample study: /tmp/RtmpTnNlOL/tflplanner-home1b1872832b6a/studies/SAMPLE-01
to <- create_study("OTHER-01")
toc_copy_profile(from, to)  # FALSE: the sample took in no TOC, no profile
# }
options(old)
```
