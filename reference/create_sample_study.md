# Add the sample study

Copies tflplanner's sample study into a studies folder and registers it:
the CDISC pilot ADaM data of the 'pharmaverseadam' package (ADSL, ADAE,
ADVS, and ADTTE derived from them), the ARD definition, five tables made
from one study ARD, a listing and two figures – every step, from the ARD
to the reports, defined in the app and written as Excel (`spec/`: the
table and report definitions, the listing / figure definition and
`ard_spec.xlsx`, an export of the ARD definition to read). With
`run = TRUE` it then makes them – the ARD programs and the report
programs, as an official run with its batch folder (see
[`run_batch()`](https://ichirio.github.io/tflplanner/reference/run_batch.md))
– so the study opens with its ARD and RTFs in place.
[`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)`(sample = TRUE)`
does this at setup, and the app's *New study* dialog under a study ID of
your own.

## Usage

``` r
create_sample_study(
  root = studies_root(home),
  run = TRUE,
  study_id = .sample_id,
  title = NULL,
  compound = NULL,
  phase = NULL,
  description = NULL,
  home = tflplanner_home()
)
```

## Arguments

- root:

  The studies folder (default: where new studies go, see
  [`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)).

- run:

  Make the ARD and the reports (needs 'cards' and 'cardx').

- study_id:

  The new study's ID (default `SAMPLE-01`).

- title, compound, phase, description:

  The study's fields; `NULL` keeps the sample's.

- home:

  tflplanner's home.

## Value

The study, invisibly.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmppeoQLT/tflplanner-home1a4934249461 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmppeoQLT/tflplanner-home1a4934249461
#> new studies go to: /tmp/RtmppeoQLT/tflplanner-home1a4934249461/workspace
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_sample_study(run = FALSE)  # run = TRUE makes its ARD and reports too
#> The sample study: /tmp/RtmppeoQLT/tflplanner-home1a4934249461/workspace/SAMPLE-01
list_studies()
#>    study_id                                            title   compound phase
#> 1 SAMPLE-01 Sample study (CDISC pilot data, pharmaverseadam) Xanomeline     2
#>                 saved
#> 1 2026-10-11 04:31:43
#>                                                              path folder
#> 1 /tmp/RtmppeoQLT/tflplanner-home1a4934249461/workspace/SAMPLE-01   TRUE
#>                                                                                                                                                description
#> 1 The sample study of tflplanner: the CDISC pilot ADaM data of the pharmaverseadam package; one study ARD and tables, a listing and a figure made from it.
#>   reports analyses
#> 1      17       36
# }
options(old)
```
