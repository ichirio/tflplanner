# Add the AI sample study

Makes the sample for the AI tab as a study of its own, without the app:
the same as the app's *New study* dialog with *The sample for the AI
tab*. 'tflspec' writes the sample's materials (a TOC, shells, ADaM data,
an ADaM spec and a SAP; no answers) into `samples/ai-sample` under the
home; the study `root/<study_id>` is then made over the sample's ADaM
([`tflspec::tfl_ai_study_init()`](https://ichirio.github.io/tflspec/reference/tfl_ai_study_init.html))
with the TOC, the shells and the ADaM spec in its `docs/`, the ADaM in
`data/adam/` and the SAP in `ai/docs/`, and registered. Everything in it
is fictional.

## Usage

``` r
create_ai_sample_study(
  root = studies_root(home),
  study_id = "DMY-001",
  title = "Example Pharma DMY-001 (fictional)",
  home = tflplanner_home()
)
```

## Arguments

- root:

  The studies folder (default: where new studies go, see
  [`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md)).

- study_id:

  The new study's ID (default `DMY-001`).

- title:

  The study's title.

- home:

  tflplanner's home.

## Value

`list(study, kit)`, invisibly: the study, and the `kit` of paths
described above.

## Details

The returned `kit` holds the paths of the materials inside the study:
`toc`, `shells`, `adam`, `adam_spec` and `docs`. These are what
[`tflspec::tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.html)
takes (`toc`, `shells`, `adam`, `adam_spec` and `docs`), the same fields
the AI tab's step 1 is filled with.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmpTnNlOL/tflplanner-home1b18769c53d4 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmpTnNlOL/tflplanner-home1b18769c53d4
#> new studies go to: /tmp/RtmpTnNlOL/tflplanner-home1b18769c53d4/studies
# \donttest{
# (a few seconds: it writes the sample's materials and the study)
made <- create_ai_sample_study()
kit <- made$kit
# then, as the AI tab's step 1 does:
p <- tflspec::tfl_ai_prepare(made$study$path, toc = kit$toc, adam = kit$adam,
                             shells = kit$shells, adam_spec = kit$adam_spec,
                             docs = kit$docs, dry_run = TRUE)
# }
options(old)
```
