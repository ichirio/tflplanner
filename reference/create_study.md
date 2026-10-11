# Create, open, register and list studies

A study is known to tflplanner by its saved state in the home
([`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md));
its folder holds the data, the programs and the deliverables.

## Usage

``` r
create_study(
  study_id,
  title = NA,
  compound = NA,
  phase = NA,
  description = NA,
  planner = NULL,
  root = studies_root(home),
  home = tflplanner_home()
)

open_study(study, home = tflplanner_home())

register_study(path, home = tflplanner_home())

unregister_study(study_id, home = tflplanner_home())

list_studies(home = tflplanner_home())
```

## Arguments

- study_id:

  The study's id, which is also its folder name.

- title, compound, phase, description:

  What the study is.

- planner:

  An `tflplanner` to start from (e.g. an earlier study's
  `open_study(...)$planner`); `NULL` starts from the company standards:
  their study-default rows, analysis sets and data catalog.

- root:

  The folder the new study folder goes in; defaults to the one set up
  with
  [`setup_tflplanner()`](https://ichirio.github.io/tflplanner/reference/setup_tflplanner.md).

- home:

  tflplanner's home.

- study:

  A registered study's id, or a study folder.

- path:

  A study folder.

## Value

`create_study()`, `open_study()` and `register_study()` return an
`rtfstudy`: `path`, `meta` (the study.yml fields) and `planner`.
`list_studies()` returns a data frame.

## Details

- `create_study()` makes the study folder – layout, `study.yml`, an
  RStudio project – and saves the study, which registers it.

- `open_study()` returns a study as it was last saved, its definition
  files compared with what tflplanner last wrote or read: one changed
  outside tflplanner (edited in Excel, copied in) is read and taken in
  (`$spec` says what changed; see
  [`reload_from_spec()`](https://ichirio.github.io/tflplanner/reference/reload_from_spec.md)).
  Given the folder of a study tflplanner does not know yet, it registers
  it first.

- `register_study()` adds an existing study folder: its `study.yml`, and
  its definition workbooks when `spec/` has them. A folder unregistered
  before comes back as it was (its saved state, history and unsaved
  changes).

- `unregister_study()` takes a study off the list. Its folder is not
  deleted: what tflplanner kept about it goes into the folder
  (`.tflplanner/`), for `register_study()` to take back. tflplanner
  never deletes a study folder; to delete one, delete it yourself (in
  the file manager it goes to the recycle bin).

- `list_studies()` lists the registered studies.

## Examples

``` r
# a home in the temporary folder: used in this R session only, nothing
# is written to your settings
old <- options(tflplanner.home = NULL)
setup_tflplanner(home = tempfile("tflplanner-home"))
#> The home /tmp/RtmppeoQLT/tflplanner-home1a495205c548 is in the temporary folder: it is used in this R session only, not remembered for later ones.
#> tflplanner home: /tmp/RtmppeoQLT/tflplanner-home1a495205c548
#> new studies go to: /tmp/RtmppeoQLT/tflplanner-home1a495205c548/workspace
# \donttest{
# (a few seconds: it writes the study's spec workbooks)
s <- create_study("ABC-101", title = "A phase 2 study")
list_studies()
#>   study_id           title compound phase               saved
#> 1  ABC-101 A phase 2 study     <NA>  <NA> 2026-10-11 04:31:47
#>                                                            path folder
#> 1 /tmp/RtmppeoQLT/tflplanner-home1a495205c548/workspace/ABC-101   TRUE
#>   description reports analyses
#> 1        <NA>       0        0
s <- open_study("ABC-101")
# }
options(old)
```
