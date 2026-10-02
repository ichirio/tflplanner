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

- `open_study()` returns a study as it was last saved. Given the folder
  of a study tflplanner does not know yet, it registers it first.

- `register_study()` adds an existing study folder: its `study.yml`, and
  its definition workbooks when `spec/` has them.

- `unregister_study()` forgets a study; its folder is left alone.

- `list_studies()` lists the registered studies.

## Examples

``` r
if (FALSE) { # \dontrun{
s <- create_study("ABC-101", title = "A phase 2 study")
list_studies()
s <- open_study("ABC-101")
} # }
```
