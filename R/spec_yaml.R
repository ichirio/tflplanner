# The study definition as tflspec's YAML spec (2026-10-15): study.yml and
# spec/ (data.yml, defaults.yml, codelists.yml, toc.yml, patterns/,
# outputs/<id>.yml), one file a report.  The app keeps its planner object --
# the sheets every screen edits -- and reads and writes the YAML through
# tflspec (tfl_sheets_from_yaml(), tfl_yaml_from_sheets()).  A study whose
# spec/ still holds the workbooks is of the sheets' format: it opens as
# before until it is converted.

# The format of a study folder's definition: "yaml" (spec/outputs/),
# "sheets" (the workbooks, the ARD's JSON) or "none" (nothing yet)
.study_format <- function(root) {
  sp <- file.path(root, study_layout()[["spec"]])
  if (dir.exists(file.path(sp, "outputs"))) return("yaml")
  old <- c(.table_file, .report_file, .ard_json, .ard_file, .lf_file)
  if (any(file.exists(file.path(sp, old)))) "sheets" else "none"
}

# The YAML spec's files, relative to the study folder (the converter's list
# of what it could not convert and the backups are not the study's)
.spec_files_yaml <- function(root) {
  sp <- study_layout()[["spec"]]
  f <- c(.study_file, file.path(sp, c("data.yml", "defaults.yml", "codelists.yml", "toc.yml")))
  for (d in c("patterns", "outputs")) {
    dd <- file.path(root, sp, d)
    if (dir.exists(dd)) {
      f <- c(f, file.path(sp, d, sort(list.files(dd, "\\.ya?ml$", recursive = TRUE))))
    }
  }
  f[file.exists(file.path(root, f))]
}

# The planner read from a study folder's YAML spec: the sheets, the report
# list and setup, the ARD definition, the listing and figure sheets and the
# figure designs, as read_planner() and the ARD and listing readers give
# them.  What the YAML holds that the sheets cannot is the attribute
# `not_converted`.
.planner_from_yaml <- function(root) {
  back <- tflspec::tfl_sheets_from_yaml(root)
  p <- .planner_from_sheets(back$table, back$planner)
  old <- attr(p, "old_data_code")
  attr(p, "old_data_code") <- NULL
  p$ard <- stats::setNames(lapply(names(.ard_sheets()), function(s)
    .normalize_ard_sheet(back$ard[[s]] %||% data.frame(), s)), names(.ard_sheets()))
  p$lf <- .empty_lf()
  for (sh in intersect(.lf_sheet_names, names(back$listing))) {
    p$lf[[sh]] <- .normalize_lf_sheet(back$listing[[sh]], sh)
  }
  for (id in names(back$figures)) p <- set_fig_design(p, id, back$figures[[id]])
  p <- .migrate_step_code(p, old)
  p <- .renamed_outputs(p)
  attr(p, "not_converted") <- back$not_converted
  p
}
