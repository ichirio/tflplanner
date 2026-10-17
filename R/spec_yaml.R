# The study definition as tflspec's YAML spec (2026-10-15): study.yml and
# spec/ (data.yml, defaults.yml, codelists.yml, toc.yml, patterns/,
# outputs/<id>.yml), one file a report.  The planner holds the spec itself
# and the sheets every screen edits are a view of it (spec_model.R, #407);
# the view's edits go back through tflspec's tfl_yaml_from_sheets().  A study whose
# spec/ still holds the workbooks is of the sheets' format: it opens as
# before until it is converted.

# Whether a study folder has a YAML spec: spec/outputs/, or spec/toc.yml
# (a study saved with no report yet has no outputs folder)
.has_yaml_spec <- function(root) {
  sp <- file.path(root, study_layout()[["spec"]])
  dir.exists(file.path(sp, "outputs")) || file.exists(file.path(sp, "toc.yml"))
}

# The format of a study folder's definition: "yaml" (.has_yaml_spec()),
# "sheets" (the workbooks, the ARD's JSON) or "none" (nothing yet)
.study_format <- function(root) {
  sp <- file.path(root, study_layout()[["spec"]])
  if (.has_yaml_spec(root)) return("yaml")
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

# The planner read from a study folder's YAML spec: its view of the spec
# (.planner_from_spec()), the spec in `$spec` (.spec_load(); in step with
# the view once .spec_rebase() is given the study's fields).  What the YAML
# holds that the sheets cannot is the attribute `not_converted`.
.planner_from_yaml <- function(root) {
  sp <- .spec_load(root)
  if (is.null(sp)) {
    # (a spec with no reports' folder: what tflspec reads of it)
    return(.planner_from_spec(tflspec::tfl_read_study_spec(root), tflspec::tfl_read_outputs(root)))
  }
  p <- .planner_from_spec(sp$study, sp$outputs)
  p$spec <- sp
  p
}

# What tflspec's tfl_yaml_from_sheets() writes the YAML spec from: the
# planner's sheets as one definition (with the study keys), the report list
# and setup (the _tflplanner sheet's rows), the ARD definition, the listing
# and figure sheets, the figure designs, and the study's fields
.planner_sheets <- function(p, meta) {
  o <- p$outputs[names(.empty_outputs())]
  o$setup_code <- rep(NA_character_, nrow(o))
  planner <- rbind(
    data.frame(output_id = NA_character_, description = "(every report)",
               setup_code = p$setup, process_code = NA_character_,
               section = NA_character_, population = NA_character_,
               datasets = NA_character_, batches = NA_character_,
               stringsAsFactors = FALSE),
    o)
  study <- meta[intersect(.study_fields, names(meta))]
  study <- study[!vapply(study, function(v) is.null(v) || !length(v) || is.na(v[1L]), NA)]
  list(table = unclass(.spec_object(p, c(table_sheets(), report_sheets()),
                                    unique(unlist(.study_keys)))),
       planner = planner, ard = p$ard, listing = p$lf,
       figures = p$fig_designs %||% list(), study = study)
}

# The study's definition written as the YAML spec into its folder: the
# view's edits taken into the spec (.spec_sync(): only the files they
# change come from the bridge, the others keep the spec's text), study.yml
# with the study's fields, when it was created and by which tflplanner; each
# file written only when its text changed (a save that changes one report
# writes its file), a report's file gone from the definition removed when
# it is the one the last save wrote (`own(rel)`).  The files, as
# save_study() lists them: file, status; the planner, in step with the
# spec now, is the attribute `planner`.
.save_yaml_spec <- function(p, meta, root, own = function(rel) FALSE) {
  p <- .spec_sync(p, meta)
  attr(p, "changed") <- NULL
  p$spec$study$dir <- normalizePath(root, "/", mustWork = FALSE)
  files <- .spec_write(p, root, own)
  attr(files, "planner") <- p
  files
}

# A study as its YAML spec would read back: its planner written by tflspec
# (a folder of its own) and read again -- the same definition in the way
# the files say it (rows in the TOC's order, the content's own form), to
# compare the files with (outside edits, imports)
.yaml_normal <- function(s) {
  tmp <- tempfile("tflplanner-normal")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  p <- tryCatch({
    tflspec::tfl_yaml_from_sheets(.planner_sheets(s$planner, s$meta), tmp, overwrite = TRUE)
    .study_spec_keys(.planner_from_yaml(tmp))
  }, error = function(e) NULL)
  if (!is.null(p)) {
    attr(p, "not_converted") <- NULL
    # (the study's own spec stays its own)
    p$spec <- s$planner$spec
    s$planner <- p
  }
  s
}
