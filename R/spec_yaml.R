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
  for (id in names(back$figures)) {
    # (a figure with nothing of the designer's -- no template, no layers,
    # no steps -- has no design)
    d <- .fig_norm(back$figures[[id]])
    if (length(Filter(length, d))) p <- set_fig_design(p, id, d)
  }
  p <- .migrate_step_code(p, old)
  p <- .renamed_outputs(p)
  attr(p, "not_converted") <- back$not_converted
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
# files tflspec writes (in a folder of their own first), study.yml with the
# study's fields, when it was created and by which tflplanner; each file
# written only when its text changed (a save that changes one report writes
# its file, and study.yml), a report's file gone from the definition
# removed when it is the one the last save wrote (`own(rel)`).  The files,
# as save_study() lists them: file, status.
.save_yaml_spec <- function(p, meta, root, own = function(rel) FALSE) {
  tmp <- tempfile("tflplanner-yaml")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  tflspec::tfl_yaml_from_sheets(.planner_sheets(p, meta), tmp, overwrite = TRUE)
  sy <- file.path(tmp, .study_file)
  more <- list(created = meta$created %||% format(Sys.Date()),
               updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               tflplanner = as.character(utils::packageVersion("tflplanner")))
  lines <- readLines(sy, encoding = "UTF-8")
  # (no blank line between tflspec's part and the study's)
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  writeLines(c(lines, sub("\n$", "", yaml::as.yaml(more))), sy, useBytes = TRUE)
  rel <- list.files(tmp, recursive = TRUE)
  rel <- setdiff(rel, file.path(study_layout()[["spec"]], "_not_converted.yml"))
  files <- data.frame(file = character(), status = character(), stringsAsFactors = FALSE)
  for (r in rel) {
    to <- file.path(root, r)
    new <- readLines(file.path(tmp, r), encoding = "UTF-8")
    # (study.yml whose time alone would change: as it is)
    drop <- if (identical(r, .study_file)) function(x) x[!startsWith(x, "updated:")] else identity
    same <- file.exists(to) && identical(drop(readLines(to, encoding = "UTF-8")), drop(new))
    if (!same) {
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(tmp, r), to, overwrite = TRUE)
    }
    files[nrow(files) + 1L, ] <- list(to, if (same) "unchanged" else "written")
  }
  # a report's (or a pattern's) file no longer in the definition
  for (r in setdiff(.spec_files_yaml(root), c(rel, .study_file))) {
    if (isTRUE(own(r))) {
      unlink(file.path(root, r))
      files[nrow(files) + 1L, ] <- list(file.path(root, r), "removed")
    }
  }
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
    s$planner <- p
  }
  s
}
