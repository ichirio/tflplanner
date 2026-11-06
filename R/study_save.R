# The print method, save_study() and moving the programs of an old study.
# Split from R/study.R by section; no code changed.

#' @export
print.rtfstudy <- function(x, ...) {
  cat("<rtfstudy> ", x$meta$study_id, sep = "")
  if (!is.na(x$meta$title)) cat(" --", x$meta$title)
  cat("\n  ", x$path, "\n  ", sep = "")
  st <- x$spec$status
  if (!is.null(st) && st %in% c("adopted", "invalid")) {
    cat(if (st == "adopted") "definition files changed outside tflplanner: loaded" else
          "definition files changed outside tflplanner: they do not read (reload_from_spec() / write_spec())",
        "\n  ", sep = "")
  }
  print(x$planner)
  invisible(x)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Save a study
#'
#' Saves the study's state in tflplanner's home (the copy
#' [open_study()] reads; the one before goes to its history), then writes
#' the study folder from it: the definition workbooks in `spec/` (with
#' `output_path` and `program_dir` set to the study's own folders), the
#' report programs, `autoexec_report.R`, `programs/study_setup.R` (made
#' when missing; otherwise its tflplanner part only, see
#' [study_setup_code()]), and `study.yml`.  The programs
#' are the definition's: each is written from it whenever it changes.  One
#' edited by hand since (its banner's checksum no longer matches) is
#' written again too, its edited copy first put in `programs/.edited/`
#' (named after its place and the time) -- what it changed belongs in the
#' definition (the data code, a user-code report, a custom analysis,
#' where / derive).
#'
#' @param study An `rtfstudy`.
#' @param home tflplanner's home.
#' @param base The study as it was opened (or last saved) by whoever saves
#'   now.  Given it, the save merges: a part (the study fields, the report
#'   list, each sheet, each sheet of the ARD definition ...) this person did
#'   not change keeps what is saved now -- someone else may have changed
#'   it -- and a part both changed differently is a conflict that stops the
#'   save (class `tflplanner_conflict`).
#'
#' The definition files are the study's source (#274): a save never writes
#' over one changed outside tflplanner since it last wrote or read them.
#' Such a save stops (class `tflplanner_spec_changed`, its `files` the
#' files changed): open the study again, or [reload_from_spec()], to take
#' the files in; or write them back from the last save ([write_spec()]).
#'
#' @param spec `"check"` (the default) stops when a definition file was
#'   changed outside tflplanner; `"overwrite"` writes over it, after copying
#'   it to `spec/.rejected/` (what [write_spec()] does).
#' @param fill_widths `TRUE` (the app's save): a report new or changed in
#'   this save that says no column widths gets the default ones
#'   ([tflspec::tfl_fill_widths()]), and a table whose columns changed has
#'   its old widths replaced.
#' @return The study, invisibly, with `files`: what was written (status
#'   `written`, `rewritten` for a program edited by hand, `unchanged`, ...).
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (a few seconds: it writes the study's spec workbooks)
#' s <- create_sample_study(run = FALSE)
#' s <- save_study(s)  # writes the definition files and the programs
#' head(s$files)
#' }
#' options(old)
#' @export
save_study <- function(study, home = tflplanner_home(), base = NULL,
                       spec = c("check", "overwrite"), fill_widths = FALSE) {
  spec <- match.arg(spec)
  if (!is.null(base)) study <- .merge_saved(study, base, home)
  p <- .meta_tokens(.study_spec_keys(study$planner), study$meta)
  study$planner <- p
  root <- study$path
  # its programs as its setup has it (its folders' variables, its packages)
  .use_study(root)
  lay <- study_layout()
  # the state now on disk (another session may have saved since) and its
  # record of the definition files
  st <- .read_state(study$meta$study_id, home)
  rec <- st$files
  if (!is.null(rec)) {
    diff <- .spec_diff(rec, .spec_fingerprints(root))
    if (any(diff$status != "same")) {
      if (spec == "check") stop(.spec_changed_condition(diff))
      .back_up_rejected(root, diff$file[diff$status %in% c("changed", "added")])
      # a file added outside tflplanner is not the last save's: it goes
      # (its copy is in spec/.rejected/)
      unlink(file.path(root, diff$file[diff$status == "added"]))
      rec <- rec[names(rec) %in% diff$file[diff$status == "same"]]
    }
  }
  # a file is what the last save wrote when its fingerprint is the one
  # recorded
  as_recorded <- function(rel) {
    f <- file.path(root, rel)
    !is.null(rec[[rel]]) && file.exists(f) &&
      identical(unname(tools::md5sum(f)), as.character(rec[[rel]]$md5))
  }
  progs <- vapply(p$outputs$output_id, function(id)
    report_info(p, id)$program, "")
  dup <- unique(progs[duplicated(progs)])
  if (length(dup)) {
    stop("Two reports share the program ", paste(dup, collapse = ", "),
         "; give each its own `program` on the report sheet.", call. = FALSE)
  }
  meta <- study$meta[.study_fields]
  old_meta <- tryCatch(.read_meta(root), error = function(e) list())
  meta$created <- old_meta$created %||% format(Sys.Date())
  # the YAML spec: each file written only when its text changed
  before <- if (isTRUE(fill_widths)) .widths_before(root)
  files <- .save_yaml_spec(p, meta, root, own = as_recorded)
  # (the planner and its spec in step: what was saved)
  p <- attr(files, "planner")
  attr(files, "planner") <- NULL
  # the reports saved here say their column widths (R/save_widths.R)
  # (the planner as saved before them: what the app compares its own with)
  if (isTRUE(fill_widths)) {
    study$planner_saved <- p
    p <- .save_fill_widths(p, root, files, before)
  }
  study$planner <- p
  for (d in lay) dir.create(file.path(root, d), recursive = TRUE,
                            showWarnings = FALSE)
  .move_old_programs(root, progs)
  for (i in seq_along(progs)) {
    f <- file.path(root, lay[["programs_tfl"]], progs[[i]])
    id <- p$outputs$output_id[i]
    state <- .program_state(p, id, f)
    if (state %in% c("current", "todo")) {
      files[nrow(files) + 1L, ] <- list(f, "unchanged")
      next
    }
    if (state == "edited") .back_up_edited(f, root)
    .write_program(.program_code_last(p, id), f)
    files[nrow(files) + 1L, ] <- list(f, if (state == "edited") "rewritten" else "written")
  }
  ard_files <- .save_ard(p, root, json = FALSE)
  ard_problem <- attr(ard_files, "problem")
  if (!is.null(ard_problem)) {
    message("The ARD definition does not hold, so no ARD program was written ",
            "(the rest is saved):\n", ard_problem)
  }
  files <- rbind(files, ard_files)
  files <- rbind(files, .save_study_setup(study$meta, root),
                 .save_study_helpers(root),
                 .save_batch_programs(p, root))
  study$meta <- .read_meta(root)[.study_fields]
  .write_state(study, home)
  .set_config("last_study", study$meta$study_id, home)
  study$files <- files
  # why no ARD program was written, or NULL (the app says it)
  study$ard_problem <- ard_problem
  invisible(study)
}

# A study of an earlier version kept its report programs in programs/ and
# the ARD in one programs/make_ard.R: the report programs move to
# programs/tfl/ (edits and all), the generated autoexec goes.
.move_old_programs <- function(root, progs) {
  lay <- study_layout()
  for (p in progs) {
    old <- file.path(root, "programs", p)
    new <- file.path(root, lay[["programs_tfl"]], p)
    if (file.exists(old) && !file.exists(new)) file.rename(old, new)
  }
  old <- file.path(root, "programs", "autoexec_report.R")
  if (file.exists(old)) file.remove(old)
  invisible()
}
