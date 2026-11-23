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
  # (before anything is written: a program name that is a generated file's,
  # or one another report's, stops the save)
  bad <- .program_name_problems(p)
  if (length(bad)) {
    stop(paste(vapply(bad, .program_name_message, ""), collapse = "
"),
         call. = FALSE)
  }
  # A save that stops part way (a report whose program cannot be written
  # ...) writes nothing: the files it wrote before it stopped are put back
  # as they were, and the record of what is on disk is not touched -- the
  # next save does not take its own half-written files for changes made
  # outside the app (#751)
  snap <- .save_snapshot(root)
  done <- FALSE
  on.exit(if (!done) .save_rollback(root, snap), add = TRUE)
  meta <- study$meta[.study_fields]
  old_meta <- tryCatch(.read_meta(root), error = function(e) list())
  meta$created <- old_meta$created %||% format(Sys.Date())
  # the YAML spec: each file written only when its text changed
  # (the reports' files as they were: those the save writes, read just
  # before it writes them)
  before <- NULL
  files <- .save_yaml_spec(p, meta, root, own = as_recorded,
                           before_write = if (isTRUE(fill_widths)) function(q) {
                             before <<- .widths_before(root, .spec_to_write(q, root))
                           })
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
  files <- rbind(files, .save_study_setup(study$meta, root, .study_rounding_source(p)),
                 .save_study_helpers(root),
                 .save_batch_programs(p, root))
  study$meta <- .read_meta(root)[.study_fields]
  .write_state(study, home)
  done <- TRUE
  .set_config("last_study", study$meta$study_id, home)
  study$files <- files
  # why no ARD program was written, or NULL (the app says it)
  study$ard_problem <- ard_problem
  invisible(study)
}

# The files a save writes: those at the study's top (study.yml ...), and
# under spec/ and programs/ (paths relative to the study)
.save_files_now <- function(root) {
  top <- list.files(root, all.files = TRUE, no.. = TRUE)
  top <- top[!dir.exists(file.path(root, top))]
  sub <- unlist(lapply(c("spec", "programs"), function(d) {
    if (!dir.exists(file.path(root, d))) return(character())
    file.path(d, list.files(file.path(root, d), recursive = TRUE, all.files = TRUE))
  }))
  c(top, sub)
}
# them as they are now (their bytes)
.save_snapshot <- function(root) {
  rel <- .save_files_now(root)
  stats::setNames(lapply(rel, function(r) {
    f <- file.path(root, r)
    readBin(f, "raw", file.size(f))
  }), rel)
}
# them put back as the snapshot had them: a file the save made goes
.save_rollback <- function(root, snap) {
  unlink(file.path(root, setdiff(.save_files_now(root), names(snap))))
  for (r in names(snap)) {
    f <- file.path(root, r)
    if (file.exists(f) && identical(readBin(f, "raw", file.size(f)), snap[[r]])) next
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeBin(snap[[r]], f)
  }
  invisible()
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
  old <- file.path(root, "programs", .autoexec_report_file)
  if (file.exists(old)) file.remove(old)
  invisible()
}

# ---- program names no report or ARD program may have ----------------------

# The sentences, by kind (the English is the key of inst/i18n/strings/)
.program_name_templates <- c(
  reserved = "Report %s: the program %s is the name of a file tflplanner writes itself. Give the report another `program` on the report sheet.",
  duplicate = "Reports %s and %s share the program %s (names that differ only in case are one file). Give each its own `program` on the report sheet.",
  ard_reserved = "The ARD program of the output ID %s would be %s, the name of a file tflplanner writes itself. Use another output ID.",
  ard_duplicate = "The output IDs %s and %s make the same ARD program %s (names that differ only in case are one file). Use output IDs whose ARD programs differ.")

# What stops a save, from the report list alone: a list of problems, each
# list(id, kind, args, sheet, field).  Names are compared in lower case
# (Windows and macOS keep one file for T1.R and t1.R).  A report's program
# is in programs/tfl/ and an ARD program in programs/ard/: each is compared
# with its own folder's, and both with every name tflplanner writes (the
# programs/ ones too: a report program of that name is moved there, and the
# setup with it, by .move_old_programs()).
.program_name_problems <- function(p) {
  ids <- p$outputs$output_id
  ids <- ids[!is.na(ids) & nzchar(ids)]
  out <- list()
  add <- function(id, kind, args, sheet, field) {
    out[[length(out) + 1L]] <<- list(id = id, kind = kind, args = as.character(args),
                                     sheet = sheet, field = field)
  }
  reserved <- tolower(.generated_program_files())
  one <- function(ids, names, kind, sheet, field) {
    low <- tolower(names)
    for (i in seq_along(ids)) {
      if (is.na(names[i])) next
      if (low[i] %in% reserved) {
        add(ids[i], kind[1L], c(ids[i], names[i]), sheet, field)
      }
      j <- match(low[i], low[seq_len(i - 1L)])
      if (!is.na(j)) {
        add(ids[i], kind[2L], c(ids[j], ids[i], names[i]), sheet, field)
      }
    }
  }
  prog <- vapply(ids, function(id)
    tryCatch(report_info(p, id)$program, error = function(e) NA_character_), "")
  one(ids, unname(prog), c("reserved", "duplicate"), "report", "program")
  ids <- unique(ids)
  one(ids, .ard_prog_name(ids), c("ard_reserved", "ard_duplicate"),
      "_tflplanner", "output_id")
  out
}

.program_name_message <- function(x) {
  do.call(sprintf, c(list(.program_name_templates[[x$kind]]), as.list(x$args)))
}
