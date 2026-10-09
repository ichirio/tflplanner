# The ARD programs: one per output, in programs/ard/.
#
#   programs/ard/ard_setup.R      what every ARD program starts with: cards,
#                                 the statistics tflplanner computes, stat_fmt,
#                                 and the step that puts an output's rows into
#                                 the study ARD
#   programs/ard/<output_id>.R    one output's analyses: its data, its analysis
#                                 sets, one cards / cardx call per analysis;
#                                 it replaces that output's rows of
#                                 output/ard/ard.rds and records the build in
#                                 output/ard/ard_status.csv
#   programs/ard/autoexec_ard.R   the official run of them all: a batch
#                                 folder with each program's log (logrx),
#                                 the study ARD and the code (programs/batch.R)
#
# Run on its own -- from the app or in RStudio -- an ARD program is a
# preview: it updates the study's working ARD, so tables can be made from
# it, and keeps no log.  The report programs are in programs/tfl/.
#
# Like the report programs, a generated ARD program carries a checksum, and
# follows the definition on every save: one edited by hand is written again
# too, its edited copy first put in programs/.edited/.

.ard_prog_name <- function(output_id) {
  paste0(gsub("[^A-Za-z0-9._-]", "_", output_id), ".R")
}

.ard_setup_file <- "ard_setup.R"
.ard_autoexec_file <- "autoexec_ard.R"

#' The ARD programs of a study
#'
#' `ard_setup_code()` is `programs/ard/ard_setup.R`, which every ARD program
#' sources: the study's setup (`programs/study_setup.R`, see
#' [study_setup_code()]), cards, the statistics tflplanner computes ([tflspec::tfl_ard_statistics()], the company standards' catalog),
#' the stat_fmt formats, and the option that makes `save_ard()` (the
#' study's `programs/study_helpers.R`, see [study_helpers_code()]) record
#' the study setup each ARD was built with.
#' `ard_program_code()` is one output's program, `programs/ard/<output_id>.R`.  `ard_autoexec_code()` is
#' `programs/ard/autoexec_ard.R`, which runs them from the study folder --
#' all, or the ones named (`Rscript programs/ard/autoexec_ard.R T-14-1-1`)
#' -- each in its own R process with its log in `logs/ard/`.
#'
#' @param spec An [tflspec::tfl_ard_spec()] (or the path of one).
#' @param output_id The output.
#' @param date The date stamped in the banner.
#' @param dir The study folder: the fingerprint recorded with the ARD reads
#'   the study's own analysis functions (its key `source`) from it.
#' @param codelists The reports' code lists (the table definition's
#'   `codelists` sheet, every row a report's), or `NULL`: each column the
#'   report's analyses read that its code lists list becomes a factor in
#'   their order before the analyses, so the ARD keeps the order and counts
#'   a value no record has (0) ([tflspec::tfl_ard_code()]).  They are part of
#'   the fingerprint.
#' @return The code, one element per line.
#' @export
ard_setup_code <- function(spec, date = Sys.Date()) {
  .with_study_code(.ard_setup_code(spec, date))
}

.ard_setup_code <- function(spec, date = Sys.Date()) {
  x <- if (is.character(spec)) .read_ard_spec(spec) else spec
  lay <- study_layout()
  c(.banner(
      paste("Program    :", file.path(lay[["programs_ard"]], .ard_setup_file)),
      "What every ARD program of the study starts with (it sources this).",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .source_study_setup(),
    "",
    # (tflspec's: library() of what the study's setup does not attach, a
    # blank line, the company's statistics and formats)
    .drop_leading_blank(.ard_spec_code(x, part = "setup")),
    "# the files each ARD is built with, recorded with it (tflspec's",
    "# save_ard(): tflplanner compares the study setup's)",
    sprintf("options(tflspec.ard_sources = c(setup = %s))",
            encodeString(.study_setup_path(), quote = "\"")),
    "")
}

#' @rdname ard_setup_code
#' @export
ard_program_code <- function(spec, output_id, date = Sys.Date(), dir = ".",
                             codelists = NULL) {
  .with_study_code(.ard_program_code(spec, output_id, date, dir, codelists))
}

.ard_program_code <- function(spec, output_id, date = Sys.Date(), dir = ".",
                              codelists = NULL) {
  x <- if (is.character(spec)) .read_ard_spec(spec) else spec
  lay <- study_layout()
  a <- x$analyses[x$analyses$output_id %in% output_id, , drop = FALSE]
  if (!nrow(a)) stop("No analyses for ", output_id, call. = FALSE)
  labels <- ifelse(is.na(a$label), a$analysis_id,
                   paste0(a$analysis_id, " (", a$label, ")"))
  c(.banner(
      paste("Program    :", file.path(lay[["programs_ard"]],
                                      .ard_prog_name(output_id))),
      paste0("Output     : ", output_id, " -> its rows of ",
             .study_value(x, "output", "output/ard/ard.rds")),
      strwrap(paste(labels, collapse = ", "), width = 74,
              initial = "Analyses   : ", prefix = strrep(" ", 13)),
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    sprintf("source(%s)", encodeString(file.path(lay[["programs_ard"]],
                                                 .ard_setup_file),
                                       quote = "\"")),
    "",
    # its rows into the study ARD with its definition's fingerprint
    # (tflspec's save_ard(); tflplanner compares it)
    .ard_spec_code(x, output_id = output_id, part = "body", dir = dir,
                   codelists = codelists))
}

#' @rdname ard_setup_code
#' @export
ard_autoexec_code <- function(spec, date = Sys.Date()) {
  lay <- study_layout()
  c(.autoexec_banner(file.path(lay[["programs_ard"]], .ard_autoexec_file),
                     "Official run of the ARD programs, one per output.",
                     "T-14-1-1         only these", date),
    .autoexec_body("ard"))
}

# missing / current / generated / edited, as a report program's
.ard_program_state <- function(code, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  # an earlier tflplanner wrote its autoexec without a checksum
  if (!length(chk) && any(grepl("^#  Generated  : tflplanner", have))) {
    return("generated")
  }
  h <- .body_hash(have)
  if (!length(chk) || !identical(chk[1L], h)) return("edited")
  if (identical(h, .body_hash(code))) "current" else "generated"
}

# write a generated program (one edited by hand copied to
# programs/.edited/ first); the file's row of what the save did
.put_program <- function(code, f, root) {
  state <- .ard_program_state(code, f)
  if (state == "current") return("unchanged")
  if (state == "edited") .back_up_edited(f, root)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  .write_program(code, f)
  if (state == "edited") "rewritten" else "written"
}

# the ARD programs, written with the study: the setup, one per output, the
# autoexec; a program of an output no longer defined goes (one edited by
# hand copied to programs/.edited/ first)
.save_ard_programs <- function(spec, root, codelists = NULL) {
  lay <- study_layout()
  dir <- file.path(root, lay[["programs_ard"]])
  ids <- unique(stats::na.omit(spec$analyses$output_id))
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, name) {
    f <- file.path(dir, name)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f, root))
  }
  put(ard_setup_code(spec), .ard_setup_file)
  for (id in ids) put(ard_program_code(spec, id, dir = root,
                                       codelists = codelists),
                      .ard_prog_name(id))
  put(ard_autoexec_code(spec), .ard_autoexec_file)
  keep <- c(.ard_setup_file, .ard_autoexec_file, vapply(ids, .ard_prog_name, ""))
  for (f in setdiff(list.files(dir, "\\.[Rr]$"), keep)) {
    p <- file.path(dir, f)
    have <- readLines(p, warn = FALSE, encoding = "UTF-8")
    chk <- sub("^#  Checksum   : *", "",
               grep("^#  Checksum   :", have, value = TRUE))
    # a program tflplanner wrote (with a checksum); a file of one's own,
    # never written by it, is not its to remove
    if (!length(chk)) next
    if (!identical(chk[1L], .body_hash(have))) .back_up_edited(p, root)
    file.remove(p)
    out[nrow(out) + 1L, ] <- list(p, "removed")
  }
  # the single make_ard.R of earlier versions
  old <- file.path(root, "programs", "make_ard.R")
  if (file.exists(old)) {
    file.remove(old)
    out[nrow(out) + 1L, ] <- list(old, "removed")
  }
  out
}

#' Preview one output's ARD
#'
#' Runs the output's saved ARD program (`programs/ard/<output_id>.R`) on its
#' own, from the study folder: it replaces the output's rows of the study's
#' working ARD, so its table can be made, and keeps no log.  A program that
#' fails leaves the ARD alone and records its error ([ard_status()]).  The
#' official run is [run_batch()].
#'
#' @param study An `rtfstudy` (saved: the program runs from disk).
#' @param output_id The output.
#' @param timeout Seconds to allow.
#' @return A list: `ok`, `rows`, `error`, `output` (what the program
#'   printed).
#' @export
update_study_ard <- function(study, output_id, timeout = 600) {
  prog <- file.path(study_layout()[["programs_ard"]], .ard_prog_name(output_id))
  if (!file.exists(file.path(study$path, prog))) {
    stop("No ", prog, " yet: save the study first.", call. = FALSE)
  }
  px <- processx::run(file.path(R.home("bin"), "Rscript"), prog,
                      wd = study$path, error_on_status = FALSE,
                      timeout = timeout, stderr_to_stdout = TRUE)
  out <- strsplit(px$stdout, "\r?\n")[[1L]]
  ok <- identical(px$status, 0L)
  if (!ok) {
    err <- grep("^Error", out, value = TRUE)
    note <- if (length(err)) err[1L] else "failed"
    st <- .read_ard_status(study)
    st <- st[st$output_id != output_id, , drop = FALSE]
    st[nrow(st) + 1L, ] <- list(output_id, "", format(Sys.time(),
      "%Y-%m-%d %H:%M:%S"), NA_integer_, note, "")
    .write_ard_status(study, st)
  }
  st <- ard_status(study)
  st <- st[st$output_id == output_id, , drop = FALSE]
  list(ok = ok, rows = if (nrow(st)) st$rows[1L] else NA_integer_,
       error = if (!ok) paste(utils::tail(out, 15L), collapse = "\n"),
       output = out)
}
