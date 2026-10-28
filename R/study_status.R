# study_status() and run_study(): what each report has produced.
# Split from R/study.R by section; no code changed.

#' What each report of a study has produced
#'
#' One row per report: its program (`missing`; `current`, `todo` or
#' `generated` -- untouched since tflplanner wrote it, `generated` meaning
#' the next save rewrites it; or `edited` by hand), whether its ARD and RTF exist
#' and when they were made, and a `status`:
#'
#' * `no program` -- save the study to write it
#' * `unsaved` -- the definition changed since the program was written;
#'   save the study
#' * `todo` -- the program's data part is still to be written
#' * `not run` -- no RTF yet
#' * `error` -- the last run failed (see its log)
#' * `outdated` -- the report's program, or a file it sources (the figure
#'   setup, say), changed after the RTF was made; or the report was made
#'   with another `programs/study_setup.R` than the one there now (its
#'   program records it in `output/tfl/report_status.csv`); or -- a figure
#'   printing the numbers of an ARD (its own, or a table's) -- that ARD's
#'   definition is not the one it was made from, or that ARD is not made
#'   from its definition now.  The program holds the
#'   report's whole definition, so a change to the definition reaches the
#'   reports it is about and no others.
#' * `ok`
#'
#' `why` says what made a report `outdated`: `program` (its program or a
#' file it sources is newer), `setup` (the study setup changed), or
#' `ard:<id>` (the ARD of `<id>` changed, or is to be made again).
#'
#' @param study An `rtfstudy`.
#' @return A data frame.
#' @export
study_status <- function(study) {
  p <- study$planner
  root <- study$path
  .use_study(root)
  lay <- study_layout()
  setup <- .report_setup_recorded(root, p$outputs$output_id)
  now <- .study_setup_hash(root)
  # the figures printing an ARD's numbers: to be made again because of it
  ard_why <- .fig_ard_why(study, p$outputs$output_id)
  rows <- lapply(p$outputs$output_id, function(id) {
    info <- report_info(p, id)
    prog <- file.path(root, lay[["programs_tfl"]], info$program)
    rtf <- file.path(root, info$file)
    log <- file.path(root, lay[["logs_preview"]], sub("\\.[Rr]$", ".log",
                                             info$program))
    pstate <- .program_state(p, id, prog)
    t_rtf <- .mtime(rtf)
    t_log <- .mtime(log)
    failed <- !is.na(t_log) && (is.na(t_rtf) || t_log > t_rtf) &&
      any(grepl("^Error|Execution halted",
                readLines(log, warn = FALSE, encoding = "UTF-8")))
    k <- match(id, p$outputs$output_id)
    why <- c(
      if (isTRUE(.program_time(prog, root) > t_rtf)) "program",
      if (.setup_changed(setup[k], root, now)) "setup",
      if (nzchar(ard_why[[k]])) ard_why[[k]])
    status <- if (pstate == "missing") "no program" else
      if (pstate == "generated") "unsaved" else
      if (pstate == "todo") "todo" else
        if (failed) "error" else
          if (is.na(t_rtf)) "not run" else
            if (length(why)) "outdated" else "ok"
    if (status != "outdated") why <- character()
    fmt <- function(t) if (is.na(t)) NA_character_ else
      format(t, "%Y-%m-%d %H:%M")
    data.frame(output_id = id, type = info$type, program = info$program,
               program_state = pstate,
               rtf = fmt(t_rtf), status = status,
               why = paste(why, collapse = " "),
               log = if (file.exists(log)) log else NA_character_,
               rtf_path = rtf, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    output_id = character(), type = character(), program = character(),
    program_state = character(), rtf = character(),
    status = character(), why = character(), log = character(),
    rtf_path = character())),
    rows))
  rownames(out) <- NULL
  out
}

# When a report's program last changed: the program itself, or a file of
# the study it sources (programs/tfl/fig_setup.R, a setup of the user's).
# The definition workbooks are not in it: the program is written from them
# and holds all of the report's own definition, so saving a change to one
# report rewrites that report's program only.
.program_time <- function(prog, root) {
  t <- .mtime(prog)
  if (is.na(t)) return(t)
  txt <- readLines(prog, warn = FALSE, encoding = "UTF-8")
  src <- regmatches(txt, regexpr("^\\s*source\\(\"[^\"]+\"", txt))
  src <- unique(sub("^\\s*source\\(\"", "", sub("\"$", "", src)))
  if (!length(src)) return(t)
  ts <- do.call(c, lapply(file.path(root, src), .mtime))
  suppressWarnings(max(c(t, ts), na.rm = TRUE))
}

#' Preview a study's reports
#'
#' Runs report programs on their own (a preview), from the study folder --
#' every report or the ones named -- each in its own `Rscript` process.
#' They write the working RTFs (output/tfl/); what each printed is kept in
#' `logs/preview/` until the next preview, and no log of record is made.
#' A table's rows of the study ARD, and those a figure prints numbers of,
#' are made first when they are not made from the definition as it is now
#' ([update_study_ard()]).  The official run is [run_batch()].
#'
#' @param study An `rtfstudy` (saved: the programs run from disk).
#' @param output_id Reports to run; `NULL` runs all.
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return With `wait = TRUE`, [study_status()] after the run; otherwise
#'   the process.
#' @examples
#' # a home in the temporary folder: used in this R session only, nothing
#' # is written to your settings
#' old <- options(tflplanner.home = NULL)
#' setup_tflplanner(home = tempfile("tflplanner-home"))
#' \donttest{
#' # (needs the sample data and rtfreporter; each report runs in its own R process)
#' s <- create_sample_study(run = FALSE)
#' id <- s$planner$outputs$output_id[1]  # one report
#' run_study(s, id)[1, c("output_id", "status")]
#' study_status(s)[1, c("output_id", "status")]
#' }
#' options(old)
#' @export
run_study <- function(study, output_id = NULL, wait = TRUE) {
  p <- study$planner
  ids <- output_id %||% p$outputs$output_id
  # a figure printing an ARD's numbers: that ARD first, when it is not made
  # from its definition now (as the official run does, #293); a table's own
  # likewise -- the builder's preview makes its ARD apart and leaves the
  # study ARD as it is (#428), so a table run reads its rows made here
  need <- unique(stats::na.omit(vapply(ids, function(id)
    tryCatch(.fig_ard_need(p, id), error = function(e) NA_character_), "")))
  need <- unique(c(need, ids[vapply(ids, function(id)
    identical(report_info(p, id)$type, "table") && .ard_from_definition(p, id), NA)]))
  if (length(need)) {
    st <- tryCatch(ard_status(study), error = function(e) NULL)
    for (tb in need) {
      if (!identical(st$state[match(tb, st$output_id)], "built")) update_study_ard(study, tb)
    }
  }
  progs <- vapply(ids, function(id) report_info(p, id)$program, "")
  lay <- study_layout()
  # a small runner: each program in its own Rscript, what it prints kept
  # in logs/preview/ (overwritten each time)
  f <- tempfile("preview", fileext = ".R")
  writeLines(c(
    paste0("progs <- c(", paste(encodeString(progs, quote = "\""),
                                collapse = ", "), ")"),
    paste0("dir.create(", encodeString(lay[["logs_preview"]], quote = "\""),
           ", recursive = TRUE, showWarnings = FALSE)"),
    "bad <- 0L",
    "for (p in progs) {",
    paste0("  log <- file.path(", encodeString(lay[["logs_preview"]], quote = "\""),
           ", sub(\"[.][Rr]$\", \".log\", p))"),
    paste0("  rc <- system2(file.path(R.home(\"bin\"), \"Rscript\"), shQuote(file.path(",
           encodeString(lay[["programs_tfl"]], quote = "\""), ", p)),"),
    "                stdout = log, stderr = log)",
    "  cat(if (identical(rc, 0L)) \"OK   \" else \"ERROR\", p, \"\\n\")",
    "  if (!identical(rc, 0L)) bad <- bad + 1L",
    "}",
    "quit(status = if (bad) 1L else 0L)"), f)
  px <- processx::process$new(file.path(R.home("bin"), "Rscript"), f,
                              wd = study$path, stdout = NULL,
                              stderr = NULL, cleanup = TRUE)
  if (!wait) return(px)
  px$wait()
  study_status(study)
}
