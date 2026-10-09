# Official runs and previews.
#
# A program run on its own -- from the app, or opened in RStudio -- is a
# preview: it writes the study's working output (output/ard/ard.rds,
# output/tfl/) so the next step can go on, and keeps no log.
#
# An official run goes through an autoexec program, which makes a batch
# folder stamped with the date and time (runs/20260927_143000_all/) and
# keeps in it everything of the run: each program's log (logrx), what it
# made (the study ARD, the RTFs), and -- unless asked not to -- the programs
# and definition workbooks it ran.
#
#   programs/batch.R               the runner every autoexec sources
#   programs/ard/autoexec_ard.R    official run of the ARD programs
#   programs/tfl/autoexec_report.R official run of the report programs
#   programs/autoexec_all.R        both, the ARD first, into one batch folder

.batch_file <- "batch.R"
.fig_setup_file <- "fig_setup.R"
.report_setup_file <- "report_setup.R"
.autoexec_all_file <- "autoexec_all.R"

#' The official-run programs of a study
#'
#' `batch_code()` is `programs/batch.R`: the study's programs (the ARD
#' programs, the report programs, what each report makes) and the runner
#' that runs them into a batch folder.  `autoexec_all_code()` is
#' `programs/autoexec_all.R` (the ARD, then the reports); see also
#' [ard_autoexec_code()] and [autoexec_code()].
#'
#' @param x A `tflplanner`.
#' @param date The date stamped in the banner.
#' @return The program, one element per line.
#' @export
batch_code <- function(x, date = Sys.Date()) {
  lay <- study_layout()
  spec <- x$ard %||% .empty_ard_spec()
  ids <- unique(stats::na.omit(spec$analyses$output_id))
  ard_progs <- file.path(lay[["programs_ard"]],
                         vapply(ids, .ard_prog_name, ""))
  out_ids <- x$outputs$output_id
  infos <- lapply(out_ids, function(id) report_info(x, id))
  tfl_progs <- file.path(lay[["programs_tfl"]],
                         vapply(infos, `[[`, "", "program"))
  made <- lapply(seq_along(out_ids), function(i) c(
    infos[[i]]$file,
    if (identical(infos[[i]]$type, "table"))
      file.path(lay[["ard"]], paste0(out_ids[i], ".rds"))))
  ard_out <- .study_value(spec, "output", "output/ard/ard.rds")
  wb <- function(f) file.path(lay[["spec"]], f)
  support <- list(
    ard = c(file.path(lay[["programs_ard"]], c(.ard_setup_file,
                                               .ard_autoexec_file)),
            wb(.ard_file)),
    tfl = c(file.path(lay[["programs_tfl"]], c("autoexec_report.R",
                                               .fig_setup_file,
                                               .report_setup_file)),
            wb(c(.table_file, .report_file, .lf_file, .ard_file))),
    all = c(file.path("programs", c(.batch_file, .autoexec_all_file)),
            .study_setup_path(), .study_file))
  vec <- function(v, names = NULL, indent = "  ") {
    if (!length(v)) return("character()")
    q <- encodeString(v, quote = "\"")
    if (!is.null(names)) q <- paste(encodeString(names, quote = "`"), "=", q)
    paste0("c(\n", paste0(indent, q, collapse = ",\n"), ")")
  }
  # a list of vectors, its elements indented as the list's
  vlist <- function(...) {
    v <- list(...)
    paste0("list(\n", paste0("  ", names(v), " = ",
                             vapply(v, vec, "", indent = "    "), collapse = ",\n"),
           "\n)")
  }
  runner <- readLines(system.file("batch", "runner.R", package = "tflplanner"),
                      warn = FALSE, encoding = "UTF-8")
  c(.banner(
      paste("Program    :", file.path("programs", .batch_file)),
      "The runner of the study's official runs: programs/ard/autoexec_ard.R,",
      "programs/tfl/autoexec_report.R and programs/autoexec_all.R source it.",
      "Each run makes a batch folder runs/<date>_<time>_<what>/ with the",
      "logs, what the run made, and the code it ran.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "# ---- the study's programs, in the order they run ---------------------------",
    paste0(".batch_root <- ", encodeString(lay[["runs"]], quote = "\"")),
    paste0(".batch_ard  <- ", encodeString(ard_out, quote = "\"")),
    paste0(".batch_programs <- ", vlist(ard = ard_progs, tfl = tfl_progs)),
    "# the output each ARD program makes",
    paste0(".batch_ard_outputs <- ", vec(ids, basename(ard_progs))),
    "# the output each report program makes",
    paste0(".batch_report_ids <- ", vec(out_ids, tfl_progs)),
    "# the named batches (the report list's `batches`): name -> outputs",
    paste0(".batch_sets <- ", if (length(sets <- batch_sets(x)))
      paste0("list(\n", paste0("  ", encodeString(names(sets), quote = "`"), " = ",
                               vapply(sets, function(v) paste0("c(", paste(
                                 encodeString(v, quote = "\""), collapse = ", "), ")"), ""),
                               collapse = ",\n"), ")") else "list()"),
    "# what each report program makes",
    paste0(".batch_outputs <- list(\n",
           paste0("  ", encodeString(tfl_progs, quote = "`"), " = ",
                  vapply(made, function(m) paste0("c(", paste(
                    encodeString(m, quote = "\""), collapse = ", "), ")"), ""),
                  collapse = ",\n"), ")"),
    "# kept with the code of a run",
    paste0(".batch_support <- ", vlist(ard = support$ard, tfl = support$tfl,
                                       all = support$all)),
    "",
    runner,
    "")
}

# programs/tfl/fig_setup.R: the study's setup, the packages the figures'
# code uses (the designed figures': ggsurvfit for a KM ...), the figure
# style of the company standards (tflspec), and report_content()
.fig_setup_code <- function(p = NULL) {
  code <- .with_study_code(tflspec::tfl_fig_setup_code(.std_fig_style()))
  # after its banner (the comment lines it starts with)
  at <- match(FALSE, grepl("^#", code), nomatch = length(code) + 1L) - 1L
  rest <- utils::tail(code, length(code) - at)
  rest <- rest[cumsum(nzchar(rest)) > 0L]
  c(utils::head(code, at),
    if (at) "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .source_study_setup(),
    "",
    "# the packages the figures' code uses",
    paste0("library(", .fig_setup_libs(p), ")"),
    "",
    rest,
    "",
    .report_content_fun)
}

# ggplot2, patchwork, dplyr, and what the study's designed figures use
.fig_setup_libs <- function(p = NULL) {
  base <- c("ggplot2", "patchwork", "dplyr")
  ids <- names(p$fig_designs %||% list())
  libs <- unlist(lapply(ids, function(id) tryCatch(
    attr(tflspec::tfl_fig_design_code(fig_design(p, id), id, setup = TRUE,
                                      save = FALSE), "libs"),
    error = function(e) NULL)))
  c(base, sort(setdiff(unique(libs), base)))
}

.autoexec_banner <- function(file, what, usage, date) {
  .banner(
    paste("Program    :", file),
    what,
    paste("  Rscript", file, "                 every program"),
    paste("  Rscript", file, usage),
    paste("  Rscript", file, "--no-code        the batch folder without the code"),
    "Run it from the study folder.  It makes a batch folder",
    "runs/<date>_<time>_<what>/: each program's log (logrx), what the run",
    "made, and the programs and definition workbooks it ran.",
    paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
           ", ", format(date, "%Y-%m-%d")))
}

.autoexec_body <- function(parts) {
  c("",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run it from the study folder (the one with study.yml).\")",
    "}",
    paste0("source(", encodeString(file.path("programs", .batch_file),
                                   quote = "\""), ")"),
    sprintf("run_batch(%s, commandArgs(trailingOnly = TRUE))",
            if (length(parts) == 1L) encodeString(parts, quote = "\"") else
              paste0("c(", paste(encodeString(parts, quote = "\""),
                                 collapse = ", "), ")")),
    "")
}

#' @rdname batch_code
#' @export
autoexec_all_code <- function(date = Sys.Date()) {
  c(.autoexec_banner(file.path("programs", .autoexec_all_file),
                     "Official run: the ARD programs, then the report programs.",
                     "T-14-1-1         only these", date),
    .autoexec_body(c("ard", "tfl")))
}

# the runner and the autoexec programs, written with the study
.save_batch_programs <- function(p, root) {
  lay <- study_layout()
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, f) {
    f <- file.path(root, f)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f, root))
  }
  put(batch_code(p), file.path("programs", .batch_file))
  # the figure style, and the one function a report of one's own code ends
  # with (report_content()), written once here
  put(.fig_setup_code(p), file.path(lay[["programs_tfl"]], .fig_setup_file))
  put(report_setup_code(p), file.path(lay[["programs_tfl"]], .report_setup_file))
  put(autoexec_all_code(), file.path("programs", .autoexec_all_file))
  put(autoexec_code(p), file.path(lay[["programs_tfl"]], "autoexec_report.R"))
  out
}

#' Official runs of a study
#'
#' `run_batch()` runs the study's official-run program -- the ARD
#' (`programs/ard/autoexec_ard.R`), the reports
#' (`programs/tfl/autoexec_report.R`) or both (`programs/autoexec_all.R`)
#' -- which makes a batch folder `runs/<date>_<time>_<what>/` with each
#' program's log, what the run made and, with `code = TRUE`, the programs
#' and definition workbooks it ran.  `list_batches()` lists a study's
#' batch folders.
#'
#' The programs are the saved ones: save the study first.
#'
#' @param study An `rtfstudy`.
#' @param parts `"ard"`, `"tfl"` or both.
#' @param code Keep the code in the batch folder.
#' @param only Programs or outputs to run; `NULL` for all.
#' @param exclude Reports to leave out (their ARD program and their report
#'   program), by output id or program name; `NULL` for none.  The batch
#'   folder's `batch.txt` lists the programs left out.
#' @param batch A named batch ([batch_sets()]): only its reports run, and
#'   the batch folder (`runs/<date>_<time>_<what>_<batch>/`) and its
#'   `batch.txt` name it.  `NULL`: every report (or `only` / `exclude`).
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return `run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the
#'   batch folder), `result` (its run.csv), `output` (what the run
#'   printed); otherwise the process.  `list_batches()`: a data frame.
#' @export
run_batch <- function(study, parts = c("ard", "tfl"), code = TRUE,
                      only = NULL, exclude = NULL, batch = NULL, wait = TRUE) {
  lay <- study_layout()
  parts <- match.arg(parts, c("ard", "tfl"), several.ok = TRUE)
  prog <- if (length(parts) == 2L) file.path("programs", .autoexec_all_file) else
    if (parts == "ard") file.path(lay[["programs_ard"]], .ard_autoexec_file) else
      file.path(lay[["programs_tfl"]], "autoexec_report.R")
  if (!file.exists(file.path(study$path, prog))) {
    stop("The study has no ", prog, " yet: save it first.", call. = FALSE)
  }
  if (length(batch) && !batch %in% names(batch_sets(study$planner))) {
    stop("No batch named ", sQuote(batch), ": the study's are ",
         paste(sQuote(names(batch_sets(study$planner))), collapse = ", "),
         ".", call. = FALSE)
  }
  # leaving reports out, a named batch: the runner that knows how (a study
  # saved by an earlier tflplanner has an older programs/batch.R)
  runner <- readLines(file.path(study$path, "programs", .batch_file), warn = FALSE)
  if ((length(exclude) && !any(grepl("--exclude=", runner, fixed = TRUE))) ||
      (length(batch) && !any(grepl("--batch", runner, fixed = TRUE)))) {
    stop("programs/batch.R was written by an earlier tflplanner and cannot leave ",
         "reports out or run a named batch: save the study to write it again.",
         call. = FALSE)
  }
  before <- list_batches(study)$batch
  out <- tempfile("batch", fileext = ".txt")
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(prog, if (!code) "--no-code", only,
      if (length(exclude)) paste0("--exclude=", .batch_exclude(study$planner, exclude)),
      if (length(batch)) c("--batch", batch)),
    wd = study$path,
    stdout = out, stderr = "2>&1")
  if (!wait) return(px)
  px$wait()
  txt <- if (file.exists(out)) readLines(out, warn = FALSE) else character()
  b <- setdiff(list_batches(study)$batch, before)
  dir <- if (length(b)) file.path(study$path, lay[["runs"]], b[1L])
  csv <- if (!is.null(dir)) file.path(dir, "run.csv")
  list(ok = identical(px$get_exit_status(), 0L), batch = dir,
       result = if (!is.null(csv) && file.exists(csv))
         utils::read.csv(csv, colClasses = "character"),
       output = txt)
}

# The programs a report left out of a run is: its ARD program and its report
# program (an output id), or the program named
.batch_exclude <- function(x, exclude) {
  ids <- x$outputs$output_id
  progs <- unlist(lapply(exclude, function(e) {
    if (!e %in% ids) return(e)
    c(.ard_prog_name(e), report_info(x, e)$program)
  }))
  unique(progs[!is.na(progs) & nzchar(progs)])
}

# For each report, the reports whose rows of the study ARD it reads: its own
# when it has analyses, and any other report with analyses its own code
# names in quotes ("T-14-1-1").  A named list: output id -> output ids.
.batch_ard_reads <- function(x) {
  a <- x$ard$analyses
  has <- unique(stats::na.omit(a$output_id))
  ids <- x$outputs$output_id
  code <- x$outputs$data_code
  stats::setNames(lapply(seq_along(ids), function(i) {
    q <- if (!is.na(code[i])) {
      unlist(regmatches(code[i], gregexpr('"[^"]+"', code[i])))
    }
    q <- gsub('"', "", q)
    unique(c(if (ids[i] %in% has) ids[i], intersect(setdiff(q, ids[i]), has)))
  }), ids)
}

# The reports of a run that read an ARD the run leaves out: data frame
# report (kept) / needs (left out)
.batch_excluded_needs <- function(x, exclude) {
  reads <- .batch_ard_reads(x)
  keep <- setdiff(names(reads), exclude)
  rows <- lapply(keep, function(k) {
    n <- intersect(reads[[k]], exclude)
    if (length(n)) data.frame(report = k, needs = n, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(report = character(), needs = character()) else out
}

#' @rdname run_batch
#' @export
list_batches <- function(study) {
  root <- file.path(study$path, study_layout()[["runs"]])
  dirs <- if (dir.exists(root)) sort(list.dirs(root, full.names = FALSE,
                                               recursive = FALSE),
                                     decreasing = TRUE) else character()
  rows <- lapply(dirs, function(d) {
    csv <- file.path(root, d, "run.csv")
    r <- if (file.exists(csv)) tryCatch(
      utils::read.csv(csv, colClasses = "character"),
      error = function(e) NULL)
    # the programs the run left out (batch.txt's "Excluded :")
    bt <- file.path(root, d, "batch.txt")
    ex <- if (file.exists(bt)) sub("^Excluded :\\s*", "", grep("^Excluded :",
      readLines(bt, warn = FALSE), value = TRUE))
    ex <- if (length(ex) && !identical(ex[1L], "none")) ex[1L] else ""
    m <- regmatches(d, regexec("^([0-9]{8})_([0-9]{6})_(ard|report|all)(?:_(.*))?$",
                               d, perl = TRUE))[[1L]]
    # the named batch: batch.txt's "Batch set :" (the folder carries it too)
    set <- if (file.exists(bt <- file.path(root, d, "batch.txt"))) sub(
      "^Batch set :\\s*", "", grep("^Batch set :", readLines(bt, warn = FALSE),
                                   value = TRUE))
    set <- if (length(set) && !identical(set[1L], "none")) set[1L] else ""
    data.frame(
      batch = d,
      started = if (length(m)) paste(
        format(as.Date(m[2L], "%Y%m%d")),
        paste(substring(m[3L], c(1L, 3L, 5L), c(2L, 4L, 6L)), collapse = ":"))
      else NA_character_,
      what = if (length(m)) m[4L] else NA_character_,
      set = set,
      programs = if (is.null(r)) NA_integer_ else nrow(r),
      errors = if (is.null(r)) NA_integer_ else sum(r$status != "OK"),
      code = dir.exists(file.path(root, d, "code")),
      excluded = ex,
      path = file.path(root, d), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    batch = character(), started = character(), what = character(),
    set = character(), programs = integer(), errors = integer(), code = logical(),
    excluded = character(), path = character())), rows))
  rownames(out) <- NULL
  out
}
