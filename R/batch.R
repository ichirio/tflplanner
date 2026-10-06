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
                                               .fig_setup_file)),
            wb(c(.table_file, .report_file, .lf_file, .ard_file))),
    all = c(file.path("programs", c(.batch_file, .autoexec_all_file)),
            .study_file))
  vec <- function(v, names = NULL) {
    if (!length(v)) return("character()")
    q <- encodeString(v, quote = "\"")
    if (!is.null(names)) q <- paste(encodeString(names, quote = "`"), "=", q)
    paste0("c(\n", paste0("  ", q, collapse = ",\n"), ")")
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
    paste0(".batch_programs <- list(\nard = ", vec(ard_progs),
           ",\ntfl = ", vec(tfl_progs), ")"),
    "# the output each ARD program makes",
    paste0(".batch_ard_outputs <- ", vec(ids, basename(ard_progs))),
    "# what each report program makes",
    paste0(".batch_outputs <- list(\n",
           paste0("  ", encodeString(tfl_progs, quote = "`"), " = ",
                  vapply(made, function(m) paste0("c(", paste(
                    encodeString(m, quote = "\""), collapse = ", "), ")"), ""),
                  collapse = ",\n"), ")"),
    "# kept with the code of a run",
    paste0(".batch_support <- list(\nard = ", vec(support$ard),
           ",\ntfl = ", vec(support$tfl), ",\nall = ", vec(support$all), ")"),
    "",
    runner,
    "")
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
  put(tflspec::tfl_fig_setup_code(.std_fig_style()),
      file.path(lay[["programs_tfl"]], .fig_setup_file))
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
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return `run_batch()`: with `wait = TRUE`, a list: `ok`, `batch` (the
#'   batch folder), `result` (its run.csv), `output` (what the run
#'   printed); otherwise the process.  `list_batches()`: a data frame.
#' @export
run_batch <- function(study, parts = c("ard", "tfl"), code = TRUE,
                      only = NULL, wait = TRUE) {
  lay <- study_layout()
  parts <- match.arg(parts, c("ard", "tfl"), several.ok = TRUE)
  prog <- if (length(parts) == 2L) file.path("programs", .autoexec_all_file) else
    if (parts == "ard") file.path(lay[["programs_ard"]], .ard_autoexec_file) else
      file.path(lay[["programs_tfl"]], "autoexec_report.R")
  if (!file.exists(file.path(study$path, prog))) {
    stop("The study has no ", prog, " yet: save it first.", call. = FALSE)
  }
  before <- list_batches(study)$batch
  out <- tempfile("batch", fileext = ".txt")
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(prog, if (!code) "--no-code", only), wd = study$path,
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
    m <- regmatches(d, regexec("^([0-9]{8})_([0-9]{6})_(.*)$", d))[[1L]]
    data.frame(
      batch = d,
      started = if (length(m)) paste(
        format(as.Date(m[2L], "%Y%m%d")),
        paste(substring(m[3L], c(1L, 3L, 5L), c(2L, 4L, 6L)), collapse = ":"))
      else NA_character_,
      what = if (length(m)) m[4L] else NA_character_,
      programs = if (is.null(r)) NA_integer_ else nrow(r),
      errors = if (is.null(r)) NA_integer_ else sum(r$status != "OK"),
      code = dir.exists(file.path(root, d, "code")),
      path = file.path(root, d), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(
    batch = character(), started = character(), what = character(),
    programs = integer(), errors = integer(), code = logical(),
    path = character())), rows))
  rownames(out) <- NULL
  out
}
