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
#   programs/ard/autoexec_ard.R   runs them all (or the ones named), each in
#                                 its own R process with its log in logs/ard/
#
# The report programs are in programs/tfl/ (autoexec_report.R runs them,
# logs in logs/tfl/).  So the ARD and the reports can be made by different
# people at different times: a table reads whatever of the study ARD is
# there.
#
# Like the report programs, a generated ARD program carries a checksum: one
# nobody edited follows the definition on every save, one edited by hand is
# left alone.

.ard_prog_name <- function(output_id) {
  paste0(gsub("[^A-Za-z0-9._-]", "_", output_id), ".R")
}

.ard_setup_file <- "ard_setup.R"
.ard_autoexec_file <- "autoexec_ard.R"

# how programs are run and logged: R CMD BATCH (the code and its output, as
# R writes it) or logrx::axecute() (when the standards say logrx and it is
# installed)
.log_engine <- function() {
  e <- .std_setting("log_engine", "batch")
  if (e %in% c("batch", "logrx")) e else "batch"
}

#' The ARD programs of a study
#'
#' `ard_setup_code()` is `programs/ard/ard_setup.R`, which every ARD program
#' sources: cards, the statistics tflplanner computes ([ard_statistics()]),
#' the stat_fmt formats, and `.save_output()`, which replaces one output's
#' rows of the study ARD and records the build.  `ard_program_code()` is one
#' output's program, `programs/ard/<output_id>.R`.  `ard_autoexec_code()` is
#' `programs/ard/autoexec_ard.R`, which runs them from the study folder --
#' all, or the ones named (`Rscript programs/ard/autoexec_ard.R T-14-1-1`)
#' -- each in its own R process with its log in `logs/ard/`.
#'
#' @param spec An [ard_spec()] (or the path of one).
#' @param output_id The output.
#' @param date The date stamped in the banner.
#' @return The code, one element per line.
#' @export
ard_setup_code <- function(spec, date = Sys.Date()) {
  x <- if (is.character(spec)) read_ard_spec(spec) else spec
  lay <- study_layout()
  out <- .study_value(x, "output", "output/ard/ard.rds")
  st <- ard_statistics("continuous")
  c(.banner(
      paste("Program    :", file.path(lay[["programs_ard"]], .ard_setup_file)),
      "What every ARD program of the study starts with (it sources this).",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    .ard_common_lines(st$statistic[!is.na(st$fun)]),
    "# one output's rows into the study ARD, the other outputs' left as they",
    "# are; and what was built, from which definition (tflplanner reads it)",
    ".save_output <- function(ard, output_id, definition) {",
    paste0("  out <- ", encodeString(out, quote = "\"")),
    "  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)",
    "  old <- if (file.exists(out)) readRDS(out)",
    "  new <- if (is.null(old)) ard else",
    "    dplyr::bind_rows(old[old$output_id != output_id, , drop = FALSE], ard)",
    "  tmp <- paste0(out, \".tmp\")",
    "  saveRDS(new, tmp)",
    "  file.rename(tmp, out)",
    "  sf <- file.path(dirname(out), \"ard_status.csv\")",
    "  row <- data.frame(output_id = output_id, definition = definition,",
    "                    built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
    "                    rows = as.character(nrow(ard)), error = \"\",",
    "                    stringsAsFactors = FALSE)",
    "  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = \"character\")",
    "  if (!is.null(st) && nrow(st)) {",
    "    for (k in setdiff(names(row), names(st))) st[[k]] <- \"\"",
    "    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)",
    "  }",
    "  utils::write.csv(row, sf, row.names = FALSE)",
    "  cat(sprintf(\"%s: %d rows into %s\\n\", output_id, nrow(ard), out))",
    "  invisible(ard)",
    "}",
    "")
}

#' @rdname ard_setup_code
#' @export
ard_program_code <- function(spec, output_id, date = Sys.Date()) {
  x <- if (is.character(spec)) read_ard_spec(spec) else spec
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
      paste("Analyses   :", paste(labels, collapse = ", ")),
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d")),
      "",
      "Made from spec/ard_spec.xlsx.  Runs from the study folder (open the",
      "study's .Rproj, or run programs/ard/autoexec_ard.R)."),
    "",
    sprintf("source(%s)", encodeString(file.path(lay[["programs_ard"]],
                                                 .ard_setup_file),
                                       quote = "\"")),
    "",
    .ard_body_lines(x, a),
    "",
    sprintf(".save_output(ard, %s, %s)", encodeString(output_id, quote = "\""),
            encodeString(.ard_output_hash(x, output_id), quote = "\"")),
    "")
}

#' @rdname ard_setup_code
#' @export
ard_autoexec_code <- function(spec, date = Sys.Date()) {
  x <- if (is.character(spec)) read_ard_spec(spec) else spec
  lay <- study_layout()
  ids <- unique(stats::na.omit(x$analyses$output_id))
  out <- .study_value(x, "output", "output/ard/ard.rds")
  q <- function(v) paste0("  ", encodeString(v, quote = "\""),
                          c(rep(",", max(0L, length(v) - 1L)), "")[seq_along(v)])
  c(.banner(
      paste("Program    :", file.path(lay[["programs_ard"]],
                                      .ard_autoexec_file)),
      "Makes the study ARD: runs the ARD programs, one per output.",
      "  Rscript programs/ard/autoexec_ard.R            every output",
      "  Rscript programs/ard/autoexec_ard.R T-14-1-1   only these",
      "Run it from the study folder.  Each program runs in its own R process;",
      "its log is logs/ard/<program>.log.",
      paste0("Generated  : tflplanner ", utils::packageVersion("tflplanner"),
             ", ", format(date, "%Y-%m-%d"))),
    "",
    "if (!file.exists(\"study.yml\")) {",
    "  stop(\"Run autoexec_ard.R from the study folder (the one with study.yml).\")",
    "}",
    "",
    "outputs <- c(", q(ids), ")",
    "programs <- c(", q(vapply(ids, .ard_prog_name, "")), ")",
    "",
    "only <- if (interactive()) character() else commandArgs(trailingOnly = TRUE)",
    "if (length(only)) {",
    "  pick <- outputs %in% only | programs %in% only |",
    "    sub(\"\\\\.[Rr]$\", \"\", programs) %in% only",
    "  outputs <- outputs[pick]",
    "  programs <- programs[pick]",
    "}",
    "",
    .runner_lines(lay[["programs_ard"]], lay[["logs_ard"]]),
    "# a failed program leaves the study ARD alone; its error is recorded",
    "record_error <- function(output_id, note) {",
    paste0("  sf <- file.path(dirname(", encodeString(out, quote = "\""),
           "), \"ard_status.csv\")"),
    "  row <- data.frame(output_id = output_id, definition = \"\",",
    "                    built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
    "                    rows = \"\", error = if (nzchar(note)) note else \"failed\",",
    "                    stringsAsFactors = FALSE)",
    "  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = \"character\")",
    "  if (!is.null(st) && nrow(st)) {",
    "    for (k in setdiff(names(row), names(st))) st[[k]] <- \"\"",
    "    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)",
    "  }",
    "  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)",
    "  utils::write.csv(row, sf, row.names = FALSE)",
    "}",
    "",
    "result <- do.call(rbind, lapply(seq_along(programs), function(i) {",
    "  r <- run_program(programs[i])",
    "  if (r$status != \"OK\") record_error(outputs[i], r$note)",
    "  r",
    "}))",
    .runner_end("autoexec_ard.csv"),
    "")
}

# The part of an autoexec program that runs one program in its own R
# process and keeps its log: R CMD BATCH, or logrx::axecute().
.runner_lines <- function(program_dir, log_dir) {
  c(paste0("program_dir <- ", .r_string(program_dir)),
    paste0("log_dir     <- ", .r_string(log_dir)),
    paste0("log_engine  <- ", .r_string(.log_engine()),
           "   # batch: R CMD BATCH; logrx: logrx::axecute()"),
    "dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)",
    "if (log_engine == \"logrx\" && !requireNamespace(\"logrx\", quietly = TRUE)) {",
    "  message(\"logrx is not installed: the logs are R CMD BATCH's.\")",
    "  log_engine <- \"batch\"",
    "}",
    "",
    "run_program <- function(p) {",
    "  prog <- file.path(program_dir, p)",
    "  log  <- file.path(log_dir, sub(\"\\\\.[Rr]$\", \".log\", p))",
    "  if (file.exists(log)) file.remove(log)",
    "  t0 <- Sys.time()",
    "  rc <- if (log_engine == \"logrx\") {",
    "    system2(file.path(R.home(\"bin\"), \"Rscript\"),",
    "            c(\"-e\", shQuote(sprintf(",
    "              \"logrx::axecute('%s', log_name = '%s', log_path = '%s')\",",
    "              prog, basename(log), log_dir))),",
    "            stdout = FALSE, stderr = FALSE)",
    "  } else {",
    "    system2(file.path(R.home(\"bin\"), \"R\"),",
    "            c(\"CMD\", \"BATCH\", \"--no-save\", \"--no-restore\", \"--quiet\",",
    "              shQuote(prog), shQuote(log)))",
    "  }",
    "  # who ran it, where, with which R",
    "  if (log_engine == \"batch\") {",
    "    cat(\"\", sprintf(\"# %s: run by %s on %s, %s, %s; exit status %s\", prog,",
    "                    Sys.info()[[\"user\"]], Sys.info()[[\"nodename\"]],",
    "                    format(t0, \"%Y-%m-%d %H:%M:%S\"), R.version.string, rc),",
    "        file = log, sep = \"\\n\", append = TRUE)",
    "  }",
    "  lines  <- if (file.exists(log)) readLines(log, warn = FALSE) else \"\"",
    "  status <- if (identical(rc, 0L)) \"OK\" else \"ERROR\"",
    "  err    <- grep(\"^[[:space:]]*Error\", lines)[1L]",
    "  note   <- if (status == \"OK\" || is.na(err)) \"\" else",
    "    paste(trimws(stats::na.omit(lines[err + 0:1])), collapse = \" \")",
    "  note   <- sub(\" ?Execution halted$\", \"\", note)",
    "  warns  <- sum(grepl(\"^[[:space:]]*Warning\", lines))",
    "  secs   <- round(as.numeric(difftime(Sys.time(), t0, units = \"secs\")), 1)",
    "  cat(sprintf(\"%-5s %-30s %6.1fs\\n\", status, p, secs))",
    "  data.frame(program = p, status = status, warnings = warns,",
    "             seconds = secs, note = note, log = log,",
    "             stringsAsFactors = FALSE)",
    "}",
    "")
}

.runner_end <- function(csv) {
  c(paste0("utils::write.csv(result, file.path(log_dir, \"", csv,
           "\"), row.names = FALSE)"),
    "cat(sprintf(\"\\n%d of %d program(s) ran without error.\\n\",",
    "            sum(result$status == \"OK\"), nrow(result)))",
    "if (any(result$status != \"OK\")) {",
    "  print(result[result$status != \"OK\", c(\"program\", \"note\")],",
    "        right = FALSE, row.names = FALSE)",
    "  if (!interactive()) quit(status = 1L)",
    "}")
}

# missing / current / generated / edited, as a report program's
.ard_program_state <- function(code, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  if (!length(chk) || !identical(chk[1L], .body_hash(have))) return("edited")
  if (identical(.body_hash(have), .body_hash(code))) "current" else "generated"
}

# write a generated program unless it was edited by hand; the file's row of
# what the save did
.put_program <- function(code, f) {
  state <- .ard_program_state(code, f)
  if (state == "edited") return("kept")
  if (state == "current") return("unchanged")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  .write_program(code, f)
  "written"
}

# the ARD programs, written with the study: the setup, one per output, the
# autoexec; a program of an output no longer defined goes (unless edited)
.save_ard_programs <- function(spec, root) {
  lay <- study_layout()
  dir <- file.path(root, lay[["programs_ard"]])
  ids <- unique(stats::na.omit(spec$analyses$output_id))
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, name) {
    f <- file.path(dir, name)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f))
  }
  put(ard_setup_code(spec), .ard_setup_file)
  for (id in ids) put(ard_program_code(spec, id), .ard_prog_name(id))
  put(ard_autoexec_code(spec), .ard_autoexec_file)
  keep <- c(.ard_setup_file, .ard_autoexec_file, vapply(ids, .ard_prog_name, ""))
  for (f in setdiff(list.files(dir, "\\.[Rr]$"), keep)) {
    p <- file.path(dir, f)
    have <- readLines(p, warn = FALSE, encoding = "UTF-8")
    chk <- sub("^#  Checksum   : *", "",
               grep("^#  Checksum   :", have, value = TRUE))
    if (length(chk) && identical(chk[1L], .body_hash(have))) {
      file.remove(p)
      out[nrow(out) + 1L, ] <- list(p, "removed")
    }
  }
  # the single make_ard.R of earlier versions
  old <- file.path(root, "programs", "make_ard.R")
  if (file.exists(old)) {
    file.remove(old)
    out[nrow(out) + 1L, ] <- list(old, "removed")
  }
  out
}

#' Make the study ARD from its saved programs
#'
#' Runs `programs/ard/autoexec_ard.R` from the study folder -- every
#' output's ARD program, or the ones named -- each in its own R process with
#' its log in `logs/ard/`.  An output's program replaces its rows of the
#' study ARD; one that fails leaves them and records its error
#' ([ard_status()]).  `update_study_ard()` does it for one output.
#'
#' The programs are the saved ones: save the study first.
#'
#' @param study An `rtfstudy`.
#' @param output_id Outputs to make; `NULL` for all.
#' @param wait `FALSE` returns the running [processx::process] at once.
#' @return With `wait = TRUE`, a list: `ok`, `status` ([ard_status()]),
#'   `result` (autoexec_ard.csv: program, status, note, log) and `log` (the
#'   output of autoexec_ard.R).
#' @export
run_study_ard <- function(study, output_id = NULL, wait = TRUE) {
  lay <- study_layout()
  auto <- file.path(study$path, lay[["programs_ard"]], .ard_autoexec_file)
  if (!file.exists(auto)) {
    stop("The study has no ARD programs yet: save it first.", call. = FALSE)
  }
  dir.create(file.path(study$path, lay[["logs_ard"]]), recursive = TRUE,
             showWarnings = FALSE)
  log <- file.path(study$path, lay[["logs_ard"]], "autoexec_ard.log")
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(lay[["programs_ard"]], .ard_autoexec_file), output_id),
    wd = study$path, stdout = log, stderr = "2>&1")
  if (!wait) return(px)
  px$wait()
  csv <- file.path(study$path, lay[["logs_ard"]], "autoexec_ard.csv")
  res <- if (file.exists(csv)) utils::read.csv(csv, colClasses = "character")
  list(ok = identical(px$get_exit_status(), 0L), status = ard_status(study),
       result = res,
       log = if (file.exists(log)) readLines(log, warn = FALSE) else "")
}

#' @rdname run_study_ard
#' @export
update_study_ard <- function(study, output_id) {
  r <- run_study_ard(study, output_id)
  st <- r$status[r$status$output_id == output_id, , drop = FALSE]
  r$rows <- if (nrow(st)) st$rows[1L] else NA_integer_
  r$error <- if (!r$ok) {
    e <- if (nrow(st)) st$error[1L] else ""
    if (nzchar(e)) e else paste(utils::tail(r$log, 15L), collapse = "\n")
  }
  r
}
