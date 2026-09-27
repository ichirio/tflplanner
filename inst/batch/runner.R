# ---- the runner ------------------------------------------------------------
# (tflplanner writes the lists above for the study; this part is the same
# for every study)

.rscript <- file.path(R.home("bin"), "Rscript")

# logrx's "Errors:" / "Warnings:" section: the tab-indented lines under it
.batch_section <- function(lines, head) {
  i <- match(head, lines)
  if (is.na(i)) return(character())
  out <- character()
  for (l in lines[-seq_len(i)]) {
    if (!startsWith(l, "\t")) break
    if (nzchar(trimws(l))) out <- c(out, trimws(l))
  }
  out
}

# one program in its own R process, from the study folder, with its log
.batch_run <- function(part, prog, log_dir, engine) {
  log <- file.path(log_dir, sub("[.][Rr]$", ".log", basename(prog)))
  t0 <- Sys.time()
  if (engine == "logrx") {
    expr <- sprintf("logrx::axecute('%s', log_name = '%s', log_path = '%s')",
                    prog, basename(log), log_dir)
    rc <- system2(.rscript, c("-e", shQuote(expr)), stdout = FALSE,
                  stderr = FALSE)
    lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    errs <- .batch_section(lines, "Errors:")
    warns <- .batch_section(lines, "Warnings:")
  } else {
    rc <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "BATCH", "--no-save", "--no-restore", "--quiet",
                    shQuote(prog), shQuote(log)))
    lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    errs <- grep("^Error", lines, value = TRUE)
    warns <- grep("^Warning", lines, value = TRUE)
  }
  status <- if (identical(rc, 0L)) "OK" else "ERROR"
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("%-5s %-4s %-32s %6.1fs\n", status, part, basename(prog), secs))
  data.frame(part = part, program = prog, status = status,
             errors = length(errs), warnings = length(warns),
             note = if (length(errs)) errs[1L] else "",
             started = format(t0, "%Y-%m-%d %H:%M:%S"), seconds = secs,
             md5 = unname(tools::md5sum(prog)), log = log,
             stringsAsFactors = FALSE)
}

# an ARD program that failed: the study ARD keeps that output's rows, and
# its error is recorded
.batch_ard_error <- function(output_id, note) {
  sf <- file.path(dirname(.batch_ard), "ard_status.csv")
  row <- data.frame(output_id = output_id, definition = "",
                    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    rows = "", error = if (nzchar(note)) note else "failed",
                    stringsAsFactors = FALSE)
  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = "character")
  if (!is.null(st) && nrow(st)) {
    for (k in setdiff(names(row), names(st))) st[[k]] <- ""
    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)
  }
  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(row, sf, row.names = FALSE)
}

.batch_copy <- function(files, to) {
  for (f in files[file.exists(files)]) {
    dir.create(file.path(to, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(f, file.path(to, f), overwrite = TRUE, copy.date = TRUE)
  }
}

# An official run: the programs of `parts` ("ard", "tfl"), each logged, and
# a batch folder runs/<date>_<time>_<what>/ that keeps what the run used
# and made:
#   logs/      one log per program (logrx; R CMD BATCH without logrx)
#   output/    the study ARD the run made or read, the reports it made
#   code/      the programs and definition workbooks it ran (not with
#              --no-code)
#   run.csv    one row per program: status, errors, warnings, time, md5
#   batch.txt  who, where, when, which R
# `args`: --no-code, and program or output names to run only those.
run_batch <- function(parts, args = character()) {
  if (!file.exists("study.yml")) {
    stop("Run it from the study folder (the one with study.yml).")
  }
  code <- !"--no-code" %in% args
  only <- setdiff(args, "--no-code")
  what <- if (length(parts) > 1L) "all" else
    c(ard = "ard", tfl = "report")[[parts]]
  started <- Sys.time()
  dir <- file.path(.batch_root, paste0(format(started, "%Y%m%d_%H%M%S"), "_",
                                       what))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  engine <- if (requireNamespace("logrx", quietly = TRUE)) "logrx" else "batch"
  if (engine == "batch") message("logrx is not installed: the logs are R CMD BATCH's.")
  cat("Batch folder:", dir, "\n\n")
  rows <- list()
  for (part in parts) {
    progs <- .batch_programs[[part]]
    if (length(only)) {
      progs <- progs[basename(progs) %in% only |
                       sub("[.][Rr]$", "", basename(progs)) %in% only]
    }
    if (!length(progs)) next
    log_dir <- file.path(dir, "logs", part)
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    for (p in progs) {
      r <- .batch_run(part, p, log_dir, engine)
      if (part == "ard" && r$status != "OK") {
        .batch_ard_error(.batch_ard_outputs[[basename(p)]], r$note)
      }
      rows[[length(rows) + 1L]] <- r
    }
  }
  result <- do.call(rbind, rows)
  if (is.null(result)) stop("Nothing to run.")

  # what the run made (and, for the reports, the study ARD they read)
  ok <- result$program[result$status == "OK"]
  .batch_copy(c(.batch_ard, file.path(dirname(.batch_ard), "ard_status.csv"),
                unlist(.batch_outputs[intersect(names(.batch_outputs), ok)])),
              file.path(dir))
  if (code) .batch_copy(unique(c(result$program,
                                 unlist(.batch_support[c(parts, "all")]))),
                        file.path(dir, "code"))
  result$log <- substring(result$log, nchar(dir) + 2L)
  utils::write.csv(result, file.path(dir, "run.csv"), row.names = FALSE)
  writeLines(c(
    paste("Batch    :", basename(dir)),
    paste("Runs     :", paste(parts, collapse = ", "),
          if (length(only)) paste0("(", paste(only, collapse = ", "), ")")),
    paste("Started  :", format(started, "%Y-%m-%d %H:%M:%S")),
    paste("Finished :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("User     :", Sys.info()[["user"]]),
    paste("Machine  :", Sys.info()[["nodename"]]),
    paste("R        :", R.version.string),
    paste("Logs     :", if (engine == "logrx") "logrx" else "R CMD BATCH"),
    paste("Code     :", if (code) "kept (code/)" else "not kept"),
    paste("Result   :", sum(result$status == "OK"), "of", nrow(result),
          "program(s) without error")),
    file.path(dir, "batch.txt"))

  cat(sprintf("\n%d of %d program(s) ran without error.  %s\n",
              sum(result$status == "OK"), nrow(result), dir))
  if (any(result$status != "OK")) {
    print(result[result$status != "OK", c("program", "note")],
          right = FALSE, row.names = FALSE)
    if (!interactive()) quit(status = 1L)
  }
  invisible(result)
}
