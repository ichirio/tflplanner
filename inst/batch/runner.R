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
# whether a report's rows of the study ARD are not made from the definition
# batch.R was written with: none, an error, or another fingerprint
.batch_ard_stale <- function(output_id) {
  sf <- file.path(dirname(.batch_ard), "ard_status.csv")
  if (!file.exists(sf)) return(TRUE)
  st <- utils::read.csv(sf, colClasses = "character")
  r <- st[st$output_id == output_id, , drop = FALSE]
  if (!nrow(r)) return(TRUE)
  if (!is.na(r$error[1L]) && nzchar(r$error[1L])) return(TRUE)
  want <- unname(.batch_needs_hash[output_id])
  !is.na(want) && nzchar(want) && !identical(r$definition[1L], want)
}

.batch_run <- function(part, prog, log_dir, engine) {
  log <- file.path(log_dir, sub("[.][Rr]$", ".log", basename(prog)))
  t0 <- Sys.time()
  # no program to run: the study was not saved since, or its ARD
  # definition does not hold (saving then writes no ARD program)
  if (!file.exists(prog)) {
    note <- paste0("no program ", prog, ": save the study (an ARD definition ",
                   "that does not hold writes none; see the Review tab)")
    cat(sprintf("%-5s %-4s %-32s %s\n", "ERROR", part, basename(prog), note))
    return(data.frame(part = part, program = prog, status = "ERROR",
                      errors = 1L, warnings = 0L, note = note,
                      started = format(t0, "%Y-%m-%d %H:%M:%S"), seconds = 0,
                      md5 = NA_character_, log = NA_character_,
                      stringsAsFactors = FALSE))
  }
  if (engine == "logrx") {
    expr <- sprintf("logrx::axecute('%s', log_name = '%s', log_path = '%s')",
                    prog, basename(log), log_dir)
    # what R says on stderr is kept beside the log: an error that stops the
    # program before logrx can log it (a package too old) is only there
    err <- sub("[.]log$", ".err", log)
    rc <- system2(.rscript, c("-e", shQuote(expr)), stdout = FALSE,
                  stderr = err)
    lines <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    errs <- .batch_section(lines, "Errors:")
    warns <- .batch_section(lines, "Warnings:")
    if (!identical(rc, 0L) && !length(errs) && file.exists(err)) {
      e <- grep("^Error", readLines(err, warn = FALSE), value = TRUE)
      if (length(e)) errs <- e[length(e)]
    }
    if (file.exists(err) && !file.size(err)) unlink(err)
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

# The data files the run's programs read (.batch_data, by the data
# catalog), copied into the batch folder's data/ under their path in the
# study folder (a file outside it: data/external/<name>), and listed with
# their size, md5 and time in data_manifest.csv.  Returns the manifest.
.batch_keep_data <- function(progs, dir) {
  files <- unique(unlist(.batch_data[intersect(names(.batch_data), progs)],
                         use.names = FALSE))
  files <- files[file.exists(files)]
  to <- ifelse(grepl("^([A-Za-z]:)?[/\\\\]", files) | startsWith(files, ".."),
               file.path("external", basename(files)), files)
  for (i in seq_along(files)) {
    dir.create(file.path(dir, "data", dirname(to[i])), recursive = TRUE,
               showWarnings = FALSE)
    file.copy(files[i], file.path(dir, "data", to[i]), overwrite = TRUE,
              copy.date = TRUE)
  }
  man <- data.frame(
    path = files, kept = file.path("data", to),
    size = unname(file.size(files)),
    md5 = unname(tools::md5sum(file.path(dir, "data", to))),
    modified = format(file.mtime(files), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE)
  utils::write.csv(man, file.path(dir, "data_manifest.csv"), row.names = FALSE)
  man
}

# The packages the run's programs name (library(), require(), pkg::), with
# the versions installed here, and sessionInfo() once they are loaded:
# session_info.txt.  The study's renv.lock, when it has one, is kept too.
# Never the end of a run: what cannot be read is said in the file.
.batch_session_info <- function(progs, dir) {
  ver <- tryCatch({
    # the R files only (the support files are workbooks too)
    progs <- progs[grepl("[.][Rr]$", progs) & file.exists(progs)]
    code <- unlist(lapply(progs, readLines, warn = FALSE, encoding = "UTF-8"))
    code <- sub("#.*$", "", code, useBytes = TRUE)
    lib <- unlist(regmatches(code, gregexpr(
      "(library|require|requireNamespace)\\(\\s*['\"]?[A-Za-z][A-Za-z0-9.]*", code,
      perl = TRUE, useBytes = TRUE)))
    lib <- sub("^.*\\(\\s*['\"]?", "", lib, useBytes = TRUE)
    ns <- unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=:::?)", code,
                                           perl = TRUE, useBytes = TRUE)))
    pkgs <- sort(unique(c(lib, ns)))
    pkgs <- pkgs[vapply(pkgs, function(p) nzchar(system.file(package = p)), NA)]
    for (p in pkgs) try(suppressPackageStartupMessages(loadNamespace(p)), silent = TRUE)
    vapply(pkgs, function(p) as.character(utils::packageVersion(p)), "")
  }, error = function(e) structure(character(), note = conditionMessage(e)))
  writeLines(c(
    "The R and packages of the run: the packages its programs name, with",
    "the versions installed where it ran, then sessionInfo() once they are loaded.",
    "",
    sprintf("%-24s %s", c("package", names(ver)), c("version", ver)),
    if (!is.null(attr(ver, "note"))) paste("(the programs could not be read:",
                                           attr(ver, "note"), ")"),
    "",
    utils::capture.output(print(utils::sessionInfo()))),
    file.path(dir, "session_info.txt"))
  if (file.exists("renv.lock")) file.copy("renv.lock", file.path(dir, "renv.lock"))
  invisible(ver)
}

# An official run: the programs of `parts` ("ard", "tfl"), each logged, and
# a batch folder runs/<date>_<time>_<what>/ that keeps what the run used
# and made:
#   logs/      one log per program (logrx; R CMD BATCH without logrx)
#   output/    the study ARD the run made or read, the reports it made
#   code/      the programs and definition workbooks it ran (not with
#              --no-code)
#   data/      with --data: the data files the programs read, under their
#              path in the study folder; data_manifest.csv lists them
#              (size, md5, time)
#   run.csv    one row per program: status, errors, warnings, time, md5
#   session_info.txt  the R and package versions (renv.lock when the study
#              has one)
#   batch.txt  who, where, when, which R, what was kept
# `args`: --no-code; --data; program or output names to run only those;
# --exclude=<name> (again for another) to leave a program out; --batch <name>
# (or --batch=<name>) to run a named batch of `.batch_sets`.
run_batch <- function(parts, args = character()) {
  if (!file.exists("study.yml")) {
    stop("Run it from the study folder (the one with study.yml).")
  }
  code <- !"--no-code" %in% args
  data <- "--data" %in% args
  ex_args <- grep("^--exclude=", args, value = TRUE)
  exclude <- sub("^--exclude=", "", ex_args)
  # a named batch: --batch Topline, or --batch=Topline
  set <- NULL
  b <- match("--batch", args)
  if (!is.na(b)) {
    set <- args[b + 1L]
    args <- args[-c(b, b + 1L)]
  }
  b <- grep("^--batch=", args)
  if (length(b)) {
    set <- sub("^--batch=", "", args[b[1L]])
    args <- args[-b]
  }
  if (!is.null(set)) {
    if (is.na(set) || !set %in% names(.batch_sets)) {
      stop("No batch named '", set, "': the study's are ",
           paste(names(.batch_sets), collapse = ", "), ".")
    }
    # its reports without a program to run (not saved yet, or none of the
    # parts run): said, and run what there is
    progs_of <- c(.batch_ard_outputs, .batch_report_ids)
    gone <- setdiff(.batch_sets[[set]], progs_of)
    if (length(gone)) {
      message("Batch '", set, "': no program for ", paste(gone, collapse = ", "),
              " (save the study to write them); the rest runs.")
    }
  }
  only <- setdiff(args, c("--no-code", "--data", ex_args))
  named <- function(progs, names) {
    basename(progs) %in% names | sub("[.][Rr]$", "", basename(progs)) %in% names
  }
  excluded <- character()
  what <- if (length(parts) > 1L) "all" else
    c(ard = "ard", tfl = "report")[[parts]]
  if (!is.null(set)) what <- paste0(what, "_", gsub("[^A-Za-z0-9._-]", "_", set))
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
    if (length(only)) progs <- progs[named(progs, only)]
    if (!is.null(set)) {
      of <- if (part == "ard") .batch_ard_outputs[basename(progs)] else
        .batch_report_ids[progs]
      keep <- !is.na(of) & of %in% .batch_sets[[set]]
      excluded <- c(excluded, progs[!keep])
      progs <- progs[keep]
    }
    if (length(exclude)) {
      excluded <- c(excluded, progs[named(progs, exclude)])
      progs <- progs[!named(progs, exclude)]
    }
    if (!length(progs)) next
    log_dir <- file.path(dir, "logs", part)
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    for (p in progs) {
      # a figure printing an ARD's numbers: that ARD first, when it is not
      # made from the definition it has now (#293)
      need <- if (part == "tfl") unname(.batch_needs[p]) else NA_character_
      if (length(need) && !is.na(need) && .batch_ard_stale(need)) {
        ap <- .batch_programs$ard[.batch_ard_outputs[basename(.batch_programs$ard)] %in% need]
        if (length(ap)) {
          cat(sprintf("(%s prints %s's ARD, which is not made from its definition: made first)
",
                      basename(p), need))
          ard_dir <- file.path(dir, "logs", "ard")
          dir.create(ard_dir, recursive = TRUE, showWarnings = FALSE)
          ra <- .batch_run("ard", ap[1L], ard_dir, engine)
          if (ra$status != "OK") .batch_ard_error(need, ra$note)
          rows[[length(rows) + 1L]] <- ra
        }
      }
      r <- .batch_run(part, p, log_dir, engine)
      if (part == "ard" && r$status != "OK") {
        .batch_ard_error(.batch_ard_outputs[[basename(p)]], r$note)
      }
      rows[[length(rows) + 1L]] <- r
    }
  }
  result <- do.call(rbind, rows)
  if (is.null(result)) stop("Nothing to run.")
  if (length(excluded)) {
    cat("\nLeft out:", paste(basename(excluded), collapse = ", "), "\n")
  }

  # what the run made (and, for the reports, the study ARD they read)
  ok <- result$program[result$status == "OK"]
  made <- as.character(unlist(.batch_outputs[intersect(names(.batch_outputs), ok)]))
  # (with the reports' status, as the ARD's)
  .batch_copy(c(.batch_ard, file.path(dirname(.batch_ard), "ard_status.csv"), made,
                unique(file.path(dirname(made[grepl("[.]rtf$", made)]),
                                 "report_status.csv"))),
              file.path(dir))
  if (code) .batch_copy(unique(c(result$program,
                                 unlist(.batch_support[c(parts, "all")]))),
                        file.path(dir, "code"))
  man <- if (data) .batch_keep_data(result$program, dir)
  .batch_session_info(unique(c(result$program,
                               unlist(.batch_support[c(parts, "all")]))), dir)
  result$log <- substring(result$log, nchar(dir) + 2L)
  utils::write.csv(result, file.path(dir, "run.csv"), row.names = FALSE)
  writeLines(c(
    paste("Batch    :", basename(dir)),
    paste("Runs     :", paste(parts, collapse = ", "),
          if (length(only)) paste0("(", paste(only, collapse = ", "), ")")),
    paste("Batch set :", if (is.null(set)) "none" else set),
    paste("Started  :", format(started, "%Y-%m-%d %H:%M:%S")),
    paste("Finished :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("User     :", Sys.info()[["user"]]),
    paste("Machine  :", Sys.info()[["nodename"]]),
    paste("R        :", R.version.string),
    paste("Logs     :", if (engine == "logrx") "logrx" else "R CMD BATCH"),
    paste("Code     :", if (code) "kept (code/)" else "not kept"),
    paste("Data     :", if (data) sprintf(
      "kept (data/, %d file(s), %.1f MB; data_manifest.csv)", nrow(man),
      sum(man$size) / 1024^2) else "not kept"),
    paste("Session  :", if (file.exists(file.path(dir, "renv.lock")))
      "session_info.txt and renv.lock" else "session_info.txt"),
    paste("Excluded :", if (length(excluded))
      paste(excluded, collapse = ", ") else "none"),
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
