# A figure printing a table's numbers turns stale with that table's ARD
# (#293 phase 4): its program records the definition of the ARD it read
# (record_report(ard =)), study_status() compares it with the definition
# now and says why (`ard:<id>`), and the official run and the preview make
# the table's ARD first when it is not made from its definition.

stale_study <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  withr::local_options(tflplanner.home = home, .local_envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(sample_study_copy())
}

fig_state <- function(s, id = "F-14-2-3") {
  st <- study_status(s)
  as.list(st[st$output_id == id, c("status", "why")])
}

test_that("a figure from an ARD records the definition it read; batch.R knows its need", {
  skip_on_cran()
  s <- stale_study()
  p <- s$planner
  expect_identical(.fig_ard_need(p, "F-14-2-3"), "T-14-2-2")
  expect_identical(.fig_ard_need(p, "T-14-2-2"), NA_character_)
  expect_identical(.fig_ard_need(p, "F-14-2-1"), NA_character_)
  code <- program_code(p, "F-14-2-3")
  expect_true("ard_built <- ard_fingerprint(\"T-14-2-2\")" %in% code)
  expect_true("record_report(report_id, ard = ard_built)" %in% code)
  # the others record no ARD
  expect_true("record_report(report_id)" %in% program_code(p, "T-14-2-2"))
  # record_report() has the column
  rs <- report_setup_code(p)
  expect_true(any(grepl("record_report <- function(output_id, ard = \"\", empty = NA)", rs, fixed = TRUE)))
  # (one element a statement: a line each)
  lines_of <- function(x) unlist(strsplit(x, "\n", fixed = TRUE))
  b <- lines_of(batch_code(p, root = s$path))
  # .batch_needs: a report program -> the reports whose ARD it reads (#790)
  needs_of <- function(lines) {
    i <- grep("^[.]batch_needs <- ", lines)
    j <- grep("^# a report program -> why the ARD", lines)
    eval(parse(text = paste(lines[i:(j - 1L)], collapse = "\n")))
  }
  n <- needs_of(b)
  # (F-14-2-2's own code reads T-14-2-2's ARD too, as its ARD source)
  expect_true("T-14-2-2" %in% n[["programs/tfl/F-14-2-2.R"]])
  expect_true("T-14-2-2" %in% n[["programs/tfl/F-14-2-3.R"]])
  # the forest plot reads its own ARD (#293 P6): made first too
  expect_true("F-14-2-4" %in% n[["programs/tfl/F-14-2-4.R"]])
  # a table reads its own ARD
  expect_true("T-14-2-2" %in% n[["programs/tfl/T-14-2-2.R"]])
  # no figure from an ARD: that figure reads none
  p0 <- set_fig_ard_source(set_fig_ard_source(set_fig_ard_source(p, "F-14-2-3", NULL),
                                                "F-14-2-4", NULL), "F-14-2-2", NULL)
  n0 <- needs_of(lines_of(batch_code(p0, root = s$path)))
  # .batch_unresolved: nothing in the sample is unresolved
  un <- eval(parse(text = grep("^[.]batch_unresolved <- ", b, value = TRUE)))
  expect_identical(un, character())
  expect_false("T-14-2-2" %in% n0[["programs/tfl/F-14-2-3.R"]])
  expect_false(any(grepl("batch_needs_hash", b, fixed = TRUE)))
})

test_that("record_report(): a listing says whether it has no rows, in the column batch.R reads (sample-01 B4)", {
  skip_on_cran()
  s <- stale_study()
  p <- s$planner
  # a listing of `data` records whether it is empty; a table does not say
  expect_true("record_report(report_id, empty = nrow(data) == 0)" %in% program_code(p, "L-16-2-7"))
  expect_true("record_report(report_id)" %in% program_code(p, "T-14-2-2"))
  # record_report() as report_setup.R has it, run in the study folder
  rec <- report_setup_code(p)
  rec <- rec[seq(grep("^record_report <- function", rec), length(rec) - 1L)]
  sf <- file.path(s$path, study_layout()[["tfl"]], .report_status_name)
  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)
  # a row an earlier record_report() wrote: no `empty` column
  utils::write.csv(data.frame(output_id = "T-14-2-2", built = "2026-11-01 10:00:00",
                              setup = "x", ard = ""), sf, row.names = FALSE)
  e <- new.env()
  withr::with_dir(s$path, {
    eval(parse(text = rec), envir = e)
    e$record_report("L-16-2-7", empty = TRUE)
    e$record_report("T-14-1-1", empty = FALSE)
    e$record_report("F-14-2-1")
  })
  st <- utils::read.csv(sf, colClasses = "character")
  expect_identical(st$empty[match(c("T-14-2-2", "L-16-2-7", "T-14-1-1", "F-14-2-1"),
                                  st$output_id)], c("", "TRUE", "FALSE", ""))
  expect_identical(st$setup[st$output_id == "T-14-2-2"], "x")
  # the column the runner reads (tflspec's batch.R): `empty`, TRUE or FALSE
  runner <- readLines(tflspec::tfl_runner_file(), warn = FALSE)
  expect_true(any(grepl("c(\"output_id\", \"empty\") %in% names(st)", runner, fixed = TRUE)))
})

test_that("the figure turns outdated with the table's ARD, and the preview and the batch make it first", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  s <- stale_study()
  # an official run of the table and the figure: the table's ARD first.  The
  # screens read the latest run (a preview or a hand run leaves the working
  # output/, which the report's state does not read)
  both <- c("T-14-2-2", "F-14-2-3")
  b1 <- run_def(s, "Both", both)
  expect_true(b1$ok)
  ard_run <- ard_status(s, "run")
  expect_identical(ard_run$state[ard_run$output_id == "T-14-2-2"], "built")
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # T-14-2-2's definition changes: its ARD is not made from it, the figure
  # is to be made again because of it
  an <- s$planner$ard$analyses
  k <- an$output_id %in% "T-14-2-2" & an$analysis_id == "KM"
  an$code[k] <- sub("150, 180)", "150)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  # the working ARD made again (a design aid's): the figure's state is its
  # run's, so it is to be made again all the same
  suppressMessages(update_study_ard(s, "T-14-2-2"))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  Sys.sleep(1.1)
  b2 <- run_def(s, "Both", both)
  expect_true(b2$ok)
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # changed again; an official run of the figure alone does not start: it
  # reads the ARD of a report the run does not select (#790)
  an$code[k] <- sub("150)", "150, 180)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  b0 <- run_def(s, "FigOnly", "F-14-2-3")
  expect_false(b0$ok)
  expect_null(b0$batch)
  expect_true(any(grepl("F-14-2-3 reads the ARD of T-14-2-2", b0$output, fixed = TRUE)))
  # with the table selected too: its ARD, then the table and the figure
  Sys.sleep(1.1)
  b <- run_def(s, "Both", both)
  expect_identical(b$result$part, c("ard", "tfl", "tfl"))
  expect_identical(b$result$program, c("programs/ard/T-14-2-2.R", "programs/tfl/T-14-2-2.R",
                                       "programs/tfl/F-14-2-3.R"))
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # nothing changed: made again all the same (a run never reads an ARD an
  # earlier run left, #790), in a run folder of its own
  Sys.sleep(1.1)
  b2 <- run_def(s, "Both", both)
  expect_identical(b2$result$program, b$result$program)
  expect_false(identical(b2$batch, b$batch))
  expect_true(dir.exists(b$batch))
})

test_that("the runs table says why", {
  expect_identical(.why_text(c("", "program", "ard:T-14-2-2 setup")),
                   c("", ": its program changed", ": T-14-2-2's ARD changed; the study setup changed"))
})

# An official run makes the ARD its reports read (#790): the ARD only, or
# the ARD then the reports; never the reports alone, and a report never
# reads an ARD an earlier run left.
test_that("an official run starts from the name of a run definition", {
  local_home()
  expect_error(run_batch(list(), "tfl"), "No run definition|Home|study", ignore.case = TRUE)
  expect_error(run_batch(list()), "Give the name of a run definition")
})

# the runner (tflspec::tfl_runner_file()) with the lists batch.R gives it, its
# programs not run: which ran, in what order
runner_env <- function() {
  e <- new.env()
  sys.source(tflspec::tfl_runner_file(), envir = e)
  e$.batch_root <- "runs"
  e$.batch_ard <- "output/ard/ard.rds"
  e$.batch_study <- "S-1"
  e$.batch_spec <- "spec"
  e$.batch_programs <- list(ard = "programs/ard/T1.R", tfl = "programs/tfl/F1.R")
  e$.batch_ard_outputs <- c(T1.R = "T1")
  e$.batch_report_ids <- c(`programs/tfl/F1.R` = "F1")
  e$.batch_needs <- list(`programs/tfl/F1.R` = "T1")
  e$.batch_unresolved <- character()
  e$.batch_runs <- list(list(name = "Main", reports = c("T1", "F1"), mode = "all",
                             copy_data = FALSE, problem = ""))
  e$.batch_outputs <- list(`programs/tfl/F1.R` = "output/tfl/F1.rtf")
  e$.batch_support <- list(ard = character(), tfl = character(), all = character())
  e$.batch_data <- list()
  e$.batch_data_vars <- character()
  e$.batch_data_dirs <- c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other")
  e$.batch_sources <- character()
  e$.batch_check_code <- character()
  e$.batch_kind <- "run"
  e$.batch_paths <- list()
  # (the stamp of the spec the programs were made from: the folder's own)
  e$.batch_stamp <- e$.batch_spec_md5()
  e$ran <- character()
  e$.batch_run <- function(part, prog, log_dir, engine) {
    e$ran <- c(e$ran, prog)
    data.frame(part = part, program = prog, status = "OK", errors = 0L, warnings = 0L,
               note = "", started = "", seconds = 0, md5 = NA_character_,
               log = file.path(log_dir, "x.log"), stringsAsFactors = FALSE)
  }
  e$.batch_session_info <- function(progs, dir) invisible(NULL)
  e$.batch_quit <- function(status) stop("the runner ended with status ", status, call. = FALSE)
  e
}


# A report selected that reads the ARD of one the run does not select: the
# run does not start (#790).  The runner quits, so it is run in its own R.
test_that("a hand run of a report that reads the ARD of a report not selected stops, and writes nothing", {
  skip_on_cran()
  d <- tempfile("runner")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("study_id: R1", file.path(d, "study.yml"))
  script <- file.path(d, "go.R")
  writeLines(c(
    paste0("sys.source(", encodeString(tflspec::tfl_runner_file(), quote = "'"),
           ", envir = globalenv())"),
    ".batch_root <- 'runs'",
    ".batch_ard <- 'output/ard/ard.rds'",
    ".batch_spec <- 'spec'",
    paste0(".batch_stamp <- '", tflspec::tfl_spec_stamp(d), "'"),
    ".batch_programs <- list(ard = c('programs/ard/T1.R', 'programs/ard/T2.R'),",
    "                        tfl = c('programs/tfl/T1.R', 'programs/tfl/L1.R'))",
    ".batch_ard_outputs <- c(T1.R = 'T1', T2.R = 'T2')",
    ".batch_report_ids <- c(`programs/tfl/T1.R` = 'T1', `programs/tfl/L1.R` = 'L1')",
    ".batch_needs <- list(`programs/tfl/T1.R` = 'T1', `programs/tfl/L1.R` = c('T1', 'T2'))",
    ".batch_runs <- list(list(name = 'Sel', reports = 'L1', mode = 'all', copy_data = FALSE, problem = ''))",
    ".batch_outputs <- list(`programs/tfl/T1.R` = 'output/tfl/T1.rtf', `programs/tfl/L1.R` = 'output/tfl/L1.rtf')",
    ".batch_support <- list(ard = character(), tfl = character(), all = character())",
    ".batch_data <- list()",
    ".batch_data_vars <- character()",
    ".batch_data_dirs <- c(adam = 'data/adam', sdtm = 'data/sdtm', other = 'data/other')",
    ".batch_sources <- character()",
    ".batch_check_code <- character()",
    ".batch_kind <- 'run'",
    ".batch_paths <- list()",
    "run_batch(c('ard', 'tfl'), commandArgs(trailingOnly = TRUE))"), script)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rscript, c(shQuote(script), "--run=Sel"),
                                  stdout = TRUE, stderr = TRUE))
  expect_gt(attr(out, "status") %||% 0L, 0L)
  expect_true(any(grepl("L1 reads the ARD of T2", out, fixed = TRUE)), info = paste(out, collapse = "\n"))
  # nothing written: no run folder, no RTF
  expect_false(dir.exists("runs"))
  expect_length(list.files(d, pattern = "[.]rtf$", recursive = TRUE), 0L)
})

test_that("a hand run of the reports alone is refused: only the runner (tfl_run_study(), the Run button) starts an official run", {
  skip_on_cran()
  d <- tempfile("runner")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("study_id: R1", file.path(d, "study.yml"))
  script <- file.path(d, "go.R")
  writeLines(c(
    paste0("sys.source(", encodeString(tflspec::tfl_runner_file(), quote = "'"),
           ", envir = globalenv())"),
    ".batch_root <- 'runs'",
    ".batch_ard <- 'output/ard/ard.rds'",
    ".batch_spec <- 'spec'",
    paste0(".batch_stamp <- '", tflspec::tfl_spec_stamp(d), "'"),
    ".batch_programs <- list(ard = 'programs/ard/T1.R', tfl = 'programs/tfl/T1.R')",
    ".batch_ard_outputs <- c(T1.R = 'T1')",
    ".batch_report_ids <- c(`programs/tfl/T1.R` = 'T1')",
    ".batch_needs <- list(`programs/tfl/T1.R` = 'T1')",
    ".batch_runs <- list(list(name = 'Main', reports = 'T1', mode = 'all', copy_data = FALSE, problem = ''))",
    ".batch_outputs <- list(`programs/tfl/T1.R` = 'output/tfl/T1.rtf')",
    ".batch_support <- list(ard = character(), tfl = character(), all = character())",
    ".batch_data <- list()",
    ".batch_data_vars <- character()",
    ".batch_data_dirs <- c(adam = 'data/adam', sdtm = 'data/sdtm', other = 'data/other')",
    ".batch_sources <- character()",
    ".batch_check_code <- character()",
    ".batch_kind <- 'run'",
    ".batch_paths <- list()",
    "run_batch('tfl', commandArgs(trailingOnly = TRUE))"), script)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rscript, c(shQuote(script), "--run=Main"), stdout = TRUE, stderr = TRUE))
  expect_gt(attr(out, "status") %||% 0L, 0L)
  expect_true(any(grepl("a run in the study starts from a run definition", out, fixed = TRUE)), info = paste(out, collapse = "\n"))
  expect_false(dir.exists("runs"))
})


# A report whose ARD program failed in the run is not run on the rows an
# earlier run left: it is recorded as failed.
test_that("the runner does not run a report whose ARD program failed", {
  d <- tempfile("runner")
  dir.create(file.path(d, "output", "ard"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  writeLines("study_id: R1", "study.yml")
  e <- runner_env()
  e$.batch_run <- function(part, prog, log_dir, engine) {
    e$ran <- c(e$ran, prog)
    data.frame(part = part, program = prog, status = if (part == "ard") "ERROR" else "OK",
               errors = 0L, warnings = 0L, note = "", started = "", seconds = 0,
               md5 = NA_character_, log = file.path(log_dir, "x.log"),
               stringsAsFactors = FALSE)
  }
  # (a run with an error ends Rscript with status 1: here it is kept)
  e$.batch_quit <- function(status) e$quit_status <- status
  utils::capture.output(r <- suppressMessages(e$run_batch(c("ard", "tfl"), "--run=Main")))
  expect_identical(e$quit_status, 1L)
  expect_identical(e$ran, "programs/ard/T1.R")
  expect_identical(r$status, c("ERROR", "ERROR"))
  expect_match(r$note[2L], "the ARD of T1 failed", fixed = TRUE)
})

# two tables with ARD analyses, and a listing
rd_planner <- function(ids = c("DM", "VS")) {
  p <- add_output(new_planner(), ids[1])
  p <- add_output(p, ids[2])
  p <- add_output(p, "L1", type = "listing", at = "end")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = ids, analysis_id = c("AGE", "AGE"),
    method = "continuous", population_id = "SAF", by = "TRT01A",
    variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

# The rules (which reads, which reasons) are tflspec's: tfl_ard_reads() has
# its own tests, rule by rule.  Here: the wrapper passes tflspec every report
# as it is on screen (unsaved edits too) and returns its answer.
test_that("the wrapper passes every report on screen and the study's spec to tflspec", {
  p <- rd_planner(c("T-1", "T-2"))
  p <- add_output(p, "F-1", type = "figure")
  got <- NULL
  testthat::local_mocked_bindings(
    tfl_ard_reads = function(study, outputs = NULL) {
      got <<- list(study = study, outputs = outputs)
      data.frame(report = c("T-2", "T-1"), reads = c("T-2", "T-1"), kind = "own",
                 reason = NA_character_, stringsAsFactors = FALSE)
    },
    .package = "tflspec")
  r <- .batch_report_reads(p)
  expect_s3_class(got$study, "tfl_study_spec")
  expect_setequal(names(got$outputs), p$outputs$output_id)
  expect_true(all(vapply(got$outputs, inherits, NA, "tfl_output")))
  # the analyses of the sheets are in the report tflspec reads
  expect_gt(length(got$outputs[["T-1"]]$ard$analyses), 0L)
  # the shape, and the report list's order
  expect_identical(names(r), c("report", "reads", "kind", "reason"))
  expect_identical(r$report, c("T-1", "T-2"))
  expect_identical(.batch_reads_needs(r), list(`T-1` = "T-1", `T-2` = "T-2"))
  expect_identical(.batch_report_needs(p), .batch_reads_needs(r))
})

test_that("a lookup that fails as a whole is every report's reason, not an error", {
  p <- rd_planner(c("T-1", "T-2"))
  testthat::local_mocked_bindings(
    tfl_ard_reads = function(study, outputs = NULL) stop("boom"),
    .package = "tflspec")
  r <- .batch_report_reads(p)
  expect_identical(names(r), c("report", "reads", "kind", "reason"))
  expect_setequal(r$report, p$outputs$output_id)
  expect_true(all(is.na(r$reads)))
  expect_true(all(r$reason == "lookup failed: boom"))
  expect_length(.batch_reads_needs(r), 0L)
  expect_setequal(names(.batch_reads_unresolved(r)), p$outputs$output_id)
})

test_that("tflspec's answer for the planner's reports: sources, analyses, import", {
  p <- rd_planner()
  p <- add_output(p, "F1", type = "figure")
  p <- set_fig_ard_source(p, "F1", "table:DM")
  # the default design does not read an ARD: the figure still needs the table
  r <- .batch_report_reads(p)
  expect_identical(r$reads[r$report == "F1"], "DM")
  expect_identical(r$kind[r$report == "F1"], "source")
  expect_identical(.batch_reads_needs(r)[["F1"]], "DM")
  expect_length(.batch_reads_unresolved(r), 0L)
  # a table with no analyses
  p <- add_output(p, "T9")
  p <- add_output(p, "F2", type = "figure")
  p <- set_fig_ard_source(p, "F2", "table:T9")
  r <- .batch_report_reads(p)
  expect_identical(.batch_reads_unresolved(r), c(F2 = "source not in analyses"))
  expect_false("F2" %in% names(.batch_reads_needs(r)))
  expect_true(is.na(r$reads[r$report == "F2"]))
  # a source that names no report
  p$sheets$report$ard_source[p$sheets$report$output_id == "F2"] <- "table:GONE"
  expect_identical(.batch_reads_unresolved(.batch_report_reads(p)),
                   c(F2 = "source report not found"))
  # no source, and a design that reads no ARD: no row
  p <- set_fig_ard_source(p, "F2", NULL)
  expect_length(.batch_reads_unresolved(.batch_report_reads(p)), 0L)
  # an import reads no report's ARD: neither a need nor unresolved (tflspec
  # gives it no row)
  p$sheets$report$ard_source[p$sheets$report$output_id == "F2"] <- "import:x.rds"
  r <- .batch_report_reads(p)
  expect_false("F2" %in% r$report)
  expect_length(.batch_reads_unresolved(r), 0L)
  expect_false("F2" %in% names(.batch_reads_needs(r)))
})

test_that("the on-screen reports are read, not the saved ones", {
  skip_on_cran()
  s <- stale_study()
  p <- s$planner
  # as saved: the sample figure reads its table's ARD
  r <- .batch_report_reads(p)
  expect_identical(.batch_reads_needs(r)[["F-14-2-3"]], "T-14-2-2")
  # unsaved: a new table with analyses, and the figure's source moved to it
  a <- p$ard$analyses
  row <- a[a$output_id == "T-14-2-2", , drop = FALSE][1L, , drop = FALSE]
  row$output_id <- "T-99"
  row$analysis_id <- "NEW1"
  p <- add_output(p, "T-99")
  p$ard$analyses <- rbind(a, row)
  p <- set_fig_ard_source(p, "F-14-2-3", "table:T-99")
  r <- .batch_report_reads(p)
  expect_identical(.batch_reads_needs(r)[["F-14-2-3"]], "T-99")
  expect_identical(.batch_reads_needs(r)[["T-99"]], "T-99")
  expect_length(.batch_reads_unresolved(r), 0L)
  # the saved study is as it was (the wrapper did not read it)
  expect_identical(.batch_reads_needs(.batch_report_reads(s$planner))[["F-14-2-3"]], "T-14-2-2")
  # a report taken away: a source naming it is not found
  q <- set_fig_ard_source(p, "F-14-2-3", NULL)
  q$sheets$report$ard_source[q$sheets$report$output_id == "F-14-2-3"] <- "table:T-GONE"
  expect_identical(.batch_reads_unresolved(.batch_report_reads(q))[["F-14-2-3"]],
                   "source report not found")
})

test_that("batch_code writes .batch_unresolved, report program -> reason", {
  p <- rd_planner()
  p <- add_output(p, "T9")
  p <- add_output(p, "F2", type = "figure")
  p <- set_fig_ard_source(p, "F2", "table:T9")
  b <- unlist(strsplit(batch_code(p), "\n", fixed = TRUE))
  i <- grep("^[.]batch_unresolved <- ", b)
  j <- grep("^# the run definitions", b)
  un <- eval(parse(text = paste(b[i:(j - 1L)], collapse = "\n")))
  expect_identical(un, c(`programs/tfl/F2.R` = "source not in analyses"))
  expect_true(any(grepl("^# a report program -> why the ARD it reads cannot be worked out", b)))
})

# the runner with two outputs, an ARD program and a report program each
two_env <- function() {
  e <- runner_env()
  e$.batch_programs <- list(ard = c("programs/ard/T1.R", "programs/ard/T2.R"),
                            tfl = c("programs/tfl/T1.R", "programs/tfl/T2.R"))
  e$.batch_ard_outputs <- c(T1.R = "T1", T2.R = "T2")
  e$.batch_report_ids <- c(`programs/tfl/T1.R` = "T1", `programs/tfl/T2.R` = "T2")
  e$.batch_needs <- list(`programs/tfl/T1.R` = "T1", `programs/tfl/T2.R` = "T2")
  e$.batch_outputs <- list(`programs/tfl/T1.R` = "output/tfl/T1.rtf",
                           `programs/tfl/T2.R` = "output/tfl/T2.rtf")
  e
}
in_study <- function(env = parent.frame()) {
  d <- tempfile("runner")
  dir.create(d)
  old <- setwd(d)
  withr::defer(setwd(old), env)
  withr::defer(unlink(d, recursive = TRUE), env)
  writeLines("study_id: R1", "study.yml")
  d
}





test_that(".batch_missing_needs names the selected reports that read an ARD not selected", {
  needs <- list(`T-1` = "T-1", `F-1` = c("T-1", "T-2"), `L-1` = "T-3")
  # a figure reads its table's ARD; a table reads its own
  m <- .batch_missing_needs(needs, c("T-1", "F-1", "L-1"))
  expect_identical(m, data.frame(report = c("F-1", "L-1"), need = c("T-2", "T-3"),
                                 stringsAsFactors = FALSE))
  expect_identical(.batch_missing_needs(needs, "F-1")$need, c("T-1", "T-2"))
  # a table whose own ARD is not selected is not selected itself: no row
  expect_identical(.batch_missing_needs(list(`T-9` = "T-9"), "T-1")$report, character())
  # none missing: no rows
  m0 <- .batch_missing_needs(needs, c("T-1", "T-2", "T-3", "F-1", "L-1"))
  expect_identical(nrow(m0), 0L)
  expect_identical(names(m0), c("report", "need"))
})


# (the runner's own behaviour inside its batch folder -- the ARD before the
# reports, the names and --exclude, the same-second folders -- is tested in
# tflspec's test-runner-run.R with real Rscript children: the hand-made
# in-process runners that stood here cannot stage a folder.)
