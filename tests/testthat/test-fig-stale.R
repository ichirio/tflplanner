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
  expect_true(any(grepl("record_report <- function(output_id, ard = \"\")", rs, fixed = TRUE)))
  # (one element a statement: a line each)
  lines_of <- function(x) unlist(strsplit(x, "\n", fixed = TRUE))
  b <- lines_of(batch_code(p, root = s$path))
  # .batch_needs: a report program -> the reports whose ARD it reads (#790)
  needs_of <- function(lines) {
    i <- grep("^[.]batch_needs <- ", lines)
    j <- grep("^# the named batches", lines)
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
  expect_false("T-14-2-2" %in% n0[["programs/tfl/F-14-2-3.R"]])
  expect_false(any(grepl("batch_needs_hash", b, fixed = TRUE)))
})

test_that("the figure turns outdated with the table's ARD, and the preview and the batch make it first", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  s <- stale_study()
  # the preview: T-14-2-2's ARD made first (it is not made yet)
  run_study(s, "F-14-2-3")
  expect_identical(ard_status(s)$state[ard_status(s)$output_id == "T-14-2-2"], "built")
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # T-14-2-2's definition changes: its ARD is not made from it, the figure
  # is to be made again because of it
  an <- s$planner$ard$analyses
  k <- an$output_id %in% "T-14-2-2" & an$analysis_id == "KM"
  an$code[k] <- sub("150, 180)", "150)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  # the ARD made again: the figure was made from the old one
  suppressMessages(update_study_ard(s, "T-14-2-2"))
  expect_identical(fig_state(s), list(status = "outdated", why = "ard:T-14-2-2"))
  run_study(s, "F-14-2-3")
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # changed again; an official run of the figure alone does not start: it
  # reads the ARD of a report the run does not select (#790)
  an$code[k] <- sub("150)", "150, 180)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  b0 <- run_batch(s, c("ard", "tfl"), only = "F-14-2-3", code = FALSE)
  expect_false(b0$ok)
  expect_null(b0$batch)
  expect_true(any(grepl("F-14-2-3 reads the ARD of T-14-2-2", b0$output, fixed = TRUE)))
  # with the table selected too: its ARD, then the table and the figure
  b <- run_batch(s, c("ard", "tfl"), only = c("T-14-2-2", "F-14-2-3"), code = FALSE)
  expect_identical(b$result$part, c("ard", "tfl", "tfl"))
  expect_identical(b$result$program, c("programs/ard/T-14-2-2.R", "programs/tfl/T-14-2-2.R",
                                       "programs/tfl/F-14-2-3.R"))
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # nothing changed: made again all the same (a run never reads an ARD an
  # earlier run left, #790), in a run folder of its own
  Sys.sleep(1.1)
  b2 <- run_batch(s, c("ard", "tfl"), only = c("T-14-2-2", "F-14-2-3"), code = FALSE)
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
test_that("an official run is the ARD only, or the ARD then the reports", {
  expect_error(run_batch(list(), "tfl"), "the ARD only (parts = \"ard\")", fixed = TRUE)
})

# the runner (inst/batch/runner.R) with the lists batch.R gives it, its
# programs not run: which ran, in what order
runner_env <- function() {
  e <- new.env()
  sys.source(system.file("batch", "runner.R", package = "tflplanner"), envir = e)
  e$.batch_root <- "runs"
  e$.batch_ard <- "output/ard/ard.rds"
  e$.batch_programs <- list(ard = "programs/ard/T1.R", tfl = "programs/tfl/F1.R")
  e$.batch_ard_outputs <- c(T1.R = "T1")
  e$.batch_report_ids <- c(`programs/tfl/F1.R` = "F1")
  e$.batch_needs <- list(`programs/tfl/F1.R` = "T1")
  e$.batch_sets <- list()
  e$.batch_outputs <- list(`programs/tfl/F1.R` = "output/tfl/F1.rtf")
  e$.batch_support <- list(ard = character(), tfl = character(), all = character())
  e$.batch_data <- list()
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

test_that("the runner makes the ARD before the reports, even when one is built", {
  d <- tempfile("runner")
  dir.create(file.path(d, "output", "ard"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  writeLines("study_id: R1", "study.yml")
  # T1's ARD made by an earlier run, from the definition as it is now
  utils::write.csv(data.frame(output_id = "T1", definition = "abc",
                              built = "2026-11-01 10:00:00", rows = "1", error = ""),
                   file.path("output", "ard", "ard_status.csv"), row.names = FALSE)
  e <- runner_env()
  # the ARD, then the reports: T1's ARD program first, once
  utils::capture.output(r <- suppressMessages(e$run_batch(c("ard", "tfl"))))
  expect_identical(e$ran, c("programs/ard/T1.R", "programs/tfl/F1.R"))
  expect_identical(r$part, c("ard", "tfl"))
  # the figure with the table it reads (each its own run folder)
  e$ran <- character()
  Sys.sleep(1.1)
  utils::capture.output(r2 <- suppressMessages(e$run_batch(c("ard", "tfl"), c("T1", "F1"))))
  expect_identical(e$ran, c("programs/ard/T1.R", "programs/tfl/F1.R"))
  expect_identical(r2$part, c("ard", "tfl"))
  expect_length(list.files("runs"), 2L)
})

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
    paste0("sys.source(", encodeString(system.file("batch", "runner.R", package = "tflplanner"), quote = "'"),
           ", envir = globalenv())"),
    ".batch_root <- 'runs'",
    ".batch_ard <- 'output/ard/ard.rds'",
    ".batch_programs <- list(ard = c('programs/ard/T1.R', 'programs/ard/T2.R'),",
    "                        tfl = c('programs/tfl/T1.R', 'programs/tfl/L1.R'))",
    ".batch_ard_outputs <- c(T1.R = 'T1', T2.R = 'T2')",
    ".batch_report_ids <- c(`programs/tfl/T1.R` = 'T1', `programs/tfl/L1.R` = 'L1')",
    ".batch_needs <- list(`programs/tfl/T1.R` = 'T1', `programs/tfl/L1.R` = c('T1', 'T2'))",
    ".batch_sets <- list()",
    ".batch_outputs <- list(`programs/tfl/T1.R` = 'output/tfl/T1.rtf', `programs/tfl/L1.R` = 'output/tfl/L1.rtf')",
    ".batch_support <- list(ard = character(), tfl = character(), all = character())",
    ".batch_data <- list()",
    "run_batch(c('ard', 'tfl'), commandArgs(trailingOnly = TRUE))"), script)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rscript, c(shQuote(script), "--exclude=T2"),
                                  stdout = TRUE, stderr = TRUE,
                                  env = paste0("RUNNER=", shQuote(system.file("batch", "runner.R", package = "tflplanner")))))
  expect_gt(attr(out, "status") %||% 0L, 0L)
  expect_true(any(grepl("L1 reads the ARD of T2", out, fixed = TRUE)), info = paste(out, collapse = "
"))
  # nothing written: no run folder, no RTF
  expect_false(dir.exists("runs"))
  expect_length(list.files(d, pattern = "[.]rtf$", recursive = TRUE), 0L)
})

test_that("a hand run of the reports alone stops: their ARD is not made in this run", {
  skip_on_cran()
  d <- tempfile("runner")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("study_id: R1", file.path(d, "study.yml"))
  script <- file.path(d, "go.R")
  writeLines(c(
    paste0("sys.source(", encodeString(system.file("batch", "runner.R", package = "tflplanner"), quote = "'"),
           ", envir = globalenv())"),
    ".batch_root <- 'runs'",
    ".batch_ard <- 'output/ard/ard.rds'",
    ".batch_programs <- list(ard = 'programs/ard/T1.R', tfl = 'programs/tfl/T1.R')",
    ".batch_ard_outputs <- c(T1.R = 'T1')",
    ".batch_report_ids <- c(`programs/tfl/T1.R` = 'T1')",
    ".batch_needs <- list(`programs/tfl/T1.R` = 'T1')",
    ".batch_sets <- list()",
    ".batch_outputs <- list(`programs/tfl/T1.R` = 'output/tfl/T1.rtf')",
    ".batch_support <- list(ard = character(), tfl = character(), all = character())",
    ".batch_data <- list()",
    "run_batch('tfl', commandArgs(trailingOnly = TRUE))"), script)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rscript, shQuote(script), stdout = TRUE, stderr = TRUE))
  expect_gt(attr(out, "status") %||% 0L, 0L)
  expect_true(any(grepl("T1 reads the ARD of T1", out, fixed = TRUE)), info = paste(out, collapse = "
"))
  expect_false(dir.exists("runs"))
})

# A run that selects both reports starts as before.
test_that("a run that selects the report and the report it reads runs both", {
  d <- tempfile("runner")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  writeLines("study_id: R1", "study.yml")
  e <- runner_env()
  e$.batch_programs <- list(ard = c("programs/ard/T1.R", "programs/ard/T2.R"),
                            tfl = c("programs/tfl/T1.R", "programs/tfl/L1.R"))
  e$.batch_ard_outputs <- c(T1.R = "T1", T2.R = "T2")
  e$.batch_report_ids <- c(`programs/tfl/T1.R` = "T1", `programs/tfl/L1.R` = "L1")
  e$.batch_needs <- list(`programs/tfl/T1.R` = "T1", `programs/tfl/L1.R` = c("T1", "T2"))
  e$.batch_outputs <- list(`programs/tfl/T1.R` = "output/tfl/T1.rtf",
                           `programs/tfl/L1.R` = "output/tfl/L1.rtf")
  # T2 is not a report of the run, but L1 reads it: ticked (T2.R is the name
  # of its ARD program too)
  utils::capture.output(r <- suppressMessages(e$run_batch(c("ard", "tfl"), c("T1", "L1", "T2"))))
  expect_identical(e$ran, c("programs/ard/T1.R", "programs/ard/T2.R",
                            "programs/tfl/T1.R", "programs/tfl/L1.R"))
  bt <- readLines(file.path("runs", list.files("runs")[1L], "batch.txt"))
  expect_true("Excluded : none" %in% bt)
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
  utils::capture.output(r <- suppressMessages(e$run_batch(c("ard", "tfl"))))
  expect_identical(e$quit_status, 1L)
  expect_identical(e$ran, "programs/ard/T1.R")
  expect_identical(r$status, c("ERROR", "ERROR"))
  expect_match(r$note[2L], "the ARD of T1 failed", fixed = TRUE)
})

test_that("a figure whose ARD source cannot be worked out stops the run, naming the figure", {
  p <- add_output(new_planner(), "F-1", type = "figure")
  testthat::local_mocked_bindings(.fig_ard_need = function(x, output_id) stop("boom"))
  expect_error(.batch_report_needs(p), "figure F-1 reads could not be found out: boom")
})

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

test_that("two runs in the same second get two run folders, and the runs table lists both", {
  d <- tempfile("runner")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  writeLines("study_id: R1", "study.yml")
  e <- runner_env()
  # the clock stands still
  e$Sys.time <- function() as.POSIXct("2026-11-02 10:00:00", tz = "UTC")
  for (i in 1:3) {
    utils::capture.output(suppressMessages(e$run_batch(c("ard", "tfl"))))
  }
  expect_setequal(list.files("runs"), c("20261102_100000_all", "20261102_100000_all_2",
                                        "20261102_100000_all_3"))
  lb <- list_batches(list(path = d))
  expect_setequal(lb$batch, list.files("runs"))
  expect_true(all(lb$what == "all"))
  expect_true(all(lb$started == lb$started[1L]))
  expect_true(all(lb$state == "done"))
})
