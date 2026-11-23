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
  i <- grep("^[.]batch_needs <- ", b)
  # (F-14-2-2's own code reads T-14-2-2's ARD too, as its ARD source)
  expect_identical(b[i + 1L], "  `programs/tfl/F-14-2-2.R` = \"T-14-2-2\",")
  expect_identical(b[i + 2L], "  `programs/tfl/F-14-2-3.R` = \"T-14-2-2\",")
  # the forest plot reads its own ARD (#293 P6): made first too
  expect_identical(b[i + 3L], "  `programs/tfl/F-14-2-4.R` = \"F-14-2-4\")")
  h <- grep("^[.]batch_needs_hash <- ", b)
  expect_match(b[h + 1L], "^  `T-14-2-2` = \"[0-9a-f]{32}\",$")
  expect_match(b[h + 2L], "^  `F-14-2-4` = \"[0-9a-f]{32}\"[)]$")
  # no figure from an ARD (the forest plot's own taken away too): empty
  p0 <- set_fig_ard_source(set_fig_ard_source(set_fig_ard_source(p, "F-14-2-3", NULL),
                                                "F-14-2-4", NULL), "F-14-2-2", NULL)
  b0 <- lines_of(batch_code(p0, root = s$path))
  expect_true(".batch_needs <- character()" %in% b0)
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
  # changed again; an official run of the figure alone makes the ARD first
  an$code[k] <- sub("150)", "150, 180)", an$code[k], fixed = TRUE)
  s$planner$ard$analyses <- an
  s <- suppressMessages(save_study(s))
  b <- run_batch(s, c("ard", "tfl"), only = "F-14-2-3", code = FALSE)
  expect_identical(b$result$part, c("ard", "tfl"))
  expect_identical(b$result$program, c("programs/ard/T-14-2-2.R", "programs/tfl/F-14-2-3.R"))
  expect_true(any(grepl("made first", b$output, fixed = TRUE)))
  expect_identical(fig_state(s), list(status = "ok", why = ""))
  # nothing changed: made again all the same (a run never reads an ARD an
  # earlier run left, #790), in a run folder of its own
  Sys.sleep(1.1)
  b2 <- run_batch(s, c("ard", "tfl"), only = "F-14-2-3", code = FALSE)
  expect_identical(b2$result$program, c("programs/ard/T-14-2-2.R", "programs/tfl/F-14-2-3.R"))
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
  e$.batch_needs <- c(`programs/tfl/F1.R` = "T1")
  e$.batch_needs_hash <- c(T1 = "abc")
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
  # the figure alone: the ARD it reads made first in the same run
  e$ran <- character()
  Sys.sleep(1.1)
  utils::capture.output(r2 <- suppressMessages(e$run_batch(c("ard", "tfl"), "F1")))
  expect_identical(e$ran, c("programs/ard/T1.R", "programs/tfl/F1.R"))
  expect_identical(r2$part, c("ard", "tfl"))
  # each run its own run folder; the earlier one kept
  expect_length(list.files("runs"), 2L)
})
