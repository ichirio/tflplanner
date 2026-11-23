# The screens read the latest official run (G4, slice A''): a report's state,
# its RTF, the report list's marks and the ARD state the list shows come
# from the batch folders of runs/ (R/latest_run.R); the working output/ of
# previews and hand runs is the design aids'.  The runs here are fake: the
# files a runner leaves (helper-runs.R), nothing is run.

lr_study <- function(id = "LR") {
  p <- add_output(new_planner(), "DM", process_code = "x <- 1")
  p <- add_output(p, "VS", process_code = "x <- 1")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", path = "data/adam/adsl.rds"), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    "populations")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = c("DM", "VS"), analysis_id = c("AGE", "SEX"),
    method = c("continuous", "categorical"), population_id = "SAF",
    by = "TRT01A", variables = c("AGE", "SEX")), "analyses")
  create_study(id, planner = p)
}

# an ARD's record as a run (or a hand run) leaves it, for one output
lr_ard_row <- function(s, id, definition = .ard_hash_now(s, id), error = "") {
  data.frame(output_id = id, definition = definition,
             built = "2026-01-01 10:00:00", rows = 10L, error = error,
             setup = .study_setup_hash(s$path), stringsAsFactors = FALSE)
}

light <- function(s) stats::setNames(.report_run_light(s)$status, s$planner$outputs$output_id)

test_that("a report is not run until an official run made it; the RTF is the run's", {
  local_home()
  s <- lr_study("LR1")
  expect_identical(light(s), c(DM = "not run", VS = "not run"))
  # a hand run's working RTF is not a run
  f <- file.path(s$path, "output", "tfl", "DM.rtf")
  dir.create(dirname(f), recursive = TRUE)
  writeLines("{\\rtf1 }", f)
  expect_identical(light(s), c(DM = "not run", VS = "not run"))
  d <- fake_run(s$path, "20260101_100000_all", "tfl", "DM")
  expect_identical(light(s), c(DM = "ok", VS = "not run"))
  st <- study_status(s)
  k <- match("DM", st$output_id)
  expect_identical(st$rtf_path[k], file.path(d, report_info(s$planner, "DM")$file))
  expect_true(file.exists(st$rtf_path[k]))
  expect_false(is.na(st$rtf[k]))
  expect_true(is.na(st$rtf_path[match("VS", st$output_id)]))
  expect_true(is.na(st$rtf[match("VS", st$output_id)]))
})

test_that("a later failed run is an error, with its log; a later good run clears it", {
  local_home()
  s <- lr_study("LR2")
  t0 <- Sys.time()
  fake_run(s$path, "20260101_100000_all", "tfl", c("DM", "VS"), finished = t0 - 300)
  expect_identical(light(s), c(DM = "ok", VS = "ok"))
  # a newer run where DM failed: DM is an error, VS stays the first run's
  fake_run(s$path, "20260102_100000_all", "tfl", "DM", status = "ERROR",
           finished = t0 - 200)
  expect_identical(light(s), c(DM = "error", VS = "ok"))
  r <- .report_run_state(s)
  expect_true(file.exists(r$log[1L]))
  expect_match(paste(readLines(r$log[1L]), collapse = "\n"), "made-up failure")
  # the RTF it shows is still the good run's
  expect_false(is.na(r$run_path[1L]))
  # a newer run that made it: ok again
  fake_run(s$path, "20260103_100000_all", "tfl", "DM", finished = t0 - 100)
  expect_identical(light(s), c(DM = "ok", VS = "ok"))
  # a failure older than the last good run does not count
  fake_run(s$path, "20251231_100000_all", "tfl", "VS", status = "ERROR",
           finished = t0 - 400)
  expect_identical(light(s), c(DM = "ok", VS = "ok"))
})

test_that("a report is outdated when its program is not the one the run ran", {
  local_home()
  s <- lr_study("LR3")
  fake_run(s$path, "20260101_100000_all", "tfl", "DM")
  expect_identical(light(s)[["DM"]], "ok")
  f <- file.path(s$path, "programs", "tfl", "DM.R")
  writeLines(c(readLines(f), "# changed after the run"), f)
  expect_identical(light(s)[["DM"]], "outdated")
  r <- .report_run_state(s)
  expect_identical(r$why[r$output_id == "DM"], "program")
  # a new run of it: ok
  fake_run(s$path, "20260102_100000_all", "tfl", "DM", finished = Sys.time() + 5)
  expect_identical(light(s)[["DM"]], "ok")
})

test_that("a failed preview sets preview_error, not the status", {
  local_home()
  s <- lr_study("LR4")
  fake_run(s$path, "20260101_100000_all", "tfl", "DM")
  log <- file.path(s$path, study_layout()[["logs_preview"]], "DM.log")
  dir.create(dirname(log), recursive = TRUE)
  writeLines(c("Error in eval(expr): a made-up failure", "Execution halted"), log)
  st <- study_status(s)
  expect_identical(st$preview_error[match(c("DM", "VS"), st$output_id)], c(TRUE, FALSE))
  expect_false(identical(st$status[match("DM", st$output_id)], "error"))
  l <- .report_run_light(s)
  expect_identical(l$status, c("ok", "not run"))
  expect_identical(l$preview_error, c(TRUE, FALSE))
  # the list shows it in the row's tooltip only: the state is the run's
  d <- .report_rows(s$planner, NULL, l)
  expect_identical(d$state, c("ok", "not built"))
  expect_identical(d$preview_error, c(TRUE, FALSE))
})

test_that("a run folder of another study does not count", {
  local_home()
  s <- lr_study("LR5")
  fake_run(s$path, "20260101_100000_all", "tfl", c("DM", "VS"), study_id = "SOMEONE-ELSE")
  expect_identical(light(s), c(DM = "not run", VS = "not run"))
  expect_identical(nrow(.run_index(s)), 0L)
  fake_run(s$path, "20260102_100000_all", "tfl", "DM")
  expect_identical(light(s), c(DM = "ok", VS = "not run"))
  expect_identical(nrow(.run_index(s)), 1L)
})

test_that("ard_status(from = 'run') reads the latest runs; 'working' the working ARD", {
  local_home()
  s <- lr_study("LR6")
  t0 <- Sys.time()
  state <- function(from = "working") {
    a <- ard_status(s, from)
    stats::setNames(a$state, a$output_id)
  }
  expect_identical(state("working"), c(DM = "not built", VS = "not built"))
  expect_identical(state("run"), c(DM = "not built", VS = "not built"))
  # a run made DM's ARD (and VS's, from an earlier definition)
  fake_run(s$path, "20260101_100000_all", "ard", c("DM", "VS"), finished = t0 - 300,
           ard_status = rbind(lr_ard_row(s, "DM"),
                              lr_ard_row(s, "VS", definition = "an earlier definition")))
  expect_identical(state("run"), c(DM = "built", VS = "outdated"))
  # (the working ARD is not read by it, nor the run by the working)
  expect_identical(state("working"), c(DM = "not built", VS = "not built"))
  expect_identical(state(), state("working"))
  # a later run where DM's ARD program failed: an error with its note
  fake_run(s$path, "20260102_100000_all", "ard", "DM", status = "ERROR",
           note = "Error: boom", finished = t0 - 200)
  a <- ard_status(s, "run")
  expect_identical(a$state[a$output_id == "DM"], "error")
  expect_identical(a$error[a$output_id == "DM"], "Error: boom")
  # a later good run: built again
  fake_run(s$path, "20260103_100000_all", "ard", "DM", finished = t0 - 100,
           ard_status = lr_ard_row(s, "DM"))
  expect_identical(state("run"), c(DM = "built", VS = "outdated"))
  # the report list's marks are made from the runs
  m <- .report_marks(s$planner, s)
  expect_identical(m[["DM"]], "not built")   # (its report is not run)
  # a run that failed without a note: "failed"
  fake_run(s$path, "20260104_100000_all", "ard", "VS", status = "ERROR", finished = t0 - 50)
  a <- ard_status(s, "run")
  expect_identical(a$error[a$output_id == "VS"], "failed")
  expect_identical(a$state[a$output_id == "VS"], "error")
})

test_that(".fig_ard_why reads the figure's own run folder, the working files without roots", {
  local_home()
  s <- lr_study("LR7")
  testthat::local_mocked_bindings(
    .fig_ard_need = function(p, output_id) "DM")
  h <- .ard_hash_now(s, "DM")
  t0 <- Sys.time()
  # F1's run made DM's ARD from the definition now, F2's from an earlier one
  a <- fake_run(s$path, "20260101_100000_all", "tfl", "F1", finished = t0 - 200,
                ard_status = lr_ard_row(s, "DM"),
                report_status = data.frame(output_id = "F1", ard = h, setup = ""))
  b <- fake_run(s$path, "20260102_100000_all", "tfl", "F2", finished = t0 - 100,
                ard_status = lr_ard_row(s, "DM", definition = "an earlier definition"),
                report_status = data.frame(output_id = "F2", ard = h, setup = ""))
  w <- .fig_ard_why(s, c("F1", "F2"), roots = c(F1 = a, F2 = b))
  expect_identical(unname(w), c("", "ard:DM"))
  # a figure with no run: nothing to say about it
  w <- .fig_ard_why(s, c("F1", "F3"), roots = c(F1 = a, F3 = NA_character_))
  expect_identical(unname(w), c("", ""))
  # the figure was made from another definition than the one now
  d <- fake_run(s$path, "20260103_100000_all", "tfl", "F4", finished = t0 - 50,
                ard_status = lr_ard_row(s, "DM"),
                report_status = data.frame(output_id = "F4", ard = "an earlier one", setup = ""))
  expect_identical(unname(.fig_ard_why(s, "F4", roots = c(F4 = d))), "ard:DM")
  # no roots: the working files (the builder's and the preview's), none made
  expect_identical(unname(.fig_ard_why(s, c("F1", "F2"))), c("ard:DM", "ard:DM"))
})

test_that("study_ard_rows falls back to the run's rows, read only, when they match the definition", {
  local_home()
  s <- lr_study("LR8")
  t0 <- Sys.time()
  work <- file.path(s$path, "output", "ard", "ard.rds")
  # no run, no working ARD
  expect_null(study_ard_rows(s, "DM"))
  d <- fake_run(s$path, "20260101_100000_all", "ard", "DM", finished = t0 - 100,
                ard_status = lr_ard_row(s, "DM"))
  saveRDS(data.frame(output_id = "DM", stat = c(1, 2, 3)), file.path(d, "output", "ard", "ard.rds"))
  r <- study_ard_rows(s, "DM")
  expect_identical(r$stat, c(1, 2, 3))
  # read only: nothing was copied into output/
  expect_false(file.exists(work))
  expect_false(file.exists(file.path(s$path, "output", "ard", "ard_status.csv")))
  # the definition has moved on since the run: no rows from it
  s2 <- s
  s2$planner$ard$analyses$statistics[1L] <- "N | mean"
  expect_null(study_ard_rows(s2, "DM"))
  # the working ARD, made from the definition now, is the one read
  dir.create(dirname(work), recursive = TRUE)
  saveRDS(data.frame(output_id = "DM", stat = 9), work)
  .write_ard_status(s, lr_ard_row(s, "DM"))
  expect_identical(study_ard_rows(s, "DM")$stat, 9)
  # the working ARD of another definition: the run's rows, which match
  .write_ard_status(s, lr_ard_row(s, "DM", definition = "an earlier definition"))
  expect_identical(study_ard_rows(s, "DM")$stat, c(1, 2, 3))
  # neither matches: the working rows as they are
  expect_identical(study_ard_rows(s2, "DM")$stat, 9)
})

test_that("the stamp of the runs changes when a run's batch.txt appears", {
  local_home()
  s <- lr_study("LR9")
  before <- .run_files_stamp(s$path)
  expect_identical(before[["done"]], "")
  i0 <- nrow(.run_index(s))
  fake_run(s$path, "20260101_100000_all", "tfl", "DM")
  after <- .run_files_stamp(s$path)
  expect_false(identical(after[["done"]], before[["done"]]))
  expect_match(after[["done"]], "^1 ")
  # the index is read again then, and not before
  expect_identical(nrow(.run_index(s)), i0 + 1L)
  expect_identical(.run_index(s), .run_index(s))
})
