# study_status() with the spec and the programs kept per definition
# (R/programs.R .spec_object_last(), R/study.R .program_code_last()): the
# same answers as without them, a changed definition seen at once

test_that("study_status() says the same with its caches as without them", {
  skip_on_cran()
  home <- local_home()
  s <- .make_big_study(24L, root = file.path(home, "ws"), home = home, study_id = "ST-24")
  fresh <- function() {
    rm(list = ls(.spec_cache), envir = .spec_cache)
    rm(list = ls(.program_cache), envir = .program_cache)
  }
  fresh()
  a <- study_status(s)
  b <- study_status(s)  # from the caches
  expect_identical(a, b)
  # each report's program as written without any cache
  for (id in output_ids(s$planner)) {
    fresh()
    expect_identical(.program_code_last(s$planner, id), program_code(s$planner, id), info = id)
  }
  # a definition changed (not saved): that report's program is to write
  # again, the others are as they were
  p2 <- s$planner
  p2 <- set_step_code(p2, "T-14-1-2", "data", "adsl <- adsl[adsl$SAFFL == \"Y\", ]")
  s2 <- s
  s2$planner <- p2
  c2 <- study_status(s2)
  expect_identical(c2$status[c2$output_id == "T-14-1-2"], "unsaved")
  same <- c2$output_id != "T-14-1-2"
  expect_identical(c2$status[same], a$status[same])
  # and back: as before
  expect_identical(study_status(s), a)
})

test_that("the report list's quick look agrees with study_status()", {
  skip_on_cran()
  home <- local_home()
  s <- .make_big_study(40L, root = file.path(home, "ws"), home = home, study_id = "ST-40")
  full <- study_status(s)
  light <- .report_run_light(s)
  expect_identical(light$output_id, full$output_id)
  # what the files say: made, failed, not run, made before a change
  files <- full$status %in% c("ok", "error", "not run", "outdated")
  expect_true(all(files))
  expect_identical(light$status, full$status)
  expect_true(all(c("ok", "error", "not run") %in% light$status))
})

test_that("a log that cannot be read now is 'not ready yet', quietly", {
  d <- withr::local_tempdir()
  # a log not there; a folder where the log is (Windows says "Permission
  # denied" for it, as for a log its program still holds)
  expect_no_warning(x <- .read_log_lines(file.path(d, "none.log")))
  expect_null(x)
  expect_no_warning(x <- .read_log_lines(d))
  expect_null(x)
  expect_false(.log_failed(file.path(d, "none.log")))
  expect_false(.log_failed(d))
  # read for real once it can be: the lines, and an error in them
  f <- file.path(d, "ok.log")
  writeLines(c("a line", "done"), f)
  expect_identical(.read_log_lines(f), c("a line", "done"))
  expect_false(.log_failed(f))
  g <- file.path(d, "bad.log")
  writeLines(c("a line", "Error in f(): boom", "Execution halted"), g)
  expect_true(.log_failed(g))
})

test_that("a preview log held open by its program is not read as failed, then is read", {
  skip_on_os(c("mac", "linux", "solaris"))
  d <- withr::local_tempdir()
  f <- file.path(d, "held.log")
  # the way a child R holds its log: opened for writing, exclusive
  ps <- file.path(d, "hold.R")
  writeLines(c(sprintf("con <- file(%s, 'w')", encodeString(f, quote = "\"")),
               "writeLines(c('Error in f(): boom', 'Execution halted'), con)",
               "flush(con)", "Sys.sleep(3)"), ps)
  px <- processx::process$new(file.path(R.home("bin"), "Rscript"), ps,
                              stdout = NULL, stderr = NULL, cleanup = TRUE)
  on.exit(px$kill(), add = TRUE)
  for (i in 1:40) if (file.exists(f) && file.size(f) > 0L || !px$is_alive()) break else Sys.sleep(0.25)
  # (whatever the system does with a held file, no warning comes out)
  expect_no_warning(.log_failed(f))
  px$wait()
  expect_true(.log_failed(f))
})
