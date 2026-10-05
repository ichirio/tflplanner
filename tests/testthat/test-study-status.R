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
  k <- which(p2$outputs$output_id == "T-14-1-2")
  p2$outputs$data_code[k] <- "adsl <- adsl[adsl$SAFFL == \"Y\", ]"
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
