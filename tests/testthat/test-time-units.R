# The days in a week, a month, a year: named constants the company
# standards set (study_setup.R part 1), the figure programs divide by.

test_that("the built-in standards set the days in a week, a month, a year", {
  code <- setup_code(standards = .builtin_standards())
  expect_true(all(c("days_per_week <- 7", "days_per_month <- 30.4375",
                    "days_per_year <- 365.25") %in% code))
})

test_that("a study's values are its setup's (the last one set), else tflspec's", {
  d <- withr_tempdir()
  expect_identical(.study_time_units(d), tflspec::tfl_time_unit_days())
  f <- file.path(d, .study_setup_path())
  dir.create(dirname(f), recursive = TRUE)
  writeLines(c("days_per_month <- 30.4375", "days_per_month <- 30  # ours",
               "x <- days_per_year"), f)
  u <- .study_time_units(d)
  expect_identical(u[["days_per_month"]], 30)
  expect_identical(u[["days_per_year"]], 365.25)
})

test_that("the preview divides by the study's value", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  d <- fig_design(s$planner, "F-14-2-3")
  i <- which(vapply(d$data, function(p) identical(p$step, "time_unit"), NA))
  d$data[[i]]$unit <- "months"
  f <- file.path(s$path, .study_setup_path())
  writeLines(c(readLines(f), "days_per_month <- 1"), f)
  r <- preview_figure(s, "F-14-2-3", design = d, max_px = 400)
  days <- preview_figure(s, "F-14-2-3", max_px = 400)
  # divided by 1: the same times as in days
  expect_identical(r$objects$df$head$AVAL, days$objects$df$head$AVAL)
})
