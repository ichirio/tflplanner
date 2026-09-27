test_that("the sample study is copied, registered and written", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  root <- file.path(home, "studies")
  suppressMessages(setup_tflplanner(studies_root = root))
  s <- suppressMessages(create_sample_study(run = FALSE))
  expect_equal(s$meta$study_id, "SAMPLE-01")
  expect_equal(normalizePath(dirname(s$path), "/"),
               normalizePath(root, "/"))
  expect_setequal(s$planner$outputs$output_id,
                  c("T-14-1-1", "T-14-1-2", "T-14-2-1", "T-14-3-1",
                    "L-16-2-7", "F-14-2-1"))
  expect_true(nrow(s$planner$ard$analyses) > 0)
  for (f in c("data/adam/adsl.rds", "programs/batch.R",
              "programs/ard/T-14-1-1.R", "programs/tfl/F-14-2-1.R")) {
    expect_true(file.exists(file.path(s$path, f)), label = f)
  }
  expect_equal(unique(study_status(s)$program_state), "current")
  # once only
  expect_error(create_sample_study(run = FALSE), "already registered")
  expect_message(setup_tflplanner(sample = TRUE), "already there")
})

test_that("the sample study makes its ARD and reports", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggplot2")
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "st"),
                                    sample = TRUE))
  s <- open_study("SAMPLE-01")
  expect_equal(unique(study_status(s)$status), "ok")
  expect_equal(unique(ard_status(s)$state), "built")
  b <- list_batches(s)
  expect_equal(nrow(b), 1L)
  expect_equal(b$errors, 0L)
})
