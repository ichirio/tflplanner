# The sample's whole run: every program of the sample study runs without
# error (the check the old data-raw/make-sample-study.R made, #879).  About a
# minute, so only the CI job "sample run" runs it (TFLPLANNER_HEAVY=true); the
# test shards and a local run leave it out.

test_that("the sample study's programs all run (ARD, then the reports)", {
  skip_on_cran()
  skip_if_not(identical(tolower(Sys.getenv("TFLPLANNER_HEAVY")), "true"),
              "heavy test: set TFLPLANNER_HEAVY=true")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if(length(tflplanner:::.sample_missing()) > 0L,
          "packages the sample's programs need are missing")
  home <- withr::local_tempdir()
  withr::local_options(tflplanner.home = home)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  b <- suppressMessages(run_batch(s, "full"))
  if (!isTRUE(b$ok)) {
    for (f in file.path(b$batch, b$result$log[b$result$status != "OK"])) {
      cat("\n====", f, "\n")
      cat(utils::tail(readLines(f, warn = FALSE), 40), sep = "\n")
    }
  }
  expect_true(b$ok)
})
