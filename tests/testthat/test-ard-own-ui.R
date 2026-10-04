test_that("own ARD functions: the study's and the company's side by side, tried, made, replaced", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  # the company's: one from tflspec's template
  dir.create(file.path(home, "standards", "ard_functions"), recursive = TRUE,
             showWarnings = FALSE)
  tflspec::tfl_ard_function_template(
    "ard_cv", "summary", file = file.path(home, "standards", "ard_functions", "ard_cv.R"))
  s <- create_study("OW")
  expect_identical(nrow(study_ard_functions(s)), 0L)
  own <- own_ard_functions(s)
  expect_identical(own$name, "ard_cv")
  expect_identical(own$where, "company")
  expect_false(own$loaded)
  # used in the study: copied, loaded
  s <- use_company_ard_function(s, "ard_cv")
  save_study(s)
  own <- own_ard_functions(s)
  expect_identical(own$where, "both")
  expect_true(own$loaded)
  expect_false(own$differs)
  # tried on cards' example data (no dataset given), kept
  r <- try_ard_function(s, "ard_cv", args = "by = ARM, variables = AGE")
  expect_null(r$error)
  expect_true(nrow(r$ard) > 0)
  ck <- own_function_checks(s)[["ard_cv"]]
  expect_identical(ck$data, "cards::ADSL")
  expect_identical(own_ard_functions(s)$stale, FALSE)
  # the study's copy edited: it differs, is newer, the try is stale
  f <- file.path(s$path, "programs/ard/functions/ard_cv.R")
  Sys.setFileTime(f, Sys.time() + 60)
  cat("\n# edited here\n", file = f, append = TRUE)
  own <- own_ard_functions(s)
  expect_true(own$differs)
  expect_identical(own$newer, "study")
  expect_true(own$stale)
  # replaced by the company's again
  replace_with_company_ard_function(s, "ard_cv")
  expect_false(own_ard_functions(s)$differs)
  # a new one for the study, from a template: written and loaded
  s <- new_ard_function(s, "ard_hl", "test")
  expect_true(file.exists(file.path(s$path, "programs/ard/functions/ard_hl.R")))
  expect_true(file.exists(file.path(s$path, "programs/ard/functions/test-ard_hl.R")))
  own <- own_ard_functions(s)
  expect_true(own$loaded[own$name == "ard_hl"])
  expect_identical(own$where[own$name == "ard_hl"], "study")
  expect_error(new_ard_function(s, "ard_hl", "test"), "already")
  expect_error(new_ard_function(s, "riskdiff", "test"), "starts with ard_")
  # a function that stops is told, not raised
  writeLines("ard_bad <- function(data, ...) stop(\"nope\")",
             file.path(s$path, "programs/ard/functions/ard_bad.R"))
  s <- .add_source(s, "programs/ard/functions/ard_bad.R")
  r <- try_ard_function(s, "ard_bad")
  expect_true(any(r$problems$level == "error"))
})
