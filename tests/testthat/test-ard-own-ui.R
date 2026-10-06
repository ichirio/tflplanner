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
  # the whole ARD's rows, the problems by kind, who tried it
  expect_true(ck$rows > nrow(r$ard) || ck$rows == nrow(r$ard))
  expect_identical(ck$errors, 0L)
  expect_true(all(c("warnings", "notes", "user") %in% names(ck)))
  expect_identical(own_ard_functions(s)$stale, FALSE)
  # the study's copy edited: it differs, changed in the study (not by the
  # files' times), the try is stale
  f <- file.path(s$path, "programs/ard/functions/ard_cv.R")
  cf <- file.path(home, "standards/ard_functions/ard_cv.R")
  cat("\n# edited here\n", file = f, append = TRUE)
  own <- own_ard_functions(s)
  expect_true(own$differs)
  expect_identical(own$newer, "study")
  expect_true(own$stale)
  # the company's changed too: both
  cat("\n# edited by the company\n", file = cf, append = TRUE)
  expect_identical(own_ard_functions(s)$newer, "both")
  # replaced by the company's: the same again, the copy recorded anew
  take_company_ard_function(s, "ard_cv")
  expect_false(own_ard_functions(s)$differs)
  # the company's changes again, the study's not: company
  cat("\n# again\n", file = cf, append = TRUE)
  expect_identical(own_ard_functions(s)$newer, "company")
  # a study file holding another function as well is not replaced
  cat("\nard_other <- function(data, ...) NULL\n", file = f, append = TRUE)
  expect_error(take_company_ard_function(s, "ard_cv"), "ard_other")
  writeLines(readLines(cf), f)
  # a new one for the study, from a template: written and loaded
  s <- new_ard_function(s, "ard_hl", "test")
  expect_true(file.exists(file.path(s$path, "programs/ard/functions/ard_hl.R")))
  expect_true(file.exists(file.path(s$path, "programs/ard/functions/test-ard_hl.R")))
  own <- own_ard_functions(s)
  expect_true(own$loaded[own$name == "ard_hl"])
  expect_identical(own$where[own$name == "ard_hl"], "study")
  expect_error(new_ard_function(s, "ard_hl", "test"), "already")
  expect_error(new_ard_function(s, "riskdiff", "test"), "starts with ard_")
  # a cards / cardx function's name would hide it
  expect_error(new_ard_function(s, "ard_tabulate", "test"), "cards / cardx function")
  # a function that stops is told, not raised
  writeLines("ard_bad <- function(data, ...) stop(\"nope\")",
             file.path(s$path, "programs/ard/functions/ard_bad.R"))
  s <- .add_source(s, "programs/ard/functions/ard_bad.R")
  r <- try_ard_function(s, "ard_bad")
  expect_true(any(r$problems$level == "error"))
})

test_that("the Own functions tab: listed, used, tried, a new one; the analysis form offers them", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  dir.create(file.path(home, "standards", "ard_functions"), recursive = TRUE,
             showWarnings = FALSE)
  tflspec::tfl_ard_function_template(
    "ard_cv", "summary", file = file.path(home, "standards", "ard_functions", "ard_cv.R"))
  s <- create_study("OU")
  s$planner <- add_output(s$planner, "T1", type = "table")
  s$planner <- set_ard_rows(s$planner, "analyses", "T1",
                            data.frame(analysis_id = "A1", method = "continuous",
                                       variables = "AGE"))
  save_study(s)
  shiny::testServer(server_for("OU"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    expect_no_error(output$own_list)
    session$setInputs(own_list_rows_selected = 1L)
    h <- output$own_detail$html
    expect_match(h, "ard_cv")
    expect_match(h, "own_use")
    # no keywords yet: where they go
    expect_match(h, "# tflplanner-keywords:", fixed = TRUE)
    # the analysis form: the company's, not loaded, faint
    session$setInputs(ard_ol_pick = "A1")
    n <- session$userData$st_env$n
    do.call(session$setInputs, stats::setNames(list("Own and code"), paste0("st", n, "_fn_cat")))
    expect_match(output$ard_fn_list$html, "not loaded by this study", fixed = TRUE)
    # used: copied, loaded, saved at once
    session$setInputs(own_use = 1)
    expect_true(file.exists(file.path(rv$study$path, "programs/ard/functions/ard_cv.R")))
    expect_match(open_study(rv$study$path)$planner$ard$study$value[
      open_study(rv$study$path)$planner$ard$study$key == "source"], "ard_cv.R", fixed = TRUE)
    # the form is drawn again: its category chosen again
    n <- session$userData$st_env$n
    do.call(session$setInputs, stats::setNames(list("Own and code"), paste0("st", n, "_fn_cat")))
    expect_false(grepl("not loaded by this study", output$ard_fn_list$html, fixed = TRUE))
    # under its title and its name, where it is said
    expect_match(output$ard_fn_list$html, "(ard_cv)", fixed = TRUE)
    expect_match(output$ard_fn_list$html, "copy of the company", fixed = TRUE)
    # tried on cards' example data
    session$setInputs(own_list_rows_selected = 1L, own_try = 1)
    session$setInputs(own_try_data = "__cards__", own_try_args = "by = ARM, variables = AGE", own_try_go = 1)
    expect_match(output$own_try_result$html, "It behaves", fixed = TRUE)
    # the code, shown
    session$setInputs(own_code = 1)
    # the difference, the lines the other has not marked
    cat("# the study's own line", file = file.path(rv$study$path, "programs/ard/functions/ard_cv.R"),
        append = TRUE, sep = "\n")
    session$setInputs(own_refresh = 1)
    expect_match(output$own_detail$html, "own_diff", fixed = TRUE)
    session$setInputs(own_diff = 1)
    # a new one for the study
    session$setInputs(own_new = 1)
    session$setInputs(own_new_name = "ard_hl", own_new_type = "test", own_new_where = "study",
                      own_new_test = TRUE, own_new_ok = 1)
    expect_true(file.exists(file.path(rv$study$path, "programs/ard/functions/ard_hl.R")))
    expect_true("ard_hl" %in% own_ard_functions(rv$study)$name)
  })
})

test_that("a function is tried on the study's data as the ARD programs make it", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  dir.create(file.path(home, "standards", "ard_functions"), recursive = TRUE,
             showWarnings = FALSE)
  tflspec::tfl_ard_function_template(
    "ard_cv", "summary", file = file.path(home, "standards", "ard_functions", "ard_cv.R"))
  s <- create_study("OD")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  saveRDS(cards::ADLB, file.path(s$path, "data/adam/adlb.rds"))
  s$planner$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = c("ADSL", "ADLB"), level = "adam",
    path = c("data/adam/adsl.rds", "data/adam/adlb.rds")), "datasets")
  s$planner$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""), "populations")
  s <- use_company_ard_function(s, "ard_cv")
  save_study(s)
  # ADLB has no SAFFL: its subjects are those of the analysis set (ADSL's)
  r <- try_ard_function(s, "ard_cv", dataset = "ADLB", population_id = "SAF",
                        args = "by = TRTA, variables = AVAL")
  expect_null(r$error)
  expect_true(nrow(r$ard) > 0)
  ck <- own_function_checks(s)[["ard_cv"]]
  expect_identical(ck$data, "ADLB x SAF")
  expect_true(all(c("tflspec", "cards", "cardx") %in% names(ck$versions)))
})
