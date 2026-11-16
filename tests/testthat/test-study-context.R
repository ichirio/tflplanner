# The study code context (.use_study(), #680): the study whose setup the
# programs are written for.  One R session, study A then study B; A's
# setup attaches dplyr, B's does not.

ctx_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = "DM", analysis_id = "AGE", method = "continuous",
    population_id = "SAF", by = "TRT01A", variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

# study A (the standard setup, with dplyr) and study B (its setup without
# dplyr), both registered
ctx_studies <- function() {
  a <- create_study("CTX-A", planner = ctx_planner())
  b <- create_study("CTX-B", planner = ctx_planner())
  f <- file.path(b$path, "programs", "study_setup.R")
  txt <- readLines(f)
  writeLines(txt[txt != "library(dplyr)"], f)
  list(a = a, b = b)
}

attached <- function() .code_context$opts$tflspec.attached
# a report's program keeps `dplyr::` (its setup does not attach dplyr, or
# no study's setup is assumed)
keeps_dplyr <- function(p) {
  any(grepl("dplyr::filter(output_id == report_id)", program_code(p, "DM"),
            fixed = TRUE))
}

test_that("each test starts with no study code context (#680, case 4)", {
  local_home()
  expect_null(.code_context$opts)
  expect_null(.code_context$root)
})

test_that("A then B by its ID: B's packages (#680, case 1)", {
  local_home()
  s <- ctx_studies()
  a <- open_study("CTX-A")
  expect_true("dplyr" %in% attached())
  expect_false(keeps_dplyr(a$planner))
  b <- open_study("CTX-B")
  expect_false("dplyr" %in% attached())
  expect_true(keeps_dplyr(b$planner))
})

test_that("A then B from its folder (not registered): B's packages (#680, case 1)", {
  local_home()
  s <- ctx_studies()
  unregister_study("CTX-B")
  open_study("CTX-A")
  expect_true("dplyr" %in% attached())
  b <- suppressMessages(open_study(s$b$path))
  expect_identical(b$meta$study_id, "CTX-B")
  expect_identical(.code_context$root, normalizePath(s$b$path, "/"))
  expect_false("dplyr" %in% attached())
  expect_true(keeps_dplyr(b$planner))
})

test_that("study_status() of B while A is open keeps A's context and cache (#680, case 3)", {
  local_home()
  s <- ctx_studies()
  a <- open_study("CTX-A")
  st_a <- study_status(a)
  opts_a <- .code_context$opts
  cache_a <- as.list(.program_cache, all.names = TRUE)
  expect_true(length(cache_a$code) > 0L)
  st_b <- study_status(s$b)
  expect_identical(.code_context$opts, opts_a)
  expect_identical(.code_context$root, normalizePath(a$path, "/"))
  # A's program cache: neither emptied nor taken over by B's options
  expect_identical(.program_cache$opts, opts_a)
  expect_identical(as.list(.program_cache, all.names = TRUE)[names(cache_a)], cache_a)
  expect_false(keeps_dplyr(a$planner))
  # A's programs are still as they were with A's context
  expect_identical(study_status(a)$program_state, st_a$program_state)
  expect_identical(st_b$output_id, "DM")
})

test_that("the study open in the app unregistered: no code context (#680, case 2)", {
  local_home()
  s <- ctx_studies()
  open_study("CTX-B")
  suppressWarnings(shiny::testServer(server_for("CTX-A"), {
    # the app's study sets the context (A's, not B's opened before)
    expect_true("dplyr" %in% attached())
    i <- match("CTX-A", list_studies()$study_id)
    session$setInputs(studies_rows_selected = i, unregister = 1L)
    session$setInputs(unregister_ok = 1L)
    expect_null(session$userData$rv$study)
  }))
  expect_null(.code_context$opts)
  expect_null(.code_context$root)
  # programs written now carry no setup's assumptions (A's attached dplyr)
  expect_true(keeps_dplyr(s$a$planner))
})

test_that("a study registered in the app takes its own context (#680, case 1)", {
  local_home()
  s <- ctx_studies()
  unregister_study("CTX-B")
  suppressWarnings(shiny::testServer(server_for("CTX-A"), {
    expect_true("dplyr" %in% attached())
    session$setInputs(reg_path = s$b$path, reg_ok = 1L)
    expect_identical(session$userData$rv$study$meta$study_id, "CTX-B")
    expect_false("dplyr" %in% attached())
  }))
})
