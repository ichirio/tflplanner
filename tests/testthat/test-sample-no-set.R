# The sample's reports with no analysis set (T-14-0-1, the study's
# information) and with an analysis set by a condition alone (T-14-1-4,
# the screen failures, SCRF): the app's screens draw them, ARS writes them

test_that("a report with no analysis set, and one with a set by a condition, draw and write", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  p <- s$planner
  expect_true(is.na(report_population(p, "T-14-0-1")))
  expect_identical(report_population(p, "T-14-1-4"), "SCRF")
  expect_true("SCRF" %in% p$ard$populations$population_id)
  expect_true(is.na(p$ard$populations$where[p$ard$populations$population_id == "SCRF"]) ||
                grepl("Screen Failure", p$ard$populations$where[p$ard$populations$population_id == "SCRF"],
                      fixed = TRUE))
  # the ARD tab and the report's program, each report
  shiny::testServer(server_for("SAMPLE-01"), {
    for (o in c("T-14-0-1", "T-14-1-4")) {
      session$setInputs(nav = "make", step = "ard", target = o)
      for (nm in c("ard_report_pop", "ard_adata", "ard_outline", "ard_spec_pane",
                   "ard_code", "ard_state")) {
        expect_no_error(output[[nm]])
      }
      session$setInputs(nav = "make", step = "table", target = o)
      expect_no_error(output$program)
    }
  })
  # ARS: the study information's analysis has no analysis set
  x <- .ard_spec(p$ard)
  x$analyses$purpose <- "PRIMARY OUTCOME MEASURE"
  ars <- suppressWarnings(tflspec::tfl_ars(x))
  an <- Filter(function(z) startsWith(z$id, "An_T-14-0-1_"), ars$analyses)
  expect_length(an, 1L)
  expect_null(an[[1L]]$analysisSetId)
  sets <- vapply(ars$analysisSets, `[[`, "", "id")
  expect_true("SCRF" %in% sets)
})
