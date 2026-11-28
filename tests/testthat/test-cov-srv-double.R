# The branches of R/srv_double.R no other test reaches: the ARD log below
# the state table (nothing chosen, no official run, a run's log), the
# catalog's datasets with no file, a double click on a report that is not
# there, the note above a listing's form, the switch back from a step of
# one's own code, and a listing preview asked of a table.
# (One server a test: each test does several things.)

cov_dbl_study <- function(id = "DB-1") {
  p <- new_planner()
  p <- add_output(p, "DM", process_code = "x <- 1")
  p <- add_output(p, "L1", type = "listing", at = "end")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(output_id = "DM", analysis_id = "AGE",
                               method = "continuous", population_id = "SAF", by = "TRT01A",
                               variables = "AGE")
  for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)
  suppressMessages(create_study(id, planner = p))
}

cov_dbl_notes <- function(env = parent.frame()) {
  got <- new.env()
  got$n <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      got$n <- c(got$n, paste(as.character(ui), collapse = ""))
      invisible("id")
    }, .package = "shiny", .env = env)
  function() got$n
}

test_that("the ARD log: nothing chosen, a report with no official run, the latest run's log", {
  skip_if_quick()
  local_home()
  s <- cov_dbl_study()
  shiny::testServer(server_for("DB-1"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "DM")
    expect_match(output$ard_log, "Choose a row to see its log", fixed = TRUE)
    # (the state table's first row: DM, the study's first report)
    session$setInputs(ard_state_rows_selected = 1L)
    expect_match(output$ard_log, "DM has no official run yet.", fixed = TRUE)
    # an official run of the ARD with DM's log
    d <- file.path(s$path, study_layout()[["runs"]], "20261101_101500_ard", "logs", "ard")
    dir.create(d, recursive = TRUE)
    writeLines(c("> x <- 1", "done \u00e9"), file.path(d, "DM.log"))
    rv$status_ver <- (rv$status_ver %||% 0) + 1
    session$flushReact()
    expect_match(output$ard_log, "done \u00e9", fixed = TRUE)
    expect_match(output$ard_log, "20261101_101500_ard", fixed = TRUE)
  })
})

test_that("the catalog's datasets with no file are named; a double click on a report not there does nothing", {
  skip_if_quick()
  local_home()
  cov_dbl_study()
  shiny::testServer(server_for("DB-1"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "DM")
    expect_match(output$catalog_missing$html, "No file for: ADSL", fixed = TRUE)
    p <- rv$p
    session$setInputs(ard_state_dbl = "NO-SUCH")
    expect_identical(rv$p, p)
    expect_false(session$isClosed())
  })
})

test_that("the note above the form says what a table and a listing are; a listing preview of a table is refused", {
  skip_if_quick()
  local_home()
  cov_dbl_study()
  notes <- cov_dbl_notes()
  shiny::testServer(server_for("DB-1"), {
    rv <- session$userData$rv
    open_report(session, "DM", nav = "make")
    expect_match(output$lf_note$html, "This is a Table", fixed = TRUE)
    session$setInputs(lf_preview = 1)
    expect_true(any(grepl("Choose a Listing report", notes(), fixed = TRUE)))
    open_report(session, "L1")
    # a listing's note is the (i) of its form's heading
    expect_null(output$lf_note)
  })
})

test_that("a step of one's own code: R program kept when asked, removed when asked", {
  skip_if_quick()
  local_home()
  cov_dbl_study()
  shiny::testServer(server_for("DB-1"), {
    rv <- session$userData$rv
    open_report(session, "DM", nav = "make")
    rv$p <- set_step_code(rv$p, "DM", "data", "adsl <- adsl")
    session$flushReact()
    expect_identical(output$data_is_code, "yes")
    # back to the definition, then kept: the code stays
    session$setInputs(data_mode = "gui")
    session$setInputs(mode_keep = 1)
    expect_true(.is_code_step(rv$p, "DM", "data"))
    expect_identical(output$data_is_code, "yes")
    # asked again and removed: the definition makes the step
    session$setInputs(data_mode = "code")
    session$setInputs(data_mode = "gui")
    session$setInputs(mode_drop = 1)
    expect_false(.is_code_step(rv$p, "DM", "data"))
    expect_identical(output$data_is_code, "no")
  })
})
