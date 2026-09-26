local_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

two_studies <- function() {
  p1 <- add_output(new_planner(), "A", data_code = "a <- 1",
                   description = "first")
  p2 <- add_output(new_planner(), "B", data_code = "b <- 2",
                   description = "second")
  create_study("S1", planner = p1)
  create_study("S2", planner = p2)
}

server_for <- function(study) {
  start <- open_study(study)
  function(input, output, session) app_server(input, output, session, start)
}

test_that("switching studies does not write one report's editor into another", {
  local_home()
  two_studies()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "A", data_code = "a <- 1",
                      description = "first")
    session$setInputs(studies_rows_selected = 2L, open_study = 1L)
    expect_equal(rv$study$meta$study_id, "S2")
    # the sidebar moves to B first; the text box's echo of what the server
    # put there while no report was chosen arrives after it (debounced)
    session$setInputs(target = "B")
    session$setInputs(data_code = "", description = "")
    session$setInputs(data_code = "b <- 2", description = "second")
    expect_equal(rv$p$outputs$data_code, "b <- 2")
    expect_equal(rv$p$outputs$description, "second")
    expect_false(session$userData$dirty())
  }))
  expect_equal(open_study("S2")$planner$outputs$data_code, "b <- 2")
})

test_that("an edit in the editor changes the chosen report and is saved", {
  local_home()
  two_studies()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "A", data_code = "a <- 1")
    session$setInputs(data_code = "a <- 10")
    expect_equal(rv$p$outputs$data_code, "a <- 10")
    expect_true(session$userData$dirty())
    session$setInputs(save = 1L)
    expect_false(session$userData$dirty())
  }))
  expect_equal(open_study("S1")$planner$outputs$data_code, "a <- 10")
})
