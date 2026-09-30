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

test_that("unregistering removes the chosen study only, and the list still draws", {
  local_home()
  two_studies()
  # S2 was created last: the last study opened
  suppressWarnings(shiny::testServer(server_for("S1"), {
    session$setInputs(studies_rows_selected = 2L, unregister = 1L)
    # the selection moves before the OK: the study chosen when the
    # confirmation opened is the one unregistered
    session$setInputs(studies_rows_selected = 1L, unregister_ok = 1L)
    expect_equal(list_studies()$study_id, "S1")
    expect_s3_class(output$studies, "json")
    # the last one: an empty list gives way to the getting-started card
    session$setInputs(studies_rows_selected = 1L, unregister = 2L)
    session$setInputs(unregister_ok = 2L)
    expect_equal(nrow(list_studies()), 0L)
    expect_match(output$welcome$html, "try_sample")
  }))
})

test_that("unregister_study() takes one registered study and nothing else", {
  home <- local_home()
  two_studies()
  expect_error(unregister_study(""), "one study ID")
  expect_error(unregister_study(character()), "one study ID")
  expect_error(unregister_study(NA_character_), "one study ID")
  expect_error(unregister_study("NOPE"), "not registered")
  expect_setequal(list_studies()$study_id, c("S1", "S2"))
  unregister_study("S2")
  expect_equal(list_studies()$study_id, "S1")
})

test_that("the app starts with a study given (run_app(\"S1\"))", {
  local_home()
  two_studies()
  app <- planner_app("S1")
  s <- shiny::MockShinySession$new()
  # as runApp() calls it: outside any reactive context
  expect_no_error(shiny::withReactiveDomain(
    s, app$serverFuncSource()(s$input, s$output, s)))
})
