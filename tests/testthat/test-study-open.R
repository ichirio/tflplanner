# Opening a study from the Studies tab: the dialog of the study before is
# closed, and a study with the old spec/codelists.yml says so on screen.
skip_on_cran()

# The modal and notification messages a mock session is sent, kept in `log`
record_session <- function(session, log) {
  session$sendModal <- function(type, message) log$modal <- c(log$modal, type)
  session$sendNotification <- function(type, message) {
    log$note <- c(log$note, paste(message$type, message$html))
  }
  invisible(session)
}

test_that("opening a study closes the dialog of the study before (and notes the old codelists.yml)", {
  home <- local_home()
  two_studies()
  s2 <- file.path(home, "ws", "S2")
  dir.create(file.path(s2, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines("codelists: []", file.path(s2, "spec", "codelists.yml"))
  log <- new.env()
  log$modal <- character()
  log$note <- character()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    record_session(session, log)
    session$setInputs(studies_rows_selected = 2L, open_study = 1L)
    expect_equal(session$userData$rv$study$meta$study_id, "S2")
  }))
  expect_true("remove" %in% log$modal)
  expect_equal(sum(grepl("warning.*spec/codelists.yml is not read", log$note)), 1L)
})

test_that("opening a study without the old file says nothing about it", {
  local_home()
  two_studies()
  log <- new.env()
  log$modal <- character()
  log$note <- character()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    record_session(session, log)
    session$setInputs(studies_rows_selected = 2L, open_study = 1L)
  }))
  expect_false(any(grepl("codelists.yml", log$note)))
})
