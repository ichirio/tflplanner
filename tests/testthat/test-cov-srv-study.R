# The branches of R/srv_study.R no other test reaches: Load from the
# definition files with nothing changed and with a file that does not read,
# Discard with nothing to discard, and what Close says with unsaved changes
# and with a preview going on.
# (One server a test: each test does several things.)

cov_st_study <- function(id = "ST-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "A title"))
  suppressMessages(create_study(id, planner = p))
}

# what the server shows: notifications and modals, as functions that give them so far
cov_st_shown <- function(env = parent.frame()) {
  got <- new.env()
  got$n <- character()
  got$m <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      got$n <- c(got$n, paste(as.character(ui), collapse = ""))
      invisible("id")
    },
    showModal = function(ui, session = NULL) got$m <- c(got$m, paste(as.character(ui), collapse = "")),
    removeModal = function(session = NULL) invisible(NULL),
    .package = "shiny", .env = env)
  list(notes = function() got$n, modals = function() got$m)
}

test_that("Load from the definition files with nothing changed says so and keeps the study as saved", {
  skip_if_quick()
  local_home()
  cov_st_study()
  shown <- cov_st_shown()
  shiny::testServer(server_for("ST-1"), {
    rv <- session$userData$rv
    session$setInputs(spec_reload = 1)
    expect_true(any(grepl("Nothing changed in the definition files.", shown$notes(), fixed = TRUE)))
    expect_identical(rv$p, rv$saved)
    expect_false(isTRUE(rv$spec$status == "invalid"))
  })
})

test_that("Load from the definition files when one does not read: the study stays, the files' problems shown", {
  skip_if_quick()
  local_home()
  s <- cov_st_study()
  shown <- cov_st_shown()
  shiny::testServer(server_for("ST-1"), {
    rv <- session$userData$rv
    before <- rv$p
    writeLines(c("id: [", "kind"), file.path(s$path, "spec", "outputs", "T-1.yml"))
    session$setInputs(spec_reload = 1)
    expect_identical(rv$spec$status, "invalid")
    expect_true(nrow(rv$spec$problems) >= 1L)
    expect_true(any(rv$spec$files$status == "changed"))
    expect_identical(rv$p, before)
    expect_true(length(shown$modals()) >= 1L)
  })
})

test_that("Discard with nothing unsaved asks nothing", {
  skip_if_quick()
  local_home()
  cov_st_study()
  shown <- cov_st_shown()
  shiny::testServer(server_for("ST-1"), {
    n <- length(shown$modals())
    session$setInputs(discard = 1)
    expect_identical(length(shown$modals()), n)
  })
})

test_that("Close with unsaved changes offers to save; with a preview going on it says the preview stops", {
  skip_if_quick()
  local_home()
  cov_st_study()
  shown <- cov_st_shown()
  shiny::testServer(server_for("ST-1"), {
    rv <- session$userData$rv
    rv$p$outputs$description[1] <- "changed"
    session$flushReact()
    session$setInputs(close_app = 1)
    m <- shown$modals()[length(shown$modals())]
    expect_match(m, "There are unsaved changes. Save them before tflplanner stops?", fixed = TRUE)
    expect_match(m, "close_save", fixed = TRUE)
    expect_match(m, "close_nosave", fixed = TRUE)
    # (a preview still going: all the server asks of it here, killed as the session ends)
    rv$job <- list(is_alive = function() TRUE, kill = function() invisible(TRUE))
    rv$job_kind <- "preview"
    rv$job_what <- "a preview of T-1"
    session$setInputs(close_app = 2)
    m <- shown$modals()[length(shown$modals())]
    expect_match(m, "A preview of T-1 is going on: closing stops it", fixed = TRUE)
    expect_match(m, "alert-warning", fixed = TRUE)
  })
})
