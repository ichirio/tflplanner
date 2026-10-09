# What an edit or a save makes the app work out again: only what depends
# on it (#9154).  An edit of a table's cell does not make the ARD's state,
# the report list or the own functions be worked out again; a save does
# not redraw every output (it gives the study anew, the same study); and
# a save writes only the workbook whose half changed.

test_that("a table's cell edited: the ARD state and the report list are not worked out again", {
  local_home()
  p <- add_output(new_planner(), "T1", type = "table", description = "one")
  p <- add_output(p, "T2", type = "table", description = "two")
  create_study("RS", planner = p)
  n <- new.env()
  n$ard <- 0L
  n$rows <- 0L
  real_status <- ard_status
  real_rows <- .report_rows
  local_mocked_bindings(
    ard_status = function(...) {
      n$ard <- n$ard + 1L
      real_status(...)
    },
    .report_rows = function(...) {
      n$rows <- n$rows + 1L
      real_rows(...)
    })
  shiny::testServer(server_for("RS"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", target = "T1")
    session$flushReact()
    a0 <- n$ard
    r0 <- n$rows
    expect_gt(r0, 0L)
    # a table sheet: neither
    rv$p <- set_sheet_rows(rv$p, "tables", "T1",
                           data.frame(cols = "TRT01A", label = "Edited"))
    session$flushReact()
    expect_identical(n$ard, a0)
    expect_identical(n$rows, r0)
    # a title: the list shows it (not the ARD state)
    rv$p <- set_sheet_rows(rv$p, "titles", "T1",
                           data.frame(line = "1", center = "A title"))
    session$flushReact()
    expect_identical(n$ard, a0)
    expect_gt(n$rows, r0)
    # the ARD definition: both
    r1 <- n$rows
    rv$p$ard$populations <- .normalize_ard_sheet(
      data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
      "populations")
    session$flushReact()
    expect_gt(n$ard, a0)
    expect_gt(n$rows, r1)
  })
})

test_that("a save gives the study anew: whether one is open does not change", {
  local_home()
  two_studies()
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    has_study <- session$userData$has_study
    expect_true(has_study())
    seen <- 0L
    obs <- shiny::observe({
      has_study()
      seen <<- seen + 1L
    })
    session$flushReact()
    s0 <- seen
    # the same study, a new copy (as a save gives it)
    s <- rv$study
    s$saved <- "another time"
    rv$study <- s
    session$flushReact()
    expect_identical(seen, s0)
    expect_true(has_study())
    obs$destroy()
  })
})

test_that("a save writes only the workbook whose half changed", {
  local_home()
  p <- add_output(new_planner(), "T1", type = "table", description = "one")
  s <- create_study("BK", planner = p)
  spec <- file.path(s$path, study_layout()[["spec"]])
  tf <- file.path(spec, "table_spec.xlsx")
  rf <- file.path(spec, "report_spec.xlsx")
  md5 <- function() unname(tools::md5sum(c(tf, rf)))
  m0 <- md5()
  # a table's cell: the table workbook only
  s$planner <- set_sheet_rows(s$planner, "tables", "T1",
                              data.frame(cols = "TRT01A", label = "Edited"))
  s <- save_study(s)
  m1 <- md5()
  expect_false(identical(m1[1], m0[1]))
  expect_identical(m1[2], m0[2])
  expect_identical(s$files$status[match(c(tf, rf), s$files$file)],
                   c("written", "unchanged"))
  # a title: the report workbook only
  s$planner <- set_sheet_rows(s$planner, "titles", "T1",
                              data.frame(line = "1", center = "A title"))
  s <- save_study(s)
  m2 <- md5()
  expect_identical(m2[1], m1[1])
  expect_false(identical(m2[2], m1[2]))
  # what was written reads back as the definition
  back <- read_planner(c(tf, rf))
  expect_identical(sheet_rows(back, "tables", "T1")$label, "Edited")
  expect_identical(sheet_rows(back, "titles", "T1")$center, "A title")
})

test_that("a program's checksum is the same however often it is worked out", {
  code <- c("# a", "#  Generated  : tflplanner", "x <- 1", "y <- 2")
  h <- .body_hash(code)
  expect_identical(.body_hash(code), h)
  expect_identical(.body_hash(c(code, "z <- 3")) == h, FALSE)
  # the same as md5sum() of the body written to a file of its own
  f <- tempfile()
  writeLines(enc2utf8(code[-2]), f, useBytes = TRUE)
  expect_identical(h, unname(tools::md5sum(f)))
})
