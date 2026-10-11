# When an official run ends -- this session's, or one it did not start (the
# app restarted, another session, R) -- the report list's marks and the
# table builder's ARD follow it without a reload.

run_end_server <- function() {
  skip_on_cran()
  function(input, output, session) app_server(input, output, session, open_study("SAMPLE-01"))
}
run_end_marks <- function(h) {
  c(not_built = lengths(regmatches(h, gregexpr("○", h))),
    ok = lengths(regmatches(h, gregexpr("●", h))))
}

test_that("a run this session did not start: the marks and the builder's ARD follow it", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  local_home()
  suppressMessages(create_sample_study(run = FALSE))
  shiny::testServer(run_end_server(), {
    session$setInputs(nav = "outputs")
    expect_identical(run_end_marks(output[["rp-list"]]$html)[["ok"]], 0L)
    session$setInputs(target = "T-14-1-5", nav = "make", step = "content",
                      content_nav = "content", table_nav = "builder")
    session$elapse(1000)
    expect_match(output$builder_note$html, "no ARD yet", fixed = TRUE)
    # made elsewhere: no job of this session
    invisible(suppressMessages(run_batch(open_study("SAMPLE-01"), "ard")))
    for (i in 1:5) session$elapse(1100)
    expect_false(grepl("no ARD yet", output$builder_note$html %||% "", fixed = TRUE))
    # the marks come as marks, the list is not drawn again for them (#9184)
    re <- session$userData$run_end
    mk <- re$marks()
    expect_false(is.null(mk))
    expect_true(mk$seq > attr(re$rows(), "seq"))
    expect_true(all(c("T-14-1-5", "F-14-2-3") %in% names(mk$marks)))
    # drawn again for another reason, the list has them
    session$setInputs(nav = "outputs")
    m <- run_end_marks(output[["rp-list"]]$html)
    expect_true(m[["ok"]] + m[["not_built"]] > 0L)
  })
})

test_that("a run in the background draws nothing again: the list, the report list's table, the step marks", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  local_home()
  suppressMessages(create_sample_study(run = FALSE))
  shiny::testServer(run_end_server(), {
    session$setInputs(nav = "outputs")
    session$elapse(3100)
    re <- session$userData$run_end
    rv <- session$userData$rv
    rows0 <- attr(re$rows(), "seq")
    view0 <- re$view()
    status_ver0 <- rv$status_ver
    ard_ver0 <- rv$ard_ver
    invisible(suppressMessages(run_batch(open_study("SAMPLE-01"), "ard")))
    for (i in 1:3) session$elapse(1100)
    # nothing that draws the page was invalidated
    expect_identical(attr(re$rows(), "seq"), rows0)
    expect_identical(rv$status_ver, status_ver0)
    expect_identical(rv$ard_ver, ard_ver0)
    expect_identical(re$view(), view0)
    # what follows it: the marks, the ARD's state, the Runs tab
    expect_false(is.null(re$marks()))
    expect_true(re$runs_ver() > 0L)
    expect_true(re$ard_state_ver() > 0L)
  })
})

test_that("the marks a run sent are the browser's: one span a report", {
  m <- .report_mark_msg(c(A = "ok", B = "not built", C = NA))
  expect_identical(names(m), c("A", "B"))
  expect_identical(m$A$mark, .report_state_marks[["ok"]])
  expect_match(m$B$cls, "^rp-mark text-muted$")
})

test_that("the stamp of a study's run results: the ARD's status and the batches, not the logs", {
  d <- withr_tempdir()
  expect_identical(.run_files_stamp(d), c(ard = "", runs = ""))
  dir.create(file.path(d, study_layout()[["ard"]]), recursive = TRUE)
  f <- file.path(d, study_layout()[["ard"]], "ard_status.csv")
  writeLines("output_id", f)
  s1 <- .run_files_stamp(d)
  expect_match(s1[["ard"]], "^1 ")
  expect_identical(s1[["runs"]], "")
  # a log written (a preview's, all through a run): no change
  dir.create(file.path(d, study_layout()[["logs_preview"]]), recursive = TRUE)
  writeLines("x", file.path(d, study_layout()[["logs_preview"]], "T1.log"))
  expect_identical(.run_files_stamp(d), s1)
  dir.create(file.path(d, "runs", "20261011_000000_all"), recursive = TRUE)
  writeLines("part", file.path(d, "runs", "20261011_000000_all", "run.csv"))
  s2 <- .run_files_stamp(d)
  expect_identical(s2[["ard"]], s1[["ard"]])
  expect_match(s2[["runs"]], "^1 ")
})
