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
    session$setInputs(nav = "outputs")
    m <- run_end_marks(output[["rp-list"]]$html)
    # the tables' ARDs are made (their reports not yet: no RTF)
    expect_true(m[["ok"]] + m[["not_built"]] > 0L)
  })
})

test_that("the stamp of a study's run results changes when a run writes them", {
  d <- withr_tempdir()
  expect_identical(.run_files_stamp(d), "")
  dir.create(file.path(d, study_layout()[["ard"]]), recursive = TRUE)
  f <- file.path(d, study_layout()[["ard"]], "ard_status.csv")
  writeLines("output_id", f)
  s1 <- .run_files_stamp(d)
  expect_match(s1, "^1 ")
  dir.create(file.path(d, "runs", "20261011_000000_all"), recursive = TRUE)
  writeLines("part", file.path(d, "runs", "20261011_000000_all", "run.csv"))
  Sys.setFileTime(file.path(d, "runs", "20261011_000000_all", "run.csv"), Sys.time() + 5)
  expect_false(identical(.run_files_stamp(d), s1))
})
