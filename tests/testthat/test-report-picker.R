# The list to choose a report from (R/report_picker.R) and the study of
# many reports it is tried on (R/sample_big.R)

test_that("a report's section, state and search", {
  expect_identical(.report_section(c("T-14-1-1", "L-16-2-7", "F-14-2-3", "T-14", "X"),
                                   c("table", "listing", "figure", "table", "user")),
                   c("14.1", "16.2", "14.2", "14", "user"))
  expect_identical(.section_order(c("14.4", "15.2", "14.10", "14.3", "listing", "9.1")),
                   c("9.1", "14.3", "14.4", "14.10", "15.2", "listing"))
  expect_identical(.report_state(c("built", "outdated", "not built", "error", NA, "built"),
                                 c("ok", "ok", "ok", "ok", "not run", "error")),
                   c("ok", "outdated", "not built", "error", "not built", "error"))
  rows <- data.frame(output_id = c("T-14-1-1", "T-14-3-1", "L-16-2-7"),
                     title = c("Demographics", "Adverse events by SOC", "Listing of adverse events"),
                     section = c("14.1", "14.3", "16.2"),
                     population = c("SAF", "SAF", ""), datasets = c("ADSL", "ADAE | ADSL", "ADAE"),
                     state = c("ok", "error", "not built"), stringsAsFactors = FALSE)
  # every word somewhere; the ID as typed, full width, any case
  expect_identical(.report_filter(rows, "adverse")$output_id, c("T-14-3-1", "L-16-2-7"))
  expect_identical(.report_filter(rows, "adverse listing")$output_id, "L-16-2-7")
  expect_identical(.report_filter(rows, "T-14-3")$output_id, "T-14-3-1")
  expect_identical(.report_filter(rows, "\uff34\uff0d\uff11\uff14\uff0d\uff11")$output_id, "T-14-1-1")
  expect_identical(.report_filter(rows, "adae")$output_id, c("T-14-3-1", "L-16-2-7"))
  expect_identical(.report_filter(rows, "", "error")$output_id, "T-14-3-1")
  expect_identical(.report_filter(rows, "adverse", "not built")$output_id, "L-16-2-7")
  expect_identical(nrow(.report_filter(rows, "xyz")), 0L)
  # the hidden column a table searches
  k <- .report_search_keys(rows, c("L-16-2-7", "NEW"))
  expect_match(k[1], "listingofadverseevents", fixed = TRUE)
  expect_identical(k[2], "new")
})

test_that("a study of many reports, in sections, its states mixed", {
  skip_on_cran()
  home <- local_home()
  s <- .make_big_study(60L, root = file.path(home, "ws"), home = home)
  r <- .report_rows(s$planner, ard_status(s), .report_run_light(s))
  expect_identical(nrow(r), 60L)
  expect_identical(length(unique(r$section)), 10L)
  expect_true(all(c("ok", "outdated", "not built", "error") %in% r$state))
  expect_true(all(nzchar(r$title)))
  # the light run state reads what study_status() reads from the files
  full <- study_status(s)
  light <- .report_run_light(s)
  same <- full$status %in% c("ok", "error", "not run", "outdated")
  expect_identical(light$status[same], full$status[same])
  # the same each time
  s2 <- .make_big_study(60L, root = file.path(home, "ws2"), home = home, study_id = "BIG-B")
  expect_identical(ard_status(s2)$state, ard_status(s)$state)
})

test_that("the list chooses the report; the tables search the same way", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  .make_big_study(30L, root = file.path(home, "ws"), home = home, study_id = "BIG-30")
  shiny::testServer(server_for("BIG-30"), {
    session$setInputs(nav = "make", side = TRUE)
    h <- output$`rp-list`$html
    expect_match(h, "T-14-1-1", fixed = TRUE)
    expect_match(h, "Study defaults", fixed = TRUE)
    expect_match(h, "14.1 (", fixed = TRUE)
    # a search
    session$setInputs(`rp-q` = "adverse")
    h <- output$`rp-list`$html
    expect_match(h, "T-14-3-1", fixed = TRUE)
    expect_false(grepl("T-14-1-2", h, fixed = TRUE))
    expect_match(h, "of 30 reports", fixed = TRUE)
    # a click chooses it (the chooser itself is the browser's: its value
    # comes back as input$target)
    expect_no_error(session$setInputs(`rp-pick` = "T-14-3-11"))
    session$setInputs(target = "T-14-3-11")
    expect_match(output$`rp-list`$html, 'rp-now" data-id="T-14-3-11"', fixed = TRUE)
    # the folded chooser does nothing while the list is shown
    session$setInputs(`rp-compact` = "L-16-2-7")
    # the report list's and the runs' searches
    expect_no_error(session$setInputs(outputs_q = "T-14-3 adverse", status_q = "adverse"))
  })
  expect_identical(.report_dt_search("T-14-3  \uff21dverse"), "t143 adverse")
  expect_identical(.report_dt_search(""), "")
})
