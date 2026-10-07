# The reports' font and size: the page sheet's study row, set once (the
# study tab, the company standards), written once in report_setup.R

test_that("the study's font and size are the page sheet's study row", {
  p <- new_planner()
  expect_true(is.na(study_page_value(p, "font")))
  p <- set_study_page_value(p, "font", "Courier New")
  p <- set_study_page_value(p, "font_size_half_points", "20")
  d <- p$sheets$page
  expect_identical(nrow(d), 1L)
  expect_true(is.na(d$output_id))
  expect_identical(study_page_value(p, "font"), "Courier New")
  expect_identical(study_page_value(p, "font_size_half_points"), "20")
  # a report's own row is not the study's
  p$sheets$page <- rbind(p$sheets$page, transform(p$sheets$page[1, ],
    output_id = "T-1", font = "Arial"))
  expect_identical(study_page_value(p, "font"), "Courier New")
  # blank: none; a row left with nothing goes
  p <- set_study_page_value(p, "font", "")
  p <- set_study_page_value(p, "font_size_half_points", NA)
  expect_identical(p$sheets$page$output_id, "T-1")
  expect_identical(set_study_page_value(new_planner(), "font", NA)$sheets$page,
                   new_planner()$sheets$page)
})

test_that("a size is said in points and kept in half-points", {
  expect_identical(.half_points("9"), "18")
  expect_identical(.half_points(" 10.5 "), "21")
  expect_true(is.na(.half_points("nine")))
  expect_true(is.na(.half_points("0")))
  expect_true(is.na(.half_points(NA)))
  expect_identical(.points("18"), "9")
  expect_identical(.points("21"), "10.5")
  expect_identical(.points(NA), "")
})

test_that("the company's font and size: a new study's, and added where missing", {
  home <- local_home()
  f <- file.path(withr_tempdir(), "acme.xlsx")
  s <- .builtin_standards()
  s$settings$value[s$settings$key == "font"] <- "Courier New"
  s$settings$value[s$settings$key == "font_size"] <- "10"
  writexl::write_xlsx(s, f)
  suppressMessages(setup_tflplanner(standards = f))
  st <- create_study("NEW-F")
  expect_identical(study_page_value(st$planner, "font"), "Courier New")
  expect_identical(study_page_value(st$planner, "font_size_half_points"), "20")
  # every report's program sources the setup that says them, once
  set <- report_setup_code(st$planner)
  expect_true(any(grepl('rtfreporter.font = "Courier New"', set, fixed = TRUE)))
  expect_true(any(grepl("rtfreporter.font_size_half_points = 20L", set, fixed = TRUE)))
  # a study without them gets them; one with its own keeps its own
  p <- set_study_page_value(new_planner(), "orientation", "landscape")
  p2 <- add_standard_defaults(p, "X1")
  expect_true("font" %in% attr(p2, "added"))
  expect_identical(study_page_value(p2, "font"), "Courier New")
  expect_identical(study_page_value(p2, "orientation"), "landscape")
  p3 <- add_standard_defaults(set_study_page_value(p, "font", "Arial"), "X1")
  expect_identical(study_page_value(p3, "font"), "Arial")
  expect_identical(study_page_value(p3, "font_size_half_points"), "20")
})

test_that("the draft standards give no font: rtfreporter's own, no line", {
  local_home()
  p <- create_study("NEW-0")$planner
  expect_true(is.na(study_page_value(p, "font")))
  expect_false(any(grepl("rtfreporter.font", report_setup_code(p), fixed = TRUE)))
})

test_that("the study tab sets the font and size, the size in points", {
  local_home()
  create_study("S-F")
  suppressWarnings(shiny::testServer(server_for("S-F"), {
    rv <- session$userData$rv
    # the fields as drawn (blank), then typed in
    session$setInputs(pg_font = "", pg_font_size = "")
    session$setInputs(pg_font = "Courier New", pg_font_size = "10")
    expect_identical(study_page_value(rv$p, "font"), "Courier New")
    expect_identical(study_page_value(rv$p, "font_size_half_points"), "20")
    # not a size: said, nothing changed
    session$setInputs(pg_font_size = "big")
    expect_identical(study_page_value(rv$p, "font_size_half_points"), "20")
    session$setInputs(pg_font = "", pg_font_size = "")
    expect_true(is.na(study_page_value(rv$p, "font")))
    expect_true(is.na(study_page_value(rv$p, "font_size_half_points")))
  }))
})

test_that("a report's own font and size are its row of the page sheet", {
  p <- set_study_page_value(new_planner(), "font", "Courier New")
  p <- set_page_value(p, "font", "Arial", "T-1")
  d <- p$sheets$page
  expect_identical(d$output_id, c(NA, "T-1"))
  expect_identical(page_value(p, "font", "T-1"), "Arial")
  expect_true(is.na(page_value(p, "font", "T-2")))
  expect_identical(study_page_value(p, "font"), "Courier New")
  # a report's row with other values keeps them; one left empty goes
  p <- set_page_value(p, "orientation", "landscape", "T-1")
  p <- set_page_value(p, "font", "", "T-1")
  expect_identical(page_value(p, "orientation", "T-1"), "landscape")
  p <- set_page_value(p, "orientation", NA, "T-1")
  expect_identical(p$sheets$page$output_id, NA_character_)
})

test_that("step 4 sets this report's font; blank leaves every report's", {
  local_home()
  p <- add_output(new_planner(), "T-1", description = "a table")
  p <- add_output(p, "T-2", description = "another")
  p <- set_study_page_value(p, "font", "Courier New")
  p <- set_page_value(p, "orientation", "landscape", "T-2")
  create_study("S-RF", planner = p)
  suppressMessages(suppressWarnings(shiny::testServer(server_for("S-RF"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-1")
    out <- output$report_font
    expect_match(out$html, "Every report's: Courier New, 9 pt", fixed = TRUE)
    # the fields as drawn: nothing written
    session$setInputs(rp_font = "", rp_font_size = "")
    expect_identical(rv$p$sheets$page, p$sheets$page)
    session$setInputs(rp_font = "Arial", rp_font_size = "8")
    expect_identical(page_value(rv$p, "font", "T-1"), "Arial")
    expect_identical(page_value(rv$p, "font_size_half_points", "T-1"), "16")
    expect_identical(study_page_value(rv$p, "font"), "Courier New")
    # not a size: said, nothing changed
    session$setInputs(rp_font_size = "big")
    expect_identical(page_value(rv$p, "font_size_half_points", "T-1"), "16")
    # another report: its own row keeps what it had
    session$setInputs(target = "T-2")
    out <- output$report_font
    session$setInputs(rp_font = "", rp_font_size = "")
    expect_identical(page_value(rv$p, "orientation", "T-2"), "landscape")
    expect_true(is.na(page_value(rv$p, "font", "T-2")))
    # back: blank again takes the report's own away
    session$setInputs(target = "T-1")
    out <- output$report_font
    session$setInputs(rp_font = "Arial", rp_font_size = "8")
    session$setInputs(rp_font = "", rp_font_size = "")
    expect_false("T-1" %in% rv$p$sheets$page$output_id)
  })))
})
