# Page patterns (the page-patterns design, part A): a report chooses a
# pattern; step 3's form shows each value with where it comes from and
# writes only the report's own rows.

pat_planner <- function() {
  p <- add_output(add_output(new_planner(), "T1"), "T2")
  p$sheets$header <- .normalize_sheet(data.frame(
    output_id = c(NA, NA, "@Compact"), line = c("1", "2", "2"),
    left = c("ACME", "{STUDY}", "{STUDY} (PK)")), "header")
  p$sheets$page <- .normalize_sheet(data.frame(
    output_id = c(NA, "@Compact"), paper_size = c("letter", "A4"),
    orientation = c("landscape", NA)), "page")
  p$sheets$tokens <- .normalize_sheet(data.frame(
    output_id = c(NA, "@Compact"), name = "STUDY", value = c("ABC", "ABC PK")), "tokens")
  p
}

test_that("the patterns are not reports; a report chooses one", {
  p <- pat_planner()
  expect_identical(page_patterns(p), "Compact")
  expect_identical(output_ids(p), c("T1", "T2"))
  expect_error(add_output(p, "@X"), "page pattern's name")
  p <- set_report_pattern(p, "T1", "Compact")
  expect_identical(report_pattern(p, "T1"), "Compact")
  expect_true(is.na(report_pattern(p, "T2")))
  expect_error(set_report_pattern(p, "T1", "Wide"), "No page pattern")
  expect_true(is.na(report_pattern(set_report_pattern(p, "T1", "Standard"), "T1")))
})

test_that("a value comes from the report, its pattern or Standard; the same value is no difference", {
  p <- set_report_pattern(pat_planner(), "T1", "Compact")
  expect_identical(page_value_from(p, "page", "paper_size", "T1"), list(value = "A4", from = "pattern"))
  expect_identical(page_value_from(p, "page", "orientation", "T1")$from, "standard")
  expect_identical(page_value_from(p, "page", "paper_size", "T2")$from, "standard")
  # the pattern's own value typed: nothing written
  q <- set_page_cell(p, "page", "paper_size", "T1", "A4")
  expect_identical(nrow(sheet_rows(q, "page", "T1")), 0L)
  q <- set_page_cell(p, "page", "paper_size", "T1", "letter")
  expect_identical(page_value_from(q, "page", "paper_size", "T1"), list(value = "letter", from = "own"))
  # back to the pattern: the report's row goes
  q <- set_page_cell(q, "page", "paper_size", "T1", NA)
  expect_identical(nrow(sheet_rows(q, "page", "T1")), 0L)
})

test_that("a band's lines: changed, left out and back, over the pattern's", {
  p <- set_report_pattern(pat_planner(), "T1", "Compact")
  d <- page_lines_from(p, "header", "T1")
  expect_identical(d$left, c("ACME", "{STUDY} (PK)"))
  expect_identical(d$from, c("standard", "pattern"))
  q <- omit_page_line(p, "header", "T1", "1")
  expect_true(page_lines_from(q, "header", "T1")$omitted[1])
  code <- tflspec::tfl_report_code(.spec_object(q, report_sheets(), character()), "T1")
  expect_false(any(grepl("ACME", code, fixed = TRUE)))
  expect_true(any(grepl("{STUDY} (PK)", code, fixed = TRUE)))
  q <- drop_page_line(q, "header", "T1", "1")
  expect_identical(page_lines_from(q, "header", "T1")$from, c("standard", "pattern"))
  q <- set_page_line(p, "header", "T1", "3", left = "PK only")
  expect_identical(page_lines_from(q, "header", "T1")$from, c("standard", "pattern", "own"))
  expect_identical(.page_new_line(q, "footer", "T1"), "1")
  expect_identical(.page_new_line(q, "header", "T1"), "4")
})

test_that("the tokens and the grids' inherited rows follow the pattern", {
  p <- set_report_pattern(pat_planner(), "T1", "Compact")
  expect_identical(page_tokens_from(p, "T1")$value, "ABC PK")
  expect_identical(page_tokens_from(p, "T2")$value, "ABC")
  q <- set_page_token(p, "T1", "STUDY", "ABC PK-2")
  expect_identical(page_tokens_from(q, "T1")$from, "own")
  expect_identical(page_tokens_from(set_page_token(q, "T1", "STUDY", NA), "T1")$from, "pattern")
  inh <- inherited_rows(p, "header", "T1")
  expect_identical(inh$left[order(inh$line)], c("ACME", "{STUDY} (PK)"))
  expect_identical(inherited_rows(p, "page", "T1")$paper_size, "A4")
})

test_that("step 3: the pattern chosen, a value written, a line left out, from the form", {
  local_home()
  create_study("S1", planner = pat_planner())
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1", nav = "make", step = "page")
    expect_match(output$page_pattern_bar$html, "Compact", fixed = TRUE)
    session$setInputs(page_pattern = "Compact")
    expect_identical(report_pattern(rv$p, "T1"), "Compact")
    h <- output$page_form$html
    expect_match(h, "pattern Compact", fixed = TRUE)
    expect_match(h, "{STUDY} (PK)", fixed = TRUE)
    session$setInputs(pg_edit = list(kind = "cell", sheet = "page", col = "margin_top_in",
                                     line = "", part = "", name = "", value = "0.5", n = 1))
    expect_identical(page_value_from(rv$p, "page", "margin_top_in", "T1")$value, "0.5")
    session$setInputs(pg_edit = list(kind = "cell", sheet = "page", col = "font_size_half_points",
                                     line = "", part = "", name = "", value = "8", n = 2))
    expect_identical(page_value_from(rv$p, "page", "font_size_half_points", "T1")$value, "16")
    expect_match(output$page_form$html, "Back to the pattern", fixed = TRUE)
    session$setInputs(pg_act = list(act = "omit", sheet = "header", col = "", line = "1",
                                    name = "", n = 3))
    expect_true(page_lines_from(rv$p, "header", "T1")$omitted[1])
    session$setInputs(pg_act = list(act = "own", sheet = "header", col = "", line = "2",
                                    name = "", n = 4))
    session$setInputs(pg_edit = list(kind = "line", sheet = "header", col = "", line = "2",
                                     part = "left", name = "", value = "PK study", n = 5))
    d <- page_lines_from(rv$p, "header", "T1")
    expect_identical(d$left[d$line == "2"], "PK study")
    expect_identical(d$from[d$line == "2"], "own")
    # the other report: Standard, untouched
    expect_identical(nrow(sheet_rows(rv$p, "header", "T2")), 0L)
  })
})
