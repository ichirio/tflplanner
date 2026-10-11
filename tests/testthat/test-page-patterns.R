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
  # the first page's sample prints no "(none)": the line is left out
  h <- as.character(.page_sample_html(q, "T1", "S", htmltools::div()))
  expect_false(grepl("(none)", h, fixed = TRUE))
  expect_false(grepl("ACME", h, fixed = TRUE))
  expect_match(h, "(PK)", fixed = TRUE)
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

test_that("titles and footnotes: the heading is the header's {OUTPUT_TITLE}, lines as the bands", {
  p <- pat_planner()
  p$sheets$header <- rbind(p$sheets$header, .normalize_sheet(data.frame(
    output_id = NA, line = "5", center = "{OUTPUT_TITLE}"), "header"))
  p <- set_page_token(p, "T1", "OUTPUT_TITLE", "Demographics")
  h <- as.character(.page_form_ui(p, "T1", identity))
  expect_match(h, 'data-name="OUTPUT_TITLE"', fixed = TRUE)
  expect_match(h, 'value="Demographics"', fixed = TRUE)
  # listed once: in the heading, not again with the tokens
  expect_identical(lengths(regmatches(h, gregexpr("token:OUTPUT_TITLE", h, fixed = TRUE))), 1L)
  expect_match(h, 'data-token="{PROGRAM}"', fixed = TRUE)
  # a header without {OUTPUT_TITLE}: no heading
  expect_false(grepl("Heading", as.character(.page_form_ui(pat_planner(), "T1", identity)), fixed = TRUE))
  # a footnote of this report, over Standard's of the same number
  p$sheets$footnotes <- .normalize_sheet(data.frame(output_id = NA, line = "99",
                                                    left = "Program: {PROGRAM}"), "footnotes")
  q <- set_page_line(p, "footnotes", "T1", "1", left = "n (%) of the subjects")
  d <- page_lines_from(q, "footnotes", "T1")
  expect_identical(d$line, c("1", "99"))
  expect_identical(d$from, c("own", "standard"))
  code <- tflspec::tfl_report_code(.spec_object(q, report_sheets(), character()), "T1")
  expect_true(any(grepl("n (%) of the subjects", code, fixed = TRUE)))
})

test_that("an edit on the form does not draw the pattern's select again", {
  local_home()
  p <- set_report_pattern(pat_planner(), "T1", "Compact")
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1", nav = "make", step = "page")
    bar <- output$page_pattern_bar$html
    session$setInputs(pg_act = list(act = "addline", sheet = "footnotes", col = "", line = "",
                                    name = "", n = 1))
    session$setInputs(pg_edit = list(kind = "line", sheet = "footnotes", col = "", line = "1",
                                     part = "left", name = "", value = "A note", n = 2))
    # the bar is the same drawing: its select sends nothing new
    expect_identical(output$page_pattern_bar$html, bar)
    expect_identical(report_pattern(rv$p, "T1"), "Compact")
    expect_identical(page_lines_from(rv$p, "footnotes", "T1")$left, "A note")
  })
})

test_that("a pattern made, renamed, removed; one used is not removed", {
  p <- add_page_pattern(pat_planner(), "Wide")
  expect_identical(page_patterns(p), c("Compact", "Wide"))
  expect_error(add_page_pattern(p, "Wide"), "already")
  expect_error(add_page_pattern(p, "standard"), "Standard is the blank rows")
  expect_error(add_page_pattern(p, "2col"), "a letter, then")
  p <- set_report_pattern(p, "T1", "Compact")
  q <- rename_page_pattern(p, "Compact", "PK")
  expect_identical(page_patterns(q), c("PK", "Wide"))
  expect_identical(report_pattern(q, "T1"), "PK")
  expect_identical(page_value_from(q, "page", "paper_size", "T1")$from, "pattern")
  expect_error(remove_page_pattern(q, "PK"), "used by T1")
  expect_identical(page_patterns(remove_page_pattern(q, "Wide")), "PK")
  # the workbook reads: tflspec takes the patterns as written
  sp <- .spec_object(q, report_sheets(), character())
  expect_true(any(grepl("ABC PK", tflspec::tfl_report_code(sp, "T1"), fixed = TRUE)))
})

test_that("the form edits a pattern (its own over Standard) or Standard itself", {
  p <- pat_planner()
  # a pattern: its values its own, the rest Standard's
  expect_identical(page_value_from(p, "page", "paper_size", "@Compact"), list(value = "A4", from = "own"))
  expect_identical(page_value_from(p, "page", "orientation", "@Compact")$from, "standard")
  q <- set_page_cell(p, "page", "orientation", "@Compact", "portrait")
  expect_identical(sheet_rows(q, "page", "@Compact")$orientation, "portrait")
  q <- omit_page_line(q, "header", "@Compact", "1")
  expect_true(page_lines_from(q, "header", "@Compact")$omitted[1])
  # Standard: the blank rows themselves
  expect_identical(page_value_from(p, "page", "paper_size", NA), list(value = "letter", from = "own"))
  q <- set_page_cell(p, "page", "paper_size", NA, "legal")
  expect_identical(sheet_rows(q, "page", NA)$paper_size, "legal")
  q <- set_page_line(p, "header", NA, "3", left = "{OUTPUT_LABEL}")
  expect_identical(sheet_rows(q, "header", NA)$line, c("1", "2", "3"))
  q <- drop_page_line(q, "header", NA, "1")
  expect_identical(sheet_rows(q, "header", NA)$line, c("2", "3"))
  # the form's words
  h <- as.character(.page_form_ui(p, "@Compact", identity))
  expect_match(h, 'data-pg-target="@Compact"', fixed = TRUE)
  expect_match(h, "this pattern", fixed = TRUE)
  expect_match(as.character(.page_form_ui(p, NA, identity)), 'data-pg-target=""', fixed = TRUE)
})

test_that("the pattern dialog: a new pattern, edited by its target, renamed", {
  local_home()
  create_study("S1", planner = pat_planner())
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1", nav = "make", step = "page", pat_edit = 1)
    session$setInputs(pat_name = "Wide", pat_new = 1)
    expect_true("Wide" %in% page_patterns(rv$p))
    expect_match(output$pat_form$html, 'data-pg-target="@Wide"', fixed = TRUE)
    session$setInputs(pg_edit = list(target = "@Wide", kind = "cell", sheet = "page",
                                     col = "orientation", line = "", part = "", name = "",
                                     value = "portrait", n = 1))
    expect_identical(sheet_rows(rv$p, "page", "@Wide")$orientation, "portrait")
    # the dialog stays on the pattern edited, and counts its reports
    expect_match(output$pat_form$html, 'data-pg-target="@Wide"', fixed = TRUE)
    expect_match(output$pat_count$html, "Reports using it: 0", fixed = TRUE)
    # Standard, from the dialog
    session$setInputs(pat_pick = "Standard")
    session$setInputs(pg_edit = list(target = "", kind = "cell", sheet = "page",
                                     col = "margin_top_in", line = "", part = "", name = "",
                                     value = "1", n = 2))
    expect_identical(sheet_rows(rv$p, "page", NA)$margin_top_in, "1")
    # another report's id is not a target here
    session$setInputs(pg_edit = list(target = "T2", kind = "cell", sheet = "page",
                                     col = "font", line = "", part = "", name = "",
                                     value = "Arial", n = 3))
    expect_identical(nrow(sheet_rows(rv$p, "page", "T2")), 0L)
    session$setInputs(pat_pick = "Wide", pat_name = "Landscape", pat_rename = 1)
    expect_true("Landscape" %in% page_patterns(rv$p))
    session$setInputs(pat_remove = 1)
    expect_false("Landscape" %in% page_patterns(rv$p))
  })
})
