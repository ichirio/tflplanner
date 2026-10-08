# Every report's header defined once: the study's words are tokens set in
# the study tab, a report's own (label, title, analysis set) its tokens,
# from a TOC; programs/tfl/report_setup.R writes the header once.

hdr_toc_file <- function(rows) {
  f <- tempfile(fileext = ".csv")
  writeLines(c("No.,Kind,Label,Title,Population", rows), f)
  f
}
hdr_toc_map <- c(output_id = "No.", type = "Kind", label = "Label",
                 title = "Title", population = "Population")

test_that("a study token is one row of the tokens sheet, set and cleared", {
  x <- new_planner()
  expect_identical(study_token(x, "COMPANY"), NA_character_)
  x <- set_study_token(x, "COMPANY", "Acme")
  x <- set_study_token(x, "STUDY_ID", "ABC-1")
  expect_identical(study_token(x, "COMPANY"), "Acme")
  x <- set_study_token(x, "COMPANY", "Acme Pharma")
  expect_identical(sum(x$sheets$tokens$name == "COMPANY"), 1L)
  expect_identical(study_token(x, "COMPANY"), "Acme Pharma")
  x <- set_study_token(x, "COMPANY", "")
  expect_identical(study_token(x, "COMPANY"), NA_character_)
  expect_identical(study_token(x, "STUDY_ID"), "ABC-1")
})

test_that("a new study's header is the package's, its words tokens", {
  x <- .standard_planner("ABC-1")
  h <- sheet_rows(x, "header", NA)
  expect_identical(h$left[1:2], c("{COMPANY}", "PROTOCOL: {STUDY_ID}"))
  expect_identical(h$center[4:6], c("{OUTPUT_LABEL}", "{OUTPUT_TITLE}",
                                    "<{OUTPUT_POPULATION}>"))
  expect_identical(study_token(x, "STUDY_ID"), "ABC-1")
  expect_identical(study_token(x, "ANALYSIS_TYPE"), "Final Analysis")
  expect_true(.uses_report_setup(x))
  # the setup holds them once; a report's program sources it
  x <- add_output(x, "T-14-1-1", type = "table")
  set <- paste(report_setup_code(x), collapse = "\n")
  expect_match(set, "rtfreporter.tokens", fixed = TRUE)
  expect_match(set, "STUDY_ID = \"ABC-1\"", fixed = TRUE)
  expect_match(set, "study_header <- rtf_header(", fixed = TRUE)
  prog <- paste(program_code(x, "T-14-1-1"), collapse = "\n")
  expect_match(prog, "source(\"programs/tfl/report_setup.R\")", fixed = TRUE)
  expect_match(prog, "header = study_header", fixed = TRUE)
  expect_match(prog, "OUTPUT_LABEL = \"Table 14.1.1\"", fixed = TRUE)
  expect_false(grepl("ABC-1", sub("^.*report_id <-", "", prog), fixed = TRUE))
  # a study with no tokens of its own: its header in each program, and
  # report_setup.R sourced all the same (the study setup, #268)
  y <- add_output(new_planner(), "T-1", type = "table")
  expect_false(.uses_report_setup(y))
  expect_true(grepl("report_setup", paste(program_code(y, "T-1"), collapse = "\n"),
                    fixed = TRUE))
})

test_that("a TOC gives a report its own tokens, not the title lines the header has", {
  x <- .standard_planner("ABC-1")
  sp <- tflspec::tfl_read_toc(hdr_toc_file(c(
    "T-14-1-1,Table,Table 14.1.1,Demographics,Safety Analysis Set",
    "T-14-2-1,Table,,Vital signs,")), map = hdr_toc_map)
  sp <- .toc_titles_in_header(x, sp)
  expect_identical(nrow(sp$titles[sp$titles$output_id %in% "T-14-1-1", ]), 0L)
  expect_identical(.toc_titles_in_header(x, sp), sp)   # once
  y <- toc_apply(x, sp, toc_changes(x, sp))
  tk <- sheet_rows(y, "tokens", "T-14-1-1")
  expect_identical(tk$value[match(c("OUTPUT_LABEL", "OUTPUT_TITLE", "OUTPUT_POPULATION"),
                                  tk$name)],
                   c("Table 14.1.1", "Demographics", "Safety Analysis Set"))
  expect_identical(nrow(sheet_rows(y, "titles", "T-14-1-1")), 0L)
  t2 <- sheet_rows(y, "tokens", "T-14-2-1")
  expect_identical(t2$name, "OUTPUT_TITLE")
  # again, the TOC changed: a value not edited here follows it, an edited one stays
  last <- toc_snapshot(sp, 0L)
  y$sheets$tokens$value[y$sheets$tokens$output_id %in% "T-14-2-1"] <- "Vital signs (edited)"
  sp2 <- .toc_titles_in_header(y, tflspec::tfl_read_toc(hdr_toc_file(c(
    "T-14-1-1,Table,Table 14.1.1,Demographics and baseline,Safety Analysis Set",
    "T-14-2-1,Table,,Vital signs by visit,")), map = hdr_toc_map))
  z <- toc_apply(y, sp2, toc_changes(y, sp2, last), last = last)
  v <- function(id, k) {
    d <- sheet_rows(z, "tokens", id)
    d$value[d$name == k]
  }
  expect_identical(v("T-14-1-1", "OUTPUT_TITLE"), "Demographics and baseline")
  expect_identical(v("T-14-2-1", "OUTPUT_TITLE"), "Vital signs (edited)")
})

test_that("a study whose header says no report tokens gets none from a TOC", {
  x <- new_planner()
  sp <- tflspec::tfl_read_toc(hdr_toc_file(
    "T-1,Table,Table 1,Demographics,Safety Analysis Set"), map = hdr_toc_map)
  # its titles stay title lines, and no tokens rows are written
  expect_identical(.toc_titles_in_header(x, sp)$titles, sp$titles)
  y <- toc_apply(x, sp, toc_changes(x, sp))
  expect_identical(nrow(sheet_rows(y, "tokens", "T-1")), 0L)
  expect_identical(sheet_rows(y, "titles", "T-1")$center,
                   c("Demographics", "Safety Analysis Set"))
})
