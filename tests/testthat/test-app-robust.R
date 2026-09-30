# What a person must never meet in the app: a session that stops on a bad
# edit, a tab that stays blank, an R error message instead of a sentence.
# (GUI review iter01: P0-1, P0-2, P1-4.)

ard_study <- function() {
  p <- add_output(new_planner(), "T1", description = "a table")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = "A1", method = "continuous",
    dataset = "ADSL", variables = "AGE", stringsAsFactors = FALSE),
    "analyses")
  create_study("S1", planner = p)
}

test_that("an ARD grid edit that cannot be read back leaves the session going", {
  local_home()
  ard_study()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1")
    before <- rv$p
    drawn <- session$userData$grids_drawn()
    key <- paste("ard", "analyses", "T1", rv$ver, sep = "|")
    # the shape rhandsontable sends, broken: data rows that are not rows
    session$setInputs(hot_ard_analyses = list(
      data = list("not a row"),
      changes = list(event = "afterChange",
                     changes = list(list(0L, "method", "continuous", "foo"))),
      params = list(planner_key = key, rColHeaders = list("method"),
                    rColClasses = list(method = "character"),
                    rDataDim = list(1L, 1L))))
    # still here, the definition as it was, and the grids drawn again
    expect_identical(rv$p, before)
    expect_identical(session$userData$grids_drawn(), drawn + 1L)
  }))
})

test_that("closed choice columns refuse other values; open ones take them", {
  d <- data.frame(dataset = "ADSL", method = "continuous",
                  stringsAsFactors = FALSE)
  h <- .grid(d, "analyses", "k",
             choices = list(dataset = c("ADSL", "ADAE"), method = "continuous"),
             closed = "dataset")
  cols <- h$x$columns
  by_name <- stats::setNames(cols, unlist(h$x$colHeaders))
  expect_true(isTRUE(by_name$dataset$strict))
  expect_false(isTRUE(by_name$dataset$allowInvalid))
  expect_false(isTRUE(by_name$method$strict))
  expect_identical(.ard_closed_columns, c("dataset", "population_id"))
})

test_that("the Tables tab's sub-tabs are not a card around the sheets' card", {
  html <- as.character(app_ui())
  at <- regexpr('id="table_nav"', html, fixed = TRUE)
  expect_gt(at, 0L)
  # a card navset puts its nav inside a card-header; a plain one does not
  before <- substr(html, max(1L, at - 200L), at)
  expect_false(grepl("card-header", before, fixed = TRUE))
})

test_that("R's error output becomes one line and, when known, the next step", {
  out <- c("Loading ...",
           "Error in library(rtfreporter) : there is no package called 'rtfreporter'",
           "Calls: suppressPackageStartupMessages -> withCallingHandlers -> library",
           "Execution halted")
  why <- .first_error(out)
  expect_identical(why, "there is no package called 'rtfreporter'")
  expect_match(.problem_hint(why), "install.packages\\(\"rtfreporter\"\\)")
  expect_identical(.first_error("Error: object 'x' not found"), "object 'x' not found")
  expect_identical(.first_error(c("Error in some_long_call(a, b) :", "  the message")),
                   "the message")
  expect_true(is.na(.first_error("all went well")))
  expect_identical(.problem_hint(NA_character_), "")
})

test_that("a failed ARD run is one sentence on screen and the output in the detail", {
  local_home()
  p <- add_output(new_planner(), "T1",
                  data_code = "stop(\"no ADSL here\")")
  s <- create_study("S1", planner = p)
  e <- tryCatch(fetch_ard(s, "T1"), error = function(e) e)
  expect_s3_class(e, "tflplanner_problem")
  expect_match(conditionMessage(e), "^The ARD code of 'T1' did not run: no ADSL here\\.")
  expect_false(grepl("Execution halted", conditionMessage(e), fixed = TRUE))
  expect_true(any(grepl("Execution halted", e$detail, fixed = TRUE)))
})

test_that("a data preview reads one file, and says so otherwise", {
  expect_error(read_data_head(character()), "`path` is one file")
  expect_error(read_data_head(c("a.rds", "b.rds")), "`path` is one file")
})

test_that("a new study's ID is checked where it is typed", {
  local_home()
  two_studies()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    session$setInputs(new_study = 1L)
    session$setInputs(ns_id = "", ns_from = "empty",
                      ns_root = studies_root())
    expect_match(as.character(output$ns_id_check$html), "Give the study an ID")
    session$setInputs(ns_id = "bad id!")
    expect_match(as.character(output$ns_id_check$html), "letters, digits")
    session$setInputs(ns_id = "S2")
    expect_match(as.character(output$ns_id_check$html), "already registered")
    session$setInputs(ns_id = "", ns_ok = 1L)
    expect_false("" %in% list_studies()$study_id)
    session$setInputs(ns_id = "S3")
    expect_null(output$ns_id_check)
  }))
})

test_that("every sentence the app shows has its Japanese", {
  src <- test_path("..", "..", "R", c("app.R", "app_designer.R"))
  skip_if_not(all(file.exists(src)), "source not available (installed package)")
  code <- unlist(lapply(src, readLines, encoding = "UTF-8", warn = FALSE))
  lits <- unlist(regmatches(code, gregexpr('\\bt\\("((?:[^"\\\\]|\\\\.)*)"\\)', code, perl = TRUE)))
  lits <- unique(eval(parse(text = paste0("c(", paste(sub("^t", "", lits), collapse = ","), ")"))))
  have <- .strings()$en
  expect_identical(setdiff(lits, have), character(0))
})
