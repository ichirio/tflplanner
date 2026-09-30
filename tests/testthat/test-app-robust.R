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
    # the shape rhandsontable sends, broken: rows with no columns to be
    session$setInputs(hot_ard_analyses = list(
      data = list("not a row"),
      changes = list(event = "afterChange",
                     changes = list(list(0L, "method", "continuous", "foo"))),
      params = list(planner_key = key, rClass = "data.frame", rColHeaders = list(),
                    rColClasses = list(), rDataDim = list(1L, 0L))))
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

# the analyses grid as the app shows it for one report (no output_id column)
analyses_payload <- function(rows, key, event = "afterChange") {
  cols <- setdiff(.ard_sheets()$analyses, "output_id")
  list(data = rows,
       changes = list(event = event,
                      changes = list(list(length(rows) - 1L, "method", NULL,
                                          "continuous"))),
       params = list(planner_key = key, rClass = "data.frame",
                     rColHeaders = as.list(cols),
                     rColClasses = stats::setNames(as.list(rep("character", length(cols))),
                                                   cols),
                     rDataDim = list(length(rows), length(cols))))
}

test_that("a row typed into the spare row from a dropdown column is a new row", {
  local_home()
  ard_study()
  suppressWarnings(shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1")
    cols <- setdiff(.ard_sheets()$analyses, "output_id")
    full <- as.list(stats::setNames(rep(NA, length(cols)), cols))
    full[c("analysis_id", "method", "dataset", "variables")] <-
      list("A1", "continuous", "ADSL", "AGE")
    # the new row arrives with fewer cells than the grid has columns
    short <- list(NA, NA, "categorical")[seq_len(match("method", cols))]
    key <- paste("ard", "analyses", "T1", rv$ver, sep = "|")
    session$setInputs(hot_ard_analyses = analyses_payload(
      list(unname(full), short), key))
    a <- ard_rows(rv$p, "analyses", "T1")
    expect_identical(nrow(a), 2L)
    expect_identical(a$method, c("continuous", "categorical"))
  }))
})

test_that("grid rows are padded or cut to the grid's width", {
  rows <- .grid_rows_full(list(list("a"), list("a", "b", "c", "d")), 3L)
  expect_identical(lengths(rows), c(3L, 3L))
  expect_identical(rows[[1L]], list("a", NA, NA))
  expect_identical(rows[[2L]], list("a", "b", "c"))
  expect_identical(.grid_rows_full("x", 0L), "x")
})

test_that("unsaved changes are kept as a draft and offered back", {
  home <- local_home()
  two_studies()
  draft <- .draft_file("S1", home)
  suppressWarnings(shiny::testServer(server_for("S1"), {
    # the editor echoes what the server put there, then the edit comes
    session$setInputs(target = "A", description = "first")
    session$setInputs(description = "first, edited")
    expect_true(session$userData$dirty())
    session$elapse(2500)
    expect_true(file.exists(draft))
  }))
  # the next session on S1 offers it back; taking it restores the edit,
  # still unsaved
  suppressWarnings(shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    expect_false(is.null(rv$draft))
    session$setInputs(draft_restore = 1L)
    expect_identical(rv$p$outputs$description, "first, edited")
    expect_true(session$userData$dirty())
  }))
  # discarding drops it
  suppressWarnings(shiny::testServer(server_for("S1"), {
    session$setInputs(draft_discard = 1L)
    expect_false(file.exists(draft))
  }))
})

test_that("a draft identical to the saved study is dropped, not offered", {
  home <- local_home()
  two_studies()
  .write_draft(open_study("S1"))
  suppressWarnings(shiny::testServer(server_for("S1"), {
    expect_null(session$userData$rv$draft)
  }))
  expect_false(file.exists(.draft_file("S1", home)))
})

test_that("a builder statistic the ARD cannot fill is named with what it lacks", {
  expect_identical(.template_stats("{mean} ({sd:.2f})"), c("mean", "sd"))
  expect_identical(.builder_stats_lacking(c("{mean}", "{p25}, {p75}"), c("mean", "sd")),
                   c("", "p25, p75"))
  # nothing known about the ARD: nothing is said to be missing
  expect_identical(.builder_stats_lacking(c("{p25}"), character()), "")
})

test_that("a figure's group is chosen among short text columns, with labels", {
  lab <- function(x, l) { attr(x, "label") <- l; x }
  cols <- list(TRT01A = lab(c("A", "B"), "Actual Treatment"),
               TRTSDT = as.Date(c("2020-01-01", "2020-02-01")),
               ARMCD = factor(c("X", "Y")),
               TRTDUR = c(10, 20),
               USUBJID = as.character(1:2))
  g <- .group_choices(cols)
  expect_identical(unname(g), c("TRT01A", "ARMCD"))
  expect_identical(names(g)[1L], "TRT01A \u2014 Actual Treatment")
  expect_identical(names(g)[2L], "ARMCD")
})

test_that("the builder of a table with no definition yet writes one, and keeps the session", {
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "T-DM", type = "table")
  s <- create_study("B1", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE))
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  m$variables$label <- NA  # no label: the case that ended the session
  f <- .meta_file(s, "T-DM")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))

  shiny::testServer(server_for("B1"), {
    rv <- session$userData$rv
    bform <- session$userData$bform
    session$setInputs(target = "T-DM", nav = "tables", table_nav = "builder")
    b <- function(x) paste0("b", bform$n, "_", x)
    v <- list(TRT01A = NULL)
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- "AGE"
    v[[b("arms")]] <- m$keys$TRT01A
    v[[b("stats")]] <- c("n", "mean_sd")
    v[[b("dec")]] <- 0
    v[[b("cat")]] <- "npct"
    v[[b("pct")]] <- 1
    v[[b("header")]] <- "keep"
    v[[b("lab1")]] <- ""
    do.call(session$setInputs, v[-1L])
    session$elapse(1000)
    expect_false(session$isClosed())
    expect_identical(sheet_rows(rv$p, "tables", "T-DM")$cols, "TRT01A")
    expect_match(output$builder_preview$html, "Mean")
  })
})

test_that("a table with no ARD says so, and offers the one Preview", {
  local_home()
  create_study("P1", planner = add_output(new_planner(), "T1", type = "table"))
  shiny::testServer(server_for("P1"), {
    session$setInputs(target = "T1", nav = "tables", table_nav = "builder")
    expect_match(output$assist$html, "Not made")
    expect_match(output$assist$html, 'id="fetch2"')
    expect_match(output$builder_note$html, 'id="fetch3"')
  })
})

test_that("analyses of an output that is not a report are pointed out", {
  local_home()
  p <- new_planner()
  p <- set_ard_rows(p, "analyses", "T-X", data.frame(
    analysis_id = "AGE", method = "continuous", variables = "AGE"))
  create_study("O1", planner = p)
  shiny::testServer(server_for("O1"), {
    expect_match(output$ard_check$html, "T-X: analyses of no report yet")
  })
})

test_that("a new study's ID is checked against the folders already there", {
  local_home()
  two_studies()
  root <- withr_tempdir()
  dir.create(file.path(root, "TAKEN"))
  shiny::testServer(server_for("S1"), {
    session$setInputs(ns_root = root, ns_id = "TAKEN")
    expect_match(output$ns_id_check$html, "already there")
    session$setInputs(ns_id = "FREE")
    expect_null(output$ns_id_check$html)
  })
})
