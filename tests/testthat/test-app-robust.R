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
  expect_identical(.ard_closed_columns,
                   c("data", "dataset", "population_id", "denominator", "from",
                     "subjects"))
})

test_that("the SPEC | Code | Result tabs are not a card around the sheets' card", {
  html <- as.character(app_ui())
  at <- regexpr('id="table_right"', html, fixed = TRUE)
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
    short <- as.list(rep(NA, match("method", cols)))
    short[[match("method", cols)]] <- "categorical"
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
    session$setInputs(target = "T-DM", nav = "make", step = "content", content_nav = "content", table_nav = "builder")
    b <- function(x) paste0("b", bform$n, "_", x)
    v <- list(TRT01A = NULL)
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- "AGE"
    v[[b("arms_TRT01A")]] <- m$keys$TRT01A
    v[[b("rows")]] <- c("n", "Mean (SD)")
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
    session$setInputs(target = "T1", nav = "make", step = "content", content_nav = "content", table_nav = "builder")
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

test_that("a step's right is SPEC | Code | Result, the form on its left", {
  ui <- htmltools::tagQuery(app_ui())
  vals <- function(id) {
    links <- ui$find(paste0("#", id))$find("a")$selectedTags()
    v <- vapply(links, function(x) x$attribs[["data-value"]] %||% "", "")
    unname(v[nzchar(v)])
  }
  for (id in c("ard_right", "table_right", "lf_right", "uc_right", "page_right")) {
    expect_identical(vals(id), c("spec", "code", "result"), info = id)
  }
  # the table: the sheets in SPEC, the preview in Result, the builder beside
  panes <- ui$find("#table_right")$parent()$find(".tab-pane")$selectedTags()
  pane <- function(v) as.character(Filter(function(x) identical(x$attribs[["data-value"]], v),
                                          panes)[[1L]])
  expect_true(grepl("hot_tables", pane("spec"), fixed = TRUE))
  expect_true(grepl("builder_preview", pane("result"), fixed = TRUE))
  expect_true(grepl("program_table", pane("code"), fixed = TRUE))
  expect_false(grepl("builder_form", pane("spec"), fixed = TRUE))
  # code to read: no field to edit it in
  expect_false(grepl("textarea", pane("code"), fixed = TRUE))
  # the page: its program is its Code (no sub-tab of its own)
  expect_identical(length(ui$find("#page_nav")$selectedTags()), 0L)
})

test_that("the top tabs are the flow, and a report is made in its steps", {
  ui <- htmltools::tagQuery(app_ui())
  vals <- function(id) {
    links <- ui$find(paste0("#", id))$find("a")$selectedTags()
    v <- vapply(links, function(x) x$attribs[["data-value"]] %||% "", "")
    unname(v[nzchar(v)])
  }
  expect_identical(vals("nav"), c("study", "data", "outputs", "make", "review", "results"))
  expect_identical(vals("step"), c("ard", "content", "page"))
  expect_identical(vals("content_nav"), c("content", "code"))
  expect_identical(vals("page_right"), c("spec", "code", "result"))
  # the steps' names are in one place
  expect_identical(names(.step_labels), c("ard", "content", "page"))
  html <- as.character(app_ui())
  # each kind's content shows for its kind only
  for (k in c("table", "listing", "figure")) {
    expect_true(grepl(sprintf("output.report_kind == &#39;%s&#39;", k), html, fixed = TRUE) ||
                  grepl(sprintf("output.report_kind == '%s'", k), html, fixed = TRUE))
  }
})

test_that("the page sample puts a report's lines over the study defaults", {
  p <- new_planner()
  p <- set_sheet_rows(p, "header", NA, data.frame(
    line = c("1", "2"), left = c("Company", "Protocol: {STUDY_ID}"),
    right = c("DRAFT", "Page {PAGE} of {TOTAL_PAGES}")))
  p <- add_output(p, "T1")
  p <- set_sheet_rows(p, "header", "T1", data.frame(line = "3", center = "Table 1"))
  h <- as.character(.page_sample_html(p, "T1", "S-01", htmltools::div("BODY")))
  expect_match(h, "Protocol: S-01")
  expect_match(h, "Page 1 of N")
  expect_match(h, "Table 1")
  expect_true(regexpr("Company", h) < regexpr("Table 1", h))
  expect_match(h, "BODY")
})

test_that("with no study, the app offers the ways to start, the sample first", {
  skip_on_cran()
  local_home()
  shiny::testServer(function(input, output, session)
    app_server(input, output, session, NULL), {
    expect_match(output$welcome$html, 'id="try_sample"')
    expect_match(output$welcome$html, 'id="start_empty"')
    expect_match(output$welcome$html, "New study...", fixed = TRUE)
    expect_null(output$save_btn$html)
    expect_identical(output$n_studies, "0")
  })
})

test_that("with no study, both ways to start open the New study dialog", {
  skip_on_cran()
  local_home()
  shiny::testServer(function(input, output, session)
    app_server(input, output, session, NULL), {
    # the sample: the dialog opens with the sample chosen and its ID
    # suggested, to be changed there
    session$setInputs(try_sample = 1L)
    session$setInputs(ns_from = "sample", ns_id = "TRAIN-01",
                      ns_root = studies_root())
    expect_null(output$ns_id_check)
    session$setInputs(ns_id = "bad id!")
    expect_match(as.character(output$ns_id_check$html), "letters, digits")
    # New study...: the same dialog, an empty study made under its ID
    session$setInputs(start_empty = 1L)
    session$setInputs(ns_from = "empty", ns_id = "E-01",
                      ns_root = studies_root(), ns_ok = 1L)
    expect_identical(list_studies()$study_id, "E-01")
  })
})

test_that("the next steps in Japanese name the tabs as the Japanese app does", {
  st <- .strings()
  ja <- function(en) st$ja[match(en, st$en)]
  steps <- ja(c("Open %s: its Content builds the table in words, with the table as it will print beside it.",
                "Runs: preview every report and open its RTF.", "Go to Runs"))
  expect_false(any(grepl("Runs|Content", steps)))
  expect_true(all(grepl(ja("Runs"), steps[2:3], fixed = TRUE)))
  # the tab reads "内容 (content)"; the step names its words
  expect_match(steps[1], sub(" [(]content[)]$", "", ja("Content")), fixed = TRUE)
})

test_that("New study and Register a folder show with no study; the rest waits for one", {
  skip_if_not_installed("xml2")
  doc <- xml2::read_html(as.character(app_ui()))
  # the buttons inside a panel shown only when there is a study
  cond <- xml2::xml_find_all(
    doc, "//*[@data-display-if='output.n_studies > 0']//button")
  waits <- xml2::xml_attr(cond, "id")
  expect_false(any(c("new_study", "register") %in% waits))
  expect_true(all(c("open_study", "unregister", "refresh_studies") %in% waits))
  all_ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//button"), "id")
  expect_true(all(c("new_study", "register") %in% all_ids))
  # a panel that hides itself has no Bootstrap display class to override it
  hide <- xml2::xml_find_all(doc, "//*[@data-display-if]")
  cls <- xml2::xml_attr(hide, "class")
  expect_false(any(grepl("(^| )d-(flex|inline|block|grid)", cls[!is.na(cls)])))
})

test_that("the next steps show once after the sample, and stay closed", {
  local_home()
  two_studies()
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    expect_null(output$next_steps$html)
    rv$next_steps <- TRUE
    session$flushReact()
    expect_match(output$next_steps$html, 'id="steps_ard"')
    session$setInputs(steps_close = 1)
    expect_null(output$next_steps$html)
    expect_true(file.exists(file.path(tflplanner_home(), "next_steps_seen")))
    rv$next_steps <- TRUE
    session$flushReact()
    expect_null(output$next_steps$html)
  })
})

test_that("every ARD method has its name and note in Japanese, else English", {
  m <- tflspec::tfl_ard_methods()
  for (x in m$method) {
    expect_false(identical(tr(paste0("method:", x), "ja"), paste0("method:", x)), info = x)
    expect_false(identical(tr(paste0("method-note:", x), "ja"), paste0("method-note:", x)), info = x)
  }
  expect_identical(tr("method:continuous", "ja"), "\u8981\u7d04\u7d71\u8a08\u91cf")
})

test_that("a new session opens the study opened last", {
  skip_on_cran()
  local_home()
  two_studies()
  .set_config("last_study", "S1")
  shiny::testServer(function(input, output, session)
    app_server(input, output, session, NULL), {
    expect_identical(session$userData$rv$study$meta$study_id, "S1")
    expect_null(output$welcome$html)
  })
})

test_that("a preview's spanning header cells span their columns", {
  pg <- list(data = data.frame(a = "x", b = "1", c = "2", d = "3"),
             col_header = list(
               list(list(from = 1, to = 1, label = "SOC\nPT"),
                    list(from = 3, to = 4, label = "Active")),
               c("", "Placebo", "Low", "High")))
  h <- as.character(preview_html(list(pg)))
  expect_false(grepl("list(from", h, fixed = TRUE))
  expect_match(h, 'colspan="2"[^>]*>Active')
  expect_match(h, "SOC<br>PT")
  expect_match(h, "Placebo")
})

test_that("tflspec's column help (English) is shown in the app's language", {
  d <- tflspec::tfl_spec_columns()
  ja <- tr(unique(d$description), lang = "ja")
  # every description has its translation
  expect_identical(sum(ja == unique(d$description)), 0L)
  expect_identical(tr("The variable's name.", lang = "ja"), "変数名。")
})

test_that("a report's kind says where it is made, and the tabs it has nothing on", {
  expect_identical(names(.type_moves), c("table", "figure", "listing", "user"))
  expect_identical(names(.type_moves$table), c("ard", "tables"))
  expect_true(all(unlist(lapply(.type_moves, names)) %in%
                    c("ard", "tables", "designer", "lf", "usercode")))
  expect_identical(.type_idle_tabs$figure, "ard")
  expect_length(.type_idle_tabs$table, 0L)
  # every kind of report has its moves and its idle tabs
  expect_setequal(names(.type_moves), report_types())
  expect_setequal(names(.type_idle_tabs), report_types())
})

test_that("the report list's buttons are above the list, the marks have a legend", {
  html <- as.character(app_ui())
  # Add and Take in a TOC before the table (below 200 rows no one found them)
  at <- function(x) regexpr(x, html, fixed = TRUE)[[1L]]
  expect_lt(at('id="add"'), at('id="outputs"'))
  expect_lt(at('id="toc_new"'), at('id="outputs"'))
  # the runs table says it is being read until it is drawn
  expect_lt(at('id="status_loading"'), at('id="status"'))
  # what the marks after a report's title mean
  for (m in .report_state_marks) expect_true(grepl(m, html, fixed = TRUE))
  expect_match(html, "The marks", fixed = TRUE)
})

test_that("a copy of the sample is listed at once; its run goes on in the background", {
  skip_on_cran()
  local_home()
  alive <- TRUE
  px <- list(is_alive = function() alive, get_exit_status = function() 0L,
             kill = function() invisible(TRUE))
  started <- NULL
  local_mocked_bindings(run_batch = function(study, parts, ...) {
    started <<- list(id = study$meta$study_id, parts = parts)
    px
  })
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  shiny::testServer(function(input, output, session)
    app_server(input, output, session, NULL), {
    rv <- session$userData$rv
    session$setInputs(try_sample = 1L)
    session$setInputs(ns_from = "sample", ns_id = "TRAIN-01",
                      ns_root = studies_root(), ns_ok = 1L)
    # copied and listed, its official run started and not waited for
    expect_identical(list_studies()$study_id, "TRAIN-01")
    expect_identical(started, list(id = "TRAIN-01", parts = c("ard", "tfl")))
    expect_identical(rv$job_study, "TRAIN-01")
    ids <- function() .study_list_ids(list_studies(), "TRAIN-01",
                                      if (!is.null(rv$job)) rv$job_study)
    expect_identical(ids(), "TRAIN-01 \u25cf (running)")
    # done: the mark goes
    alive <<- FALSE
    session$elapse(1500)
    expect_null(rv$job)
    expect_identical(ids(), "TRAIN-01 \u25cf")
  })
})

test_that("step 3: the page's sheets on the left, Result first, SPEC read only", {
  ui <- as.character(app_ui())
  # the sheets are the input (left), not a tab of the right
  i_sheet <- regexpr("page_sheet", ui, fixed = TRUE)
  i_tabs <- regexpr("page_right", ui, fixed = TRUE)
  expect_true(i_sheet > 0 && i_tabs > i_sheet)
  # Result shown first; the page at its actual size with a fit toggle
  expect_match(ui, "page_fit", fixed = TRUE)
  expect_match(ui, "rp-page-wrap", fixed = TRUE)
  expect_match(ui, "page_spec_view", fixed = TRUE)
})

test_that("an input left by an earlier session is not read as this form's (the Total switch, #331)", {
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "T-DM", type = "table")
  s <- create_study("B2", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE))
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, "T-DM")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  shiny::testServer(server_for("B2"), {
    rv <- session$userData$rv
    bform <- session$userData$bform
    session$setInputs(target = "T-DM", nav = "make", step = "content",
                      content_nav = "content", table_nav = "builder")
    # the numbers start apart from a session's before (1, 2, 3 ...)
    expect_gt(bform$n, 100L)
    b <- function(x) paste0("b", bform$n, "_", x)
    v <- list()
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- "AGE"
    do.call(session$setInputs, v)
    session$elapse(1000)
    total <- function() sheet_rows(rv$p, "tables", "T-DM")$total
    expect_true(is.na(total()))
    # a reconnecting browser sends the inputs of the session before: an
    # earlier form's switch, on -- not this form's, nothing changes
    old <- list(TRUE, TRUE, TRUE)
    names(old) <- paste0("b", 1:3, "_total_on")
    do.call(session$setInputs, old)
    session$elapse(1000)
    expect_true(is.na(total()))
    # this form's own switch does
    on <- list(TRUE)
    names(on) <- b("total_on")
    do.call(session$setInputs, on)
    session$elapse(1000)
    expect_identical(total(), "Total")
  })
})
