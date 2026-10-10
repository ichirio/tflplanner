# The Review tab (#288 phase 2): one review the app shares, its counts
# where the user is, the filters, a click going to the item, the data read
# on demand, the deep check moved here from the Runs tab.

.rv_ui_study <- function() {
  p <- add_output(new_planner(), "T1", type = "table", description = "one",
                  population = "SAF")
  p <- add_output(p, "T2", type = "table", description = "two")
  p <- add_output(p, "L1", type = "listing", description = "a listing", at = "end")
  p$ard$populations <- .normalize_ard_sheet(
    data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    "populations")
  p$ard$datasets <- .normalize_ard_sheet(
    data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"), "datasets")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = c("AGE", "SEX"), method = c("continuous", "categorical"),
    dataset = "ADSL", population_id = "SAF", by = "TRT01A",
    variables = c("AGE", "SEX | TRT01A")), "analyses")
  p <- set_sheet_rows(p, "tokens", "T1", data.frame(name = "OUTPUT_TITLE", value = "Age"))
  p <- set_sheet_rows(p, "tables", "T1", data.frame(cols = "TRT01A"))
  p
}

test_that("one review for the app: the tab, the counts in the picker and the report head", {
  local_home()
  create_study("RV", planner = .rv_ui_study())
  shiny::testServer(server_for("RV"), {
    review_now <- session$userData$review_now
    session$elapse(2100)
    r <- review_now()
    expect_s3_class(r, "tfl_review")
    # T2: no title, no analyses; L1: no columns; ADSL: no file (D01);
    # T1: SEX grouped by TRT01A and analysing it (A06)
    expect_true(all(c("R01", "R06", "D01", "A06") %in% r$rule))
    # the report head: T1's counts, each a way to the tab
    session$setInputs(nav = "make", target = "T1")
    h <- output$report_head$html
    expect_match(h, "review_goto", fixed = TRUE)
    expect_match(h, "to check", fixed = TRUE)
    # the Review tab: the levels with their counts, the table, the study's
    # rows chosen in its picker
    session$setInputs(nav = "review")
    tb <- output$`review-table`
    expect_match(tb, "SEX", fixed = TRUE)
    session$setInputs(`review-levels` = "error")
    expect_false(grepl("A06", jsonlite::toJSON(review_now()[0, ]), fixed = TRUE))
    # the head's link: the tab, that report and that level
    session$setInputs(review_goto = list(id = "T1", level = "check"))
    session$setInputs(`review-levels` = c("error", "check", "hand"))
    expect_match(output$`review-table`, "SEX", fixed = TRUE)
    # its picker: T1 only (L1's no columns not among them)
    expect_false(grepl("L1", output$`review-table`, fixed = TRUE))
  })
})

test_that("a click on an item goes to it: the report, the page, the tabs, the grid's row", {
  local_home()
  create_study("RV", planner = .rv_ui_study())
  shiny::testServer(server_for("RV"), {
    jump <- session$userData$review_jump
    sent <- list()
    session$sendCustomMessage <- function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
    # a table's cell
    row <- data.frame(output_id = "T1", sheet = "tables", row = "", field = "cols",
                      area = "table", stringsAsFactors = FALSE)
    tg <- .review_target(row, session$userData$rv$p)
    expect_identical(tg$go, "tables")
    expect_identical(tg$nav$table_sheet, "tables")
    expect_identical(tg$grid, "hot_tables")
    # an analysis: its outline row picked
    tg <- .review_target(data.frame(output_id = "T1", sheet = "analyses", row = "SEX",
                                    field = "by", area = "ard", stringsAsFactors = FALSE),
                         session$userData$rv$p)
    expect_identical(tg$go, "ard")
    expect_identical(tg$inputs, list(ard_ol_pick = "SEX"))
    # a code list value: the analysis data's part 4, the column's grid
    p <- set_analysis_data(session$userData$rv$p, "T1", "adsl_saf", from = "ADSL",
                           population_id = "SAF")
    tg <- .review_target(data.frame(output_id = "T1", sheet = "codelists", row = "SEX / U",
                                    field = "value", area = "codelists", stringsAsFactors = FALSE), p)
    expect_identical(tg$inputs, list(ard_adata_pick = "adsl_saf", adata_cl_pick = "SEX"))
    expect_identical(tg$grid, "adata_cl_hot")
    expect_identical(tg$key, "U")
    # a dataset with no file: the Data tab, the catalog's row
    tg <- .review_target(data.frame(output_id = NA_character_, sheet = "datasets", row = "ADSL",
                                    field = "path", area = "data", stringsAsFactors = FALSE), p)
    expect_identical(tg$nav, list(nav = "data", data_nav = "datasets"))
    expect_identical(tg$grid, "hot_ard_datasets")
    expect_identical(tg$keycols, "dataset")
    # a figure's piece: the designer's section and place
    tg <- .review_target(data.frame(output_id = "F1", sheet = "design", row = "data[2] derive",
                                    field = "expr", area = "figure", stringsAsFactors = FALSE), p)
    expect_identical(tg$inputs$pd_act[c("op", "sec", "i")], list(op = "sel", sec = "data", i = 2L))
    # the jump itself: the report chosen, and the focus sent once drawn
    jump(row)
    session$flushReact()
    # a click on the tab's item goes there (every click)
    session$setInputs(nav = "review")
    session$setInputs(`review-table_cell_clicked` = list(row = 1L, col = 3L, value = "x"))
    session$setInputs(`review-table_cell_clicked` = list(row = 1L, col = 3L, value = "x"))
    expect_identical(session$userData$rv$p$outputs$output_id[1], "T1")
  })
})

test_that("the deep check is the Review tab's (the Runs tab has no check card)", {
  ui <- htmltools::renderTags(app_ui("en"))$html
  expect_false(grepl("check_result", ui, fixed = TRUE))
  expect_match(ui, "review-deep", fixed = TRUE)
  local_home()
  create_study("RV", planner = .rv_ui_study())
  shiny::testServer(server_for("RV"), {
    review_now <- session$userData$review_now
    session$elapse(2100)
    session$setInputs(`review-deep` = 1)
    session$elapse(2100)
    expect_s3_class(review_now(), "tfl_review")
  })
})

test_that("rules switched off in the company standards are left out, and said", {
  local_home()
  std <- company_standards()
  std$settings$value[std$settings$key == "review_off"] <- "A06 | D01"
  local_mocked_bindings(company_standards = function(...) std)
  r <- review_problems(.rv_ui_study(), lang = "en")
  expect_false(any(r$rule %in% c("A06", "D01")))
  expect_identical(attr(r, "off"), c("A06", "D01"))
})
