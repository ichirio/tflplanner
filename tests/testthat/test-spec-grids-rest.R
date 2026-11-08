# The Spec tab's grids of a table whose plan_columns(), plan_col_header() or
# plan_labels() has an argument no sheet has a column for (tflspec#689).
#
# tflspec used to send such an item whole to the table's content_more, so the
# arguments that do have columns left their cells and the grids (columns,
# style, col_header, variables, tables) came up empty.  Now the cells hold
# what the columns say and content_more only the rest.  The study is the
# sample with its reports' items changed in the YAML, opened through the
# server; the grids are read from the rendered outputs of the Spec tab.

# a grid output (the rhandsontable JSON) as a data frame of text
grid_df <- function(json) {
  j <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  hdr <- unlist(j$x$colHeaders)
  rows <- lapply(j$x$data, function(r) vapply(r, function(x) if (is.null(x)) "" else as.character(x), ""))
  d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(d) <- hdr
  d
}

# a cell of a grid: the row whose `by` column is `key`, in column `col`
grid_cell <- function(d, by, key, col) d[[col]][d[[by]] %in% key]

rest_edit_output <- function(study_path, id, content) {
  f <- file.path(study_path, "spec", "outputs", paste0(id, ".yml"))
  o <- tflspec::tfl_read_output(f)
  o$content <- content
  tflspec::tfl_write_output(o, f)
}

rest_head <- tflspec::tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable")))

test_that("the Spec tab's grids hold what the columns say; the rest of an item is not in them", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  it <- tflspec::tfl_item
  # plan_columns with a cell_format, plan_labels with a heading named twice
  # and a data variable's value labels
  rest_edit_output(s$path, "T-14-1-1", list(
    rest_head,
    it("plan_columns", list(row_title = "group", decimal = ".values",
                            widths = list(group = 2, label = 3, .values = 2),
                            auto_width = FALSE, sep = "____",
                            cell_format = tflspec::tfl_r("function(x) paste0(x, '!')"))),
    it("plan_labels", list(variable = list(AGE = "Age (years)", SEX = "Sex"), AGE = "Age at baseline",
                           TRT01A = list(`Drug X` = "Active", Control = "Placebo"))),
    it("plan_col_header", list(header = list(list(line = 1L, cols = "group", text = "Parameter"),
                                             list(line = 1L, cols = ".values", text = "{col}")),
                               header_sep = " / "))))
  # plan_columns with absolute widths, plan_col_header with lines and a span
  rest_edit_output(s$path, "T-14-1-2", list(
    rest_head,
    it("plan_columns", list(row_title = "group", decimal = ".values", auto_width = FALSE, sep = "____",
                            column_widths_twips = list(1000L, 2000L, 1500L, 1500L))),
    it("plan_col_header", list(lines = list(list(group = "", label = "", .values = "Treatment"),
                                            list(group = "", label = "Parameter", .values = "{col}")),
                               span = list(list(.values = "one"), list(.values = "each")),
                               header_sep = " / "))))
  # plan_columns with widths by position, plan_col_header with a span only
  rest_edit_output(s$path, "T-14-2-1", list(
    rest_head,
    it("plan_columns", list(widths = list(2, 3, 2, 2), row_title = "group", decimal = ".values")),
    it("plan_col_header", list(span = "TRT01A", col_header_align = "left"))))
  shiny::testServer(server_for("SAMPLE-01"), {
    rv <- session$userData$rv
    open <- function(id) session$setInputs(target = id, nav = "make", step = "content",
                                           content_nav = "content", table_nav = "spec")
    open("T-14-1-1")
    cl <- grid_df(output$hot_columns)
    expect_identical(grid_cell(cl, "column", c("group", "label", ".values"), "rel_width"), c("2", "3", "2"))
    expect_identical(grid_cell(cl, "column", "group", "row_title"), "TRUE")
    expect_identical(grid_cell(cl, "column", ".values", "decimal_split"), "TRUE")
    expect_identical(grid_df(output$hot_style)$auto_width[[1L]], "FALSE")
    expect_identical(grid_df(output$hot_tables)$sep[[1L]], "____")
    ch <- grid_df(output$hot_col_header)
    expect_identical(grid_cell(ch, "cols", c("group", ".values"), "text"), c("Parameter", "{col}"))
    vr <- grid_df(output$hot_variables)
    expect_identical(grid_cell(vr, "variable", c("AGE", "SEX"), "label"), c("Age (years)", "Sex"))
    # the rest is in the tables row's content_more, and only the rest
    cm <- sheet_rows(rv$p, "tables", "T-14-1-1")$content_more
    expect_match(cm, "cell_format", fixed = TRUE)
    expect_match(cm, "header_sep", fixed = TRUE)
    expect_false(grepl("rel_width|widths:|row_title|decimal:|sep: ____", cm))
    open("T-14-1-2")
    cl <- grid_df(output$hot_columns)
    expect_identical(grid_cell(cl, "column", "group", "row_title"), "TRUE")
    expect_identical(grid_cell(cl, "column", ".values", "decimal_split"), "TRUE")
    expect_identical(grid_df(output$hot_style)$auto_width[[1L]], "FALSE")
    expect_identical(grid_df(output$hot_tables)$sep[[1L]], "____")
    ch <- grid_df(output$hot_col_header)
    expect_identical(ch$line, c("1", "1", "1", "2", "2", "2"))
    expect_identical(ch$span, c("each", "each", "", "each", "each", "each"))
    expect_identical(ch$text, c("", "", "Treatment", "", "Parameter", "{col}"))
    open("T-14-2-1")
    cl <- grid_df(output$hot_columns)
    expect_identical(grid_cell(cl, "column", "group", "row_title"), "TRUE")
    expect_identical(grid_cell(cl, "column", ".values", "decimal_split"), "TRUE")
    expect_identical(grid_df(output$hot_style)$col_header_align[[1L]], "left")
  })
})
