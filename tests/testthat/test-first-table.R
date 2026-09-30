test_that("data files go into the data catalog once, with name and level", {
  p <- new_planner()
  files <- data.frame(folder = c("data/adam", "data/adam", "data/sdtm", "data/other"),
                      file = c("adsl.rds", "notes.txt", "dm.xpt", "x.rds"),
                      stringsAsFactors = FALSE)
  p2 <- catalog_add_files(p, files)
  expect_identical(attr(p2, "added"), c("ADSL", "DM"))
  ds <- ard_rows(p2, "datasets")
  expect_identical(ds$level, c("ADaM", "SDTM"))
  expect_identical(ds$path, c("data/adam/adsl.rds", "data/sdtm/dm.xpt"))
  # again: nothing more
  expect_length(attr(catalog_add_files(p2, files), "added"), 0L)
})

test_that("first_table() writes every sheet a summary table needs", {
  d <- data.frame(SAFFL = c("Y", "Y"), TRT01A = c("B", "A"), TRT01AN = c(1, 2),
                  AGE = c(50, 60),
                  SEX = c("F", "M"), TRTSDT = as.Date(c("2020-01-01", "2020-01-02")),
                  stringsAsFactors = FALSE)
  attr(d$AGE, "label") <- "Age"
  p <- first_table(new_planner(), "T-DM", "data/adam/adsl.rds", d,
                   population = "SAFFL", group = "TRT01A",
                   variables = c("AGE", "SEX"), description = "Demographics")
  expect_true("T-DM" %in% p$outputs$output_id)
  expect_identical(report_info(p, "T-DM")$type, "table")
  expect_identical(ard_rows(p, "datasets")$dataset, "ADSL")
  po <- ard_rows(p, "populations")
  expect_identical(po$population_id, "SAF")
  expect_identical(po$where, "SAFFL == \"Y\"")
  an <- ard_rows(p, "analyses", "T-DM")
  # the N per group first (column headers), then one analysis per variable
  expect_identical(an$analysis_id, c("BIGN", "AGE", "SEX"))
  expect_identical(an$method, c("categorical", "continuous", "categorical"))
  expect_identical(an$by, c(NA, "TRT01A", "TRT01A"))
  expect_identical(an$variables, c("TRT01A", "AGE", "SEX"))
  expect_identical(an$population_id, c("SAF", "SAF", "SAF"))
  tb <- sheet_rows(p, "tables", "T-DM")
  expect_identical(tb$cols, "TRT01A")
  expect_identical(tb$rows, "group = variable")
  vr <- sheet_rows(p, "variables", "T-DM")
  # the groups in the order of TRT01AN, then the rows with the data's labels
  expect_identical(vr$variable, c("TRT01A", "AGE", "SEX"))
  expect_identical(vr$levels[1], "B | A")
  expect_identical(vr$order, c(NA, "1", "2"))
  expect_identical(vr$label, c(NA, "Age", NA))
  # a second table on the same data reuses the dataset and the analysis set
  p <- first_table(p, "T-2", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "AGE")
  expect_identical(nrow(ard_rows(p, "populations")), 1L)
  expect_identical(nrow(ard_rows(p, "datasets")), 1L)
  expect_error(first_table(p, "T-2", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "AGE"),
               "has analyses already")
  expect_error(first_table(p, "T-3", "data/adam/adsl.rds", d, "SAFFL", "TRT01A", "TRTSDT"),
               "Dates")
})

test_that("Add > Table from the data: data file to a previewed table in one form", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("F1")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  shiny::testServer(server_for("F1"), {
    rv <- session$userData$rv
    # Refresh on the Data tab: the file joins the data catalog
    session$setInputs(data_refresh = 1)
    expect_true("ADSL" %in% ard_rows(rv$p, "datasets")$dataset)
    session$setInputs(add = 1, modal_type = "table", modal_first = TRUE,
                      modal_id = "T-DM", modal_desc = "Demographics")
    session$setInputs(mf_data = "data/adam/adsl.rds")
    expect_match(output$mf_cols$html, "SAFFL")
    session$setInputs(mf_pop = "SAFFL", mf_group = "TRT01A",
                      mf_vars = c("AGE", "SEX"))
    session$setInputs(add_ok = 1)
    expect_true("T-DM" %in% rv$p$outputs$output_id)
    expect_identical(nrow(ard_rows(rv$p, "analyses", "T-DM")), 3L)
    # saved, previewed and read: the builder can start
    s2 <- open_study("F1")
    st <- ard_status(s2)
    expect_identical(st$state[st$output_id == "T-DM"], "built")
    expect_false(is.null(ard_info(s2, "T-DM")))
  })
})

test_that("dates, times and their flags are not offered as rows or groups", {
  cols <- list(AGE = c(1, 2), RFSTDTC = c("2014-01-02", "2014-02-03"),
               TRTSDT = as.Date("2014-01-01"), TRTSTMF = c("H", "M"),
               SEX = c("F", "M"), TRT01A = c("A", "B"), TRTETMF = c("H", NA))
  attr(cols$AGE, "label") <- "Age"
  r <- .row_choices(cols)
  expect_identical(unname(r), c("AGE", "SEX", "TRT01A"))
  expect_identical(names(r)[1], "AGE — Age (numbers)")
  expect_identical(unname(.group_choices(cols)), "TRT01A")
})

test_that("read_data_head() keeps the columns' labels", {
  f <- tempfile(fileext = ".rds")
  d <- data.frame(AGE = 1:3)
  attr(d$AGE, "label") <- "Age"
  saveRDS(d, f)
  expect_identical(attr(read_data_head(f, 2L)$AGE, "label"), "Age")
})
