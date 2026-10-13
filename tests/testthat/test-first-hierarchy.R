# A hierarchical table started from the data (first_hierarchy_table(), the
# "Start from the data" form's Hierarchy): the rows a table made by hand
# has -- the sample's T-14-3-1, but for what is the study's own (its page
# pattern and rows a page, titles, labels)

sample_rows <- function(x, part, sheet, id = "T-14-3-1", cols) {
  d <- if (part == "ard") x$ard[[sheet]] else x$sheets[[sheet]]
  d <- as.data.frame(d[!is.na(d$output_id) & d$output_id == id, , drop = FALSE])
  d <- d[, cols, drop = FALSE]
  rownames(d) <- NULL
  d
}

starter_from_sample <- function(...) {
  s <- suppressMessages(create_sample_study(run = FALSE))
  adae <- readRDS(file.path(s$path, "data/adam/adae.rds"))
  adsl <- readRDS(file.path(s$path, "data/adam/adsl.rds"))
  p <- first_hierarchy_table(new_planner(), "T-14-3-1", "data/adam/adae.rds", adae,
                             "data/adam/adsl.rds", adsl, population = "SAFFL",
                             group = "TRT01A", where = "TRTEMFL == \"Y\"", ...)
  list(sample = s$planner, p = p, adae = adae, adsl = adsl)
}

test_that("the starter writes the rows of the sample's T-14-3-1", {
  skip_on_cran()
  local_home()
  r <- starter_from_sample(description = "TEAE by SOC / PT")
  same <- function(part, sheet, cols) {
    expect_identical(sample_rows(r$p, part, sheet, cols = cols),
                     sample_rows(r$sample, part, sheet, cols = cols), info = sheet)
  }
  same("ard", "analysis_data", c("data_id", "from", "population_id", "subjects", "where"))
  same("ard", "analyses", c("analysis_id", "method", "data", "by", "variables",
                            "denominator", "args", "overall"))
  same("sheet", "tables", c("cols", "rows", "label", "sort", "total"))
  same("sheet", "col_header", c("line", "cols", "span", "text"))
  same("sheet", "layout", c("pages_split", "pages_max_rows"))
  # the ARD read as a hierarchy, its "Any TEAE" row: the sample's rework
  pc <- function(x) x$outputs$process_code[match("T-14-3-1", x$outputs$output_id)]
  expect_identical(pc(r$p), pc(r$sample))
  # the analysis set, as the sample's
  po <- r$p$ard$populations
  expect_identical(po$population_id, "SAF")
  expect_identical(po$where, "SAFFL == \"Y\"")
  expect_identical(po$dataset, "ADSL")
  expect_setequal(r$p$ard$datasets$dataset, c("ADSL", "ADAE"))
  # the columns: the analysis set's arms (no screen failures), in order
  vr <- sample_rows(r$p, "sheet", "variables", cols = c("variable", "levels"))
  expect_identical(vr$levels, "Placebo | Xanomeline Low Dose | Xanomeline High Dose")
  expect_identical(report_info(r$p, "T-14-3-1")$type, "table")
})

test_that("its own hierarchy (medications), all records, a Total column", {
  skip_on_cran()
  local_home()
  r <- starter_from_sample()
  cm <- data.frame(USUBJID = r$adae$USUBJID, TRT01A = r$adae$TRT01A,
                   CMCLAS = r$adae$AEBODSYS, CMDECOD = r$adae$AEDECOD, CMSEQ = 1)
  p <- first_hierarchy_table(new_planner(), "T-CM", "data/adam/adcm.rds", cm,
                             "data/adam/adsl.rds", r$adsl, population = "SAFFL",
                             group = "TRT01A", hierarchy = c("CMCLAS", "CMDECOD"),
                             total = "Total")
  an <- ard_rows(p, "analyses", "T-CM")
  expect_identical(an$analysis_id, "CM")
  expect_identical(an$variables, "CMCLAS | CMDECOD")
  expect_identical(an$overall, "TRUE")
  expect_match(p$outputs$process_code, "hierarchy = c(\"CMCLAS\", \"CMDECOD\")", fixed = TRUE)
  expect_match(p$outputs$process_code, "overall = \"Any CM\"", fixed = TRUE)
  ad <- ard_rows(p, "analysis_data", "T-CM")
  expect_identical(ad$data_id, c("adsl_saf", "adcm_saf"))
  expect_true(is.na(ad$where[2L]))
  tb <- sheet_rows(p, "tables", "T-CM")
  expect_identical(tb$total, "Total")
  expect_identical(tb$label, "label = CMDECOD")
  # three terms: two groups, the innermost the rows
  cm$CMCAT <- "ALL"
  p3 <- first_hierarchy_table(new_planner(), "T-3", "data/adam/adcm.rds", cm,
                              "data/adam/adsl.rds", r$adsl, population = "SAFFL",
                              group = "TRT01A", hierarchy = c("CMCAT", "CMCLAS", "CMDECOD"))
  tb3 <- sheet_rows(p3, "tables", "T-3")
  expect_identical(tb3$rows, "group1 = CMCAT | group2 = CMCLAS")
  expect_identical(tb3$sort, ".overall | group1 | .depth | group2 | .depth | -n | label")
})

test_that("what the starter cannot do, it says", {
  skip_on_cran()
  local_home()
  r <- starter_from_sample()
  go <- function(...) first_hierarchy_table(new_planner(), "T-X", "data/adam/adae.rds",
                                            r$adae, "data/adam/adsl.rds", r$adsl, ...)
  expect_error(go(population = "SAFFL", group = "TRT01A", hierarchy = "AEDECOD"),
               "two terms or more")
  expect_error(go(population = "SAFFL", group = "TRT01A", hierarchy = c("AEBODSYS", "NOPE")),
               "The data have no NOPE")
  expect_error(go(population = "NOFL", group = "TRT01A"),
               "subject-level data have no NOFL")
  expect_error(go(population = "SAFFL", group = "TRT01A", where = "TRTEMFL =="),
               "not R")
  expect_error(first_hierarchy_table(r$sample, "T-14-3-1", "data/adam/adae.rds", r$adae,
                                     "data/adam/adsl.rds", r$adsl, population = "SAFFL",
                                     group = "TRT01A"), "has analyses already")
})

test_that("the form: Hierarchy is chosen (never switched by the data) and starts the table", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  shiny::testServer(server_for(s$meta$study_id), {
    rv <- session$userData$rv
    session$setInputs(add = 1)
    # a summary by default, its data ADSL
    session$setInputs(modal_type = "table", modal_first = TRUE)
    expect_match(output$modal_first_ui$html, "Hierarchy (SOC / PT)", fixed = TRUE)
    expect_match(output$mf_data_ui$html, "selected>data/adam/adsl", fixed = TRUE)
    session$setInputs(mf_kind = "hierarchy")
    expect_match(output$mf_data_ui$html, "selected>data/adam/adae", fixed = TRUE)
    session$setInputs(mf_data = "data/adam/adae.rds")
    h <- output$mf_cols$html
    expect_match(h, "mf_hier", fixed = TRUE)
    # the terms outermost first (the choices' order is the selected order)
    expect_lt(regexpr("option value=\"AEBODSYS\" selected", h, fixed = TRUE),
              regexpr("option value=\"AEDECOD\" selected", h, fixed = TRUE))
    expect_match(h, "TRTEMFL == &quot;Y&quot;", fixed = TRUE)
    session$setInputs(modal_id = "T-AE", modal_desc = "TEAEs", mf_pop = "SAFFL",
                      mf_group = "TRT01A", mf_hier = c("AEBODSYS", "AEDECOD"),
                      mf_where = "TRTEMFL == \"Y\"", mf_total = FALSE, add_ok = 1)
    an <- ard_rows(rv$p, "analyses", "T-AE")
    expect_identical(an$method, "hierarchical")
    expect_identical(an$variables, "AEBODSYS | AEDECOD")
    expect_identical(sheet_rows(rv$p, "layout", "T-AE")$pages_split, "group_force")
  })
})
