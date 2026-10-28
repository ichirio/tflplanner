# A hierarchical table started from the data (first_hierarchy_table(), the
# "Apply a template" form's hierarchical table): the rows a table made by hand
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
  # the ARD read as a hierarchy, its "Any TEAE" row: from the table's
  # overall_label, the very line the sample's rework writes by hand
  expect_true(is.na(r$p$outputs$process_code[match("T-14-3-1", r$p$outputs$output_id)]))
  same("sheet", "tables", "overall_label")
  # (in the table's pipe, tflspec #302)
  norm <- function(p, id) {
    code <- .report_code_lines(p, id, table = TRUE)
    i <- which(code == "plan <- ard |>")
    j <- which(startsWith(trimws(code), "table_plan("))
    paste(trimws(code[(i + 1L):(j[j > i][1L] - 1L)]), collapse = " ")
  }
  expect_identical(norm(r$p, "T-14-3-1"), norm(r$sample, "T-14-3-1"))
  expect_match(norm(r$p, "T-14-3-1"), "overall = \"Any TEAE\"", fixed = TRUE)
  # the analysis set, as the sample's
  po <- r$p$ard$populations
  expect_identical(po$population_id, "SAF")
  expect_identical(po$where, "SAFFL == \"Y\"")
  expect_identical(po$dataset, "ADSL")
  expect_setequal(r$p$ard$datasets$dataset, c("ADSL", "ADAE"))
  # the columns: the analysis set's arms (no screen failures), in order
  vr <- sample_rows(r$p, "sheet", "variables", cols = c("variable", "levels"))
  expect_identical(vr$levels, "Drug X | Control")
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
  expect_identical(sheet_rows(p, "tables", "T-CM")$overall_label, "Any CM")
  code <- .report_code_lines(p, "T-CM", table = TRUE)
  expect_match(norm_line <- paste(code, collapse = " "),
               "hierarchy = c(\"CMCLAS\", \"CMDECOD\")", fixed = TRUE)
  expect_match(norm_line, "overall = \"Any CM\"", fixed = TRUE)
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

test_that("by severity too: each subject at the worst, the grade in the columns under each group", {
  skip_on_cran()
  local_home()
  r <- starter_from_sample(grade = "AESEV")
  an <- ard_rows(r$p, "analyses", "T-14-3-1")
  expect_identical(an$by, "TRT01A | AESEV")
  expect_identical(an$denominator, "adsl_saf")
  tb <- sheet_rows(r$p, "tables", "T-14-3-1")
  expect_identical(tb$cols, "TRT01A | AESEV")
  # the grade's levels in order: its code list (the worst is the last)
  cl <- sheet_rows(r$p, "codelists", "T-14-3-1")
  expect_identical(cl$value[cl$variable == "AESEV"], c("MILD", "MODERATE", "SEVERE"))
  # the group (and its N) over its grades
  ch <- sheet_rows(r$p, "col_header", "T-14-3-1")
  expect_identical(ch$span, c(NA, "TRT01A", NA, "each"))
  expect_identical(ch$text[2L], "{col1}\n(N={n})")
  expect_true(.has_group_n(r$p, "T-14-3-1", "TRT01A"))
  # not with a Total column
  expect_error(first_hierarchy_table(new_planner(), "T-X", "data/adam/adae.rds", r$adae,
                                     "data/adam/adsl.rds", r$adsl, population = "SAFFL",
                                     group = "TRT01A", grade = "AESEV", total = "Total"),
               "Total column and a grade")
  expect_error(first_hierarchy_table(new_planner(), "T-X", "data/adam/adae.rds", r$adae,
                                     "data/adam/adsl.rds", r$adsl, population = "SAFFL",
                                     group = "TRT01A", grade = "NOPE"), "no NOPE")
})

test_that("a grade's levels, mildest first", {
  expect_identical(.grade_levels(c("SEVERE", "MILD", NA, "MODERATE")), c("MILD", "MODERATE", "SEVERE"))
  expect_identical(.grade_levels(c("Grade 3", "Grade 1", "Grade 10", "Grade 2")),
                   c("Grade 1", "Grade 2", "Grade 3", "Grade 10"))
  expect_identical(.grade_levels(c("3", "1", "2")), c("1", "2", "3"))
  expect_identical(.grade_levels(factor(c("b", "a"), c("b", "a"))), c("b", "a"))
  expect_identical(.grade_levels(c("y", "x")), c("x", "y"))
})
