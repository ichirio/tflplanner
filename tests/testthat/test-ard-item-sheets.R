# A template item's condition rows and calls: the ARD sheets conditions and
# ards (tflspec#827).  Their columns are not fixed (a column a param, a
# call's keys); their rows are a report's.

skip_if_no_item_sheets <- function() {
  skip_if(!all(c("conditions", "ards") %in% names(.ard_sheets())),
          "this tflspec has no conditions / ards sheets (tflspec#827)")
}

test_that("conditions keep their param columns, a report's rows set and read by output_id", {
  skip_if_no_item_sheets()
  p <- new_planner()
  orr <- data.frame(analysis_id = "ORR", row = c("1", "2"), subgroup = c("ALL", "SEX"),
                    subgroup_lvl = c("ALL", "F"), where = c("TRUE", "SEX == \"F\""))
  shift <- data.frame(analysis_id = "SHIFT", row = "1", tox_dir = "High",
                      param = "Alanine Aminotransferase (U/L)", row_analysis_id = "ALT_HIGH")
  p <- set_ard_rows(p, "conditions", "T-3", orr)
  p <- set_ard_rows(p, "conditions", "T-4", shift)
  all <- ard_rows(p, "conditions")
  expect_identical(nrow(all), 3L)
  expect_true(all(c("output_id", "analysis_id", "row", "subgroup", "subgroup_lvl", "where",
                    "tox_dir", "param", "row_analysis_id") %in% names(all)))
  # (the fixed columns first)
  expect_identical(names(all)[1:3], c("output_id", "analysis_id", "row"))
  t3 <- ard_rows(p, "conditions", "T-3")
  expect_identical(t3$where, c("TRUE", "SEX == \"F\""))
  # (a param the item has not: blank)
  expect_true(all(is.na(t3$tox_dir)))
  # T-3 set again: its rows replaced, T-4's kept
  p <- set_ard_rows(p, "conditions", "T-3", orr[1L, ])
  expect_identical(nrow(ard_rows(p, "conditions", "T-3")), 1L)
  expect_identical(ard_rows(p, "conditions", "T-4")$row_analysis_id, "ALT_HIGH")
})

test_that("ards keep a call's keys; a fixed sheet still drops a column it has not", {
  skip_if_no_item_sheets()
  p <- new_planner()
  calls <- data.frame(analysis_id = "SHIFT", row = c("1", "2"),
                      by = c("TRT01A | PARAM | tox_dir | base", "TRT01A | PARAM"),
                      variables = c("worst", "worst"))
  p <- set_ard_rows(p, "ards", "T-4", calls)
  got <- ard_rows(p, "ards", "T-4")
  expect_identical(got$by, calls$by)
  expect_identical(got$output_id, c("T-4", "T-4"))
  # (analyses: its columns only, as before)
  a <- .normalize_ard_sheet(data.frame(output_id = "T-4", analysis_id = "SHIFT", no_such = "x"),
                            "analyses")
  expect_false("no_such" %in% names(a))
})

test_that("the open columns survive the study's ARD copy (JSON) read back", {
  skip_if_no_item_sheets()
  p <- new_planner()
  p <- set_ard_rows(p, "conditions", "T-3",
                    data.frame(analysis_id = "ORR", row = "1", subgroup = "ALL", where = "TRUE"))
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  writeLines(jsonlite::toJSON(p$ard, dataframe = "columns", na = "null", auto_unbox = FALSE), f)
  back <- .read_ard_json(f)
  expect_identical(back$conditions$subgroup, "ALL")
  expect_identical(back$conditions$where, "TRUE")
})

test_that("the sheets' own help is tflspec's, shown in Japanese too", {
  skip_if_no_item_sheets()
  for (s in c("conditions", "ards")) {
    h <- .ard_sheet_help(s)
    expect_true(nzchar(h))
    expect_false(identical(tr(h, lang = "ja"), h))
  }
  expect_identical(.ard_sheet_help("no_such_sheet"), "")
})
