sample_paths <- function() {
  d <- system.file("extdata", "ard-spec", package = "tflspec")
  file.path(d, c("report.xlsx", "study.xlsx"))
}

test_that("the sample reads, writes and reads back unchanged", {
  p <- read_planner(sample_paths())
  expect_equal(p$outputs$output_id, c("DM", "AE", "ORR", "LB", "PK"))
  p$outputs$data_code[1] <- "data <- normalize_ard(ard)\n# two lines"
  p$outputs$description[2] <- "有害事象"
  p$setup <- "library(cards)"

  dir <- withr_tempdir()
  paths <- write_planner(p, dir)
  expect_true(all(file.exists(paths)))
  q <- read_planner(paths)
  expect_identical(q$sheets, p$sheets)
  expect_identical(q$study, p$study)
  expect_identical(q$outputs, p$outputs)
  expect_identical(q$setup, p$setup)
})

test_that("a layout with the former column names is refused, not emptied", {
  old <- data.frame(output_id = "T1", stub_into = "row_label",
                    group_show = "FALSE", stringsAsFactors = FALSE)
  expect_error(.normalize_sheet(old, "layout"),
               "stub_into \\(now stub_name\\), group_show \\(now group_keep\\)")
  new <- data.frame(output_id = "T1", stub_name = "row_label",
                    stringsAsFactors = FALSE)
  expect_identical(.normalize_sheet(new, "layout")$stub_name, "row_label")
})

test_that("code saved with tflspec's former engine names is read with rtfreporter's", {
  expect_equal(
    .renamed_calls(c("data <- tfl_ard_normalize(ard)",
                     "d <- tflspec::tfl_ard_normalize(a, hierarchy = h)",
                     "my_tfl_ard_normalize(x)", NA)),
    c("data <- normalize_ard(ard)",
      "d <- rtfreporter::normalize_ard(a, hierarchy = h)",
      "my_tfl_ard_normalize(x)", NA))
  p <- read_planner(sample_paths())
  p$outputs$process_code[1] <- "data <- tfl_ard_normalize(ard)"
  p$setup <- "x <- tflspec::tfl_ard_normalize(y)"
  q <- read_planner(write_planner(p, withr_tempdir()))
  expect_equal(q$outputs$process_code[1], "data <- normalize_ard(ard)")
  expect_equal(q$setup, "x <- rtfreporter::normalize_ard(y)")
  st <- .planner_from_state(list(planner = list(
    outputs = list(output_id = "T1", description = NA,
                   data_code = "data <- tfl_ard_normalize(ard)",
                   process_code = NA))))
  expect_equal(st$outputs$data_code, "data <- normalize_ard(ard)")
})

test_that("each workbook carries its half and tflspec reads each alone", {
  p <- read_planner(sample_paths())
  paths <- write_planner(p, withr_tempdir())
  t <- tflspec::tfl_read_table_spec(paths[["table"]])
  r <- tflspec::tfl_read_report_spec(paths[["report"]])
  expect_gt(nrow(t$tables), 0)
  expect_equal(nrow(t$report), 0)
  expect_equal(nrow(r$tables), 0)
  expect_gt(nrow(r$header), 0)
  expect_equal(t$study$value[t$study$key == "rounding"], "sas")
  expect_equal(r$study$value[r$study$key == "output_path"], "output")
  expect_true("_tflplanner" %in% readxl::excel_sheets(paths[["report"]]))
  expect_false("_README" %in% readxl::excel_sheets(paths[["table"]]))
  expect_false("header" %in% readxl::excel_sheets(paths[["table"]]))
  expect_false("tables" %in% readxl::excel_sheets(paths[["report"]]))
})

test_that("copy, rename and remove act on every sheet", {
  p <- read_planner(sample_paths())
  n <- vapply(p$sheets, function(d) sum(d$output_id %in% "DM"), 1L)
  q <- copy_output(p, "DM", "DM_ITT")
  expect_equal(vapply(q$sheets, function(d) sum(d$output_id %in% "DM_ITT"),
                      1L), n)
  expect_error(copy_output(q, "DM", "DM_ITT"), "already exists")
  q <- rename_output(q, "DM_ITT", "DM2")
  expect_false("DM_ITT" %in% output_ids(q))
  q <- remove_output(q, "DM2")
  expect_identical(q$sheets, p$sheets)
  expect_error(add_output(p, "a/b"), "file name")
})

test_that("a filtered edit puts the rows back where they were", {
  p <- read_planner(sample_paths())
  before <- p$sheets$variables
  ae <- sheet_rows(p, "variables", "AE")
  ae$output_id <- NULL
  ae$label[1] <- "Arm"
  ae <- rbind(ae, data.frame(variable = "NEW", label = NA, order = NA,
                             levels = NA, note = NA))
  q <- set_sheet_rows(p, "variables", "AE", ae)
  v <- q$sheets$variables
  expect_equal(nrow(v), nrow(before) + 1)
  expect_equal(v$variable[v$output_id == "AE"], c("TR01AG1", "SEROSTAT", "NEW"))
  expect_equal(v[v$output_id != "AE", ], before[before$output_id != "AE", ],
               ignore_attr = TRUE)
  expect_equal(which(v$output_id == "AE"), 6:8)
  # a default-row filter writes blank ids; blank rows are dropped
  d <- sheet_rows(p, "cells", NA)
  d <- rbind(d, NA)
  q <- set_sheet_rows(p, "cells", NA, d)
  expect_identical(q$sheets$cells, p$sheets$cells)
})

test_that("a grid's empty rows never become rows of an output_id", {
  p <- read_planner(sample_paths())
  v <- sheet_rows(p, "variables", "AE")
  v$output_id <- NULL
  spare <- v[1, ]
  spare[] <- NA
  q <- set_sheet_rows(p, "variables", "AE", rbind(v, spare, ""))
  expect_identical(q$sheets, p$sheets)
  a <- add_output(new_planner(), "T1")
  a$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = "A", method = "categorical"), "analyses")
  r <- ard_rows(a, "analyses", "T1")
  r$output_id <- NULL
  r[2, ] <- NA
  expect_identical(set_ard_rows(a, "analyses", "T1", r)$ard, a$ard)
})

test_that("check_planner() reports what tflspec refuses", {
  p <- read_planner(sample_paths())
  expect_true(all(check_planner(p)$ok))
  p$sheets$layout$pages_max_rows[1] <- "twenty"
  r <- check_planner(p)
  expect_false(all(r$ok))
  expect_match(paste(r$message, collapse = " "), "pages_max_rows")
})
