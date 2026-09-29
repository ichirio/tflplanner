sample_planner <- function() {
  d <- system.file("extdata", "ard-spec", package = "tflspec")
  read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
}

test_that("a table program writes out its plan and report, saves its ARD and parses", {
  p <- sample_planner()
  code <- program_code(p, "PK")
  expect_true(any(grepl('output_id <- "PK"', code, fixed = TRUE)))
  # the definition written out, not read at run time
  expect_false(any(grepl("tfl_read_report_spec", code, fixed = TRUE)))
  expect_false(any(grepl("spec = spec", code, fixed = TRUE)))
  expect_true(any(startsWith(code, "plan <- tfl_plan(")))
  expect_true(any(grepl("tfl_plan_", code, fixed = TRUE)))
  expect_true(any(startsWith(code, "doc <- rtf_document(")))
  expect_true(any(startsWith(code, "doc <- rtf_tables(doc, plan")))
  expect_true('generate_rtfreport(doc, "output/PK.rtf", overwrite = TRUE)' %in% code)
  expect_true(any(grepl('file.exists("study.yml")', code, fixed = TRUE)))
  expect_true(any(grepl("saveRDS(data", code, fixed = TRUE)))
  # no data code: the company template takes its rows of the study ARD
  expect_true(any(grepl('ard$output_id == "PK"', code, fixed = TRUE)))
  expect_true(any(grepl("data <- tfl_ard_normalize(ard)", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  expect_equal(report_info(p, "PK")$file, "output/PK.rtf")
  expect_silent(parse(text = autoexec_code(p)))

  p$outputs$data_code[p$outputs$output_id == "PK"] <- "data <- my_pk()"
  code <- program_code(p, "PK")
  expect_true("data <- my_pk()" %in% code)
  expect_false(any(grepl("TODO", code)))

  p$sheets$report <- rbind(p$sheets$report, NA)
  p$sheets$report[nrow(p$sheets$report), c("output_id", "program")] <-
    c("AE", "t_ae_{output_id}")
  expect_equal(report_info(p, "AE")$program, "t_ae_AE.R")
})

test_that("listings and figures make `content` and skip the table plan", {
  p <- add_output(new_planner(), "L1", type = "listing")
  p <- add_output(p, "F1", type = "figure")
  expect_equal(report_info(p, "L1")$type, "listing")
  expect_equal(p$sheets$report$type, c("listing", "figure"))
  for (id in c("L1", "F1")) {
    code <- program_code(p, id)
    expect_silent(parse(text = code))
    expect_true(any(code %in% c("doc <- rtf_tables(doc, content)",
                                "doc <- rtf_figures(doc, content)")))
    expect_false(any(grepl("tfl_plan(", code, fixed = TRUE)))
  }
  expect_error(add_output(p, "X", type = "chart"))
})

test_that("a definition that does not hold yet still gives a program, which says why", {
  p <- sample_planner()
  # a statistic's display format (no template) in a table that is not
  # stats = rows: tflspec refuses it
  p$sheets$cells <- rbind(p$sheets$cells, NA)
  p$sheets$cells[nrow(p$sheets$cells), c("output_id", "row", "digits")] <-
    c("DM", "n", "0")
  code <- program_code(p, "DM")
  expect_silent(parse(text = code))
  hit <- grep("tflplanner: the definition of DM does not hold", code, fixed = TRUE)
  expect_length(hit, 1L)
})
