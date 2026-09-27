sample_planner <- function() {
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
}

test_that("a table program reads the specs, saves its ARD and parses", {
  p <- sample_planner()
  code <- program_code(p, "PK")
  expect_true(any(grepl('output_id <- "PK"', code, fixed = TRUE)))
  expect_true(any(grepl('"spec/report_spec.xlsx", "spec/table_spec.xlsx"',
                        code, fixed = TRUE)))
  expect_true(any(grepl('file.exists("study.yml")', code, fixed = TRUE)))
  expect_true(any(grepl("saveRDS(data", code, fixed = TRUE)))
  # no data code: the company template takes its rows of the study ARD
  expect_true(any(grepl('ard$output_id == "PK"', code, fixed = TRUE)))
  expect_true(any(grepl("data <- ard_normalize(ard)", code, fixed = TRUE)))
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
    expect_true("doc  <- rtf_report(spec, content)" %in% code)
    expect_false(any(grepl("rtf_plan", code)))
  }
  expect_error(add_output(p, "X", type = "chart"))
})
