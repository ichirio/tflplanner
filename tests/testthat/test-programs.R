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
  expect_true(any(startsWith(code, "plan <- table_plan(")))
  expect_true(any(startsWith(trimws(code), "plan_digits(")))
  expect_true(any(startsWith(code, "doc <- rtf_document(")))
  expect_true(any(startsWith(code, "doc <- rtf_tables(doc, plan")))
  expect_true('generate_rtfreport(doc, "output/PK.rtf", overwrite = TRUE)' %in% code)
  expect_true(any(grepl('file.exists("study.yml")', code, fixed = TRUE)))
  expect_true(any(grepl("saveRDS(data", code, fixed = TRUE)))
  # no data code: the company template takes its rows of the study ARD
  expect_true(any(grepl('subset(ard, output_id == "PK",', code, fixed = TRUE)))
  expect_true(any(grepl("data <- normalize_ard(ard)", code, fixed = TRUE)))
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
    expect_false(any(grepl("table_plan(", code, fixed = TRUE)))
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

test_that("the report list names the datasets a report reads, and its title", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  p <- suppressMessages(create_sample_study(run = FALSE))$planner
  # a table: its analyses' data and their population's
  expect_identical(.report_datasets(p, "T-14-1-1"), "ADSL")
  expect_identical(.report_datasets(p, "T-14-3-1"), c("ADAE", "ADSL"))
  # a listing: its dataset
  expect_identical(.report_datasets(p, "L-16-2-7"), "ADAE")
  # a figure: the datasets its row names, or its design reads
  expect_identical(.report_datasets(p, "F-14-2-2"), "ADTTE")
  d <- tflspec::tfl_fig_template("mean_ci", data = "ADVS", param = "SYSBP",
                                  value = "CHG")
  # (the sample's F-14-2-1 is user code: a figure again, to be designed)
  p2 <- set_fig_design(.set_report_type(p, "F-14-2-1", "figure"), "F-14-2-1", d)
  expect_identical(.report_datasets(p2, "F-14-2-1"), c("ADVS", "ADSL"))
  # the title: the titles sheet's own lines, not the running study lines
  expect_identical(.report_title(p, "T-14-1-1"), "")
  p$sheets$titles <- data.frame(
    output_id = c(NA, "T-14-1-1", "T-14-1-1"), line = c("1", "1", "2"),
    left = NA, center = c("Study {PAGE}", "Table 14.1.1", "Demographics"),
    right = NA, note = NA, stringsAsFactors = FALSE)
  expect_identical(.report_title(p, "T-14-1-1"), "Table 14.1.1 / Demographics")
})

test_that("the report list's cells are escaped, cut short, with the whole on hover", {
  expect_identical(.cell_tip("a<b"), "a&lt;b")
  expect_identical(.cell_tip("a<b", 'x"y'), '<span title="x&quot;y">a&lt;b</span>')
  long <- strrep("a", 80)
  expect_identical(nchar(.ellipsis(long, 70L)), 70L)
  expect_identical(.ellipsis("short", 70L), "short")
})

test_that("the report types stay English in the Japanese app", {
  expect_identical(tr(c("Table", "Listing", "Figure"), "ja"),
                   c("Table", "Listing", "Figure"))
  expect_identical(tr("Report title", "ja"), "タイトル")
})
