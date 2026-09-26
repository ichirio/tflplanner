test_that("a program names its report, reads the specs and parses", {
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  p <- read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
  code <- program_code(p, "PK", spec_dir = "C:\\study\\spec")
  expect_true(any(grepl('output_id <- "PK"', code, fixed = TRUE)))
  expect_true(any(grepl('spec_dir  <- "C:/study/spec"', code, fixed = TRUE)))
  expect_true(any(grepl("still to be written", code)))
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

test_that("export keeps an edited program unless told to overwrite", {
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  p <- read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
  dir <- withr_tempdir()
  r <- export_planner(p, dir)
  expect_true(all(r$status == "written"))
  expect_equal(nrow(r), 2 + 5 + 1)
  writeLines("# mine", file.path(dir, "DM.R"))
  r <- export_planner(p, dir)
  expect_equal(r$status[basename(r$file) == "DM.R"], "kept")
  expect_equal(readLines(file.path(dir, "DM.R")), "# mine")
  export_planner(p, dir, overwrite_programs = TRUE)
  expect_gt(length(readLines(file.path(dir, "DM.R"))), 1)
})

test_that("the generated programs run end to end through autoexec", {
  skip_if_not_installed("cards")
  skip_on_cran()
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  p <- read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
  for (id in c("AE", "ORR", "LB", "PK")) p <- remove_output(p, id)
  dir <- withr_tempdir()
  p$study[["output_path"]] <- file.path(dir, "rtf")
  p$setup <- "library(cards)"
  p$outputs$data_code[1] <- paste(
    "adsl <- transform(cards::ADSL, TRT01P = \"XXXXX\", HTBL = HEIGHTBL)",
    "ard <- ard_stack(",
    "  adsl, .by = TRT01P,",
    "  ard_continuous(variables = c(AGE, HTBL),",
    "                 statistic = ~ continuous_summary_fns(",
    "                   c(\"N\", \"mean\", \"sd\", \"median\", \"min\", \"max\"))),",
    "  ard_categorical(variables = c(AGEGR1, SEX, ETHNIC),",
    "                  statistic = ~ c(\"n\", \"p\")),",
    "  .total_n = TRUE)",
    "data <- ard_normalize(ard)", sep = "\n")
  p <- add_output(p, "TODO1")
  dir.create(file.path(dir, "rtf"))
  export_planner(p, dir)

  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(
    rscript, shQuote(file.path(dir, "autoexec_report.R")),
    stdout = TRUE, stderr = TRUE))
  expect_equal(attr(out, "status"), 1L)          # TODO1 is still a TODO
  res <- utils::read.csv(file.path(dir, "logs", "autoexec_report.csv"))
  expect_equal(res$status, c("OK", "ERROR"))
  expect_match(res$note[2], "still to be written")
  rtf <- file.path(dir, "rtf", "DM.rtf")
  expect_true(file.exists(rtf))
  expect_true(any(grepl("Age (years)", readLines(rtf, warn = FALSE),
                        fixed = TRUE)))
})
