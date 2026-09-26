test_that("a new study has its layout, study.yml and workbooks", {
  root <- withr_tempdir()
  s <- create_study(root, "ABC-101", title = "A phase 2 study", phase = "2")
  for (d in study_layout()) expect_true(dir.exists(file.path(s$path, d)))
  expect_true(file.exists(file.path(s$path, "study.yml")))
  expect_true(file.exists(file.path(s$path, "ABC-101.Rproj")))
  expect_true(file.exists(file.path(s$path, "spec", "table_spec.xlsx")))
  expect_true(file.exists(file.path(s$path, "programs",
                                    "autoexec_report.R")))
  expect_error(create_study(root, "ABC-101"), "already there")
  expect_error(create_study(root, "bad id"), "study id")

  l <- list_studies(root)
  expect_equal(l$study_id, "ABC-101")
  expect_equal(l$title, "A phase 2 study")

  o <- open_study(file.path(root, "ABC-101"))
  expect_equal(o$meta$phase, "2")
  expect_equal(unname(o$planner$study[c("output_path", "program_dir")]),
               c("output/tfl", "programs"))
})

test_that("saving keeps an edited program unless asked to regenerate", {
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  p <- read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
  s <- create_study(withr_tempdir(), "S1", planner = p)
  expect_equal(sum(s$files$status == "written"), 2 + 5 + 1)
  s0 <- save_study(s)
  expect_true(all(s0$files$status == "unchanged"))
  f <- file.path(s$path, "programs", "DM.R")
  expect_equal(study_status(s)$program_state, rep("todo", 5))

  # an untouched program follows the definition
  s$planner$outputs$data_code[1] <- "data <- ard_normalize(my_ard)"
  expect_equal(study_status(s)$program_state[1], "generated")
  expect_equal(study_status(s)$status[1], "unsaved")
  s <- save_study(s)
  expect_true("data <- ard_normalize(my_ard)" %in% readLines(f))
  expect_equal(study_status(s)$program_state[1], "current")

  # nothing changed: nothing is rewritten
  s2 <- save_study(s)
  expect_true(all(s2$files$status == "unchanged"))

  # a hand edit is kept
  writeLines(c(readLines(f), "# mine"), f)
  expect_equal(study_status(s)$program_state[1], "edited")
  s$planner$outputs$data_code[1] <- "data <- something_else()"
  s <- save_study(s)
  expect_equal(utils::tail(readLines(f), 1), "# mine")
  expect_equal(s$files$status[basename(s$files$file) == "DM.R"], "kept")
  s <- save_study(s, regenerate = "DM")
  expect_true("data <- something_else()" %in% readLines(f))
  expect_equal(study_status(s)$program_state[1], "current")
})

test_that("a study runs end to end and reports what it produced", {
  skip_if_not_installed("cards")
  skip_on_cran()
  d <- system.file("extdata", "ard-spec", package = "rtfreporter")
  p <- read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
  for (id in c("AE", "ORR", "LB", "PK")) p <- remove_output(p, id)
  p$setup <- "library(cards)"
  p$outputs$data_code[1] <- paste(
    "adsl <- readRDS(\"data/adam/adsl.rds\")",
    "adsl <- transform(adsl, TRT01P = \"XXXXX\", HTBL = HEIGHTBL)",
    "ard <- ard_stack(",
    "  adsl, .by = TRT01P,",
    "  ard_continuous(variables = c(AGE, HTBL),",
    "                 statistic = ~ continuous_summary_fns(",
    "                   c(\"N\", \"mean\", \"sd\", \"median\", \"min\", \"max\"))),",
    "  ard_categorical(variables = c(AGEGR1, SEX, ETHNIC),",
    "                  statistic = ~ c(\"n\", \"p\")),",
    "  .total_n = TRUE)",
    "data <- ard_normalize(ard)", sep = "\n")
  p <- add_output(p, "L1", type = "listing")
  s <- create_study(withr_tempdir(), "S2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  expect_equal(study_files(s, "data")$file, "adsl.rds")
  expect_equal(nrow(read_data_head(study_files(s)$path, n = 5)), 5)

  expect_equal(study_status(s)$status, c("not run", "todo"))
  st <- run_study(s)
  expect_equal(st$status, c("ok", "todo"))
  expect_true(file.exists(file.path(s$path, "output", "tfl", "DM.rtf")))
  expect_true(file.exists(file.path(s$path, "output", "ard", "DM.rds")))
  expect_true(file.exists(file.path(s$path, "logs", "DM.log")))

  # saving an unchanged study leaves the RTF current
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "ok")

  # a definition changed after the RTF was made
  Sys.sleep(1.1)
  s$planner$sheets$variables$label[1] <- "Age"
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "outdated")

  # a program that fails
  writeLines("stop('boom')", file.path(s$path, "programs", "DM.R"))
  st <- run_study(s, "DM")
  expect_equal(st$status[1], "error")
})
