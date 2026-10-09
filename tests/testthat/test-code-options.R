# A study's programs as its programs/study_setup.R has it (#268): its
# folders through the variables of its part 2, no `pkg::` for the packages
# it attaches

code_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = "DM", analysis_id = "AGE", method = "continuous",
    population_id = "SAF", by = "TRT01A", variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("the packages a study's setup attaches: library() and require(), anywhere", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "programs"))
  writeLines(c("library(cards)", "suppressPackageStartupMessages(library(dplyr))",
               "require(\"tidyr\")", "library(package = gt)",
               "# library(not_this)", "x <- \"library(nor_this)\""),
             file.path(d, "programs", "study_setup.R"))
  expect_identical(.study_setup_packages(d), c("cards", "dplyr", "tidyr", "gt"))
  # not written yet: the company's setup code it will start with
  std <- .study_setup_packages(file.path(d, "none"))
  expect_true(all(c("cards", "dplyr", "rtfreporter") %in% std))
  # (the programs call programs/study_helpers.R, not tflspec)
  expect_false("tflspec" %in% std)
})

test_that("a study's programs read its folders by their variables; previews as they are", {
  local_home()
  s <- create_study("CO-1", planner = code_planner())
  ard <- readLines(file.path(s$path, "programs", "ard", "DM.R"))
  expect_true(any(grepl("readRDS(file.path(path_adam, \"adsl.rds\"))", ard,
                        fixed = TRUE)))
  # the setup attaches cards and dplyr: no pkg::, no library() again
  expect_false(any(grepl("cards::|dplyr::|tflspec::", ard[!startsWith(ard, "#")])))
  setup <- readLines(file.path(s$path, "programs", "ard", "ard_setup.R"))
  expect_false(any(grepl("^library\\(", setup)))
  rep <- readLines(file.path(s$path, "programs", "tfl", report_info(s$planner, "DM")$program))
  expect_true(any(grepl("readRDS(file.path(path_ard, \"ard.rds\")) |>", rep, fixed = TRUE)))
  expect_true(any(grepl("  filter(output_id == \"DM\") |>", rep, fixed = TRUE)))
  expect_true(any(grepl("saveRDS(data, file.path(path_ard, paste0(report_id, \".rds\")))",
                        rep, fixed = TRUE)))
  # written once: saved again, the programs are as they were
  expect_identical(unique(study_status(s)$program_state), "current")
  s2 <- save_study(s)
  expect_false(any(s2$files$status[grepl("programs", s2$files$file)] == "rewritten"))
  # what the app runs itself (a report's data part): as it is
  dl <- data_lines(s$planner, "DM")
  expect_true(any(grepl("readRDS(\"output/ard/ard.rds\")", dl, fixed = TRUE)))
  expect_true(any(grepl("dplyr::filter(", dl, fixed = TRUE)))
})

test_that("a study whose setup does not attach dplyr keeps dplyr::", {
  local_home()
  s <- create_study("CO-2", planner = code_planner())
  f <- file.path(s$path, "programs", "study_setup.R")
  txt <- readLines(f)
  writeLines(txt[txt != "library(dplyr)"], f)
  s <- open_study("CO-2")
  rep <- program_code(s$planner, "DM")
  expect_true(any(grepl("  dplyr::filter(output_id == \"DM\") |>", rep, fixed = TRUE)))
  ard <- ard_program_code(.ard_spec(s$planner$ard), "DM")
  expect_true(any(grepl("library(dplyr)", ard_setup_code(.ard_spec(s$planner$ard)),
                        fixed = TRUE)))
  expect_false(any(grepl("dplyr::", ard[!startsWith(ard, "#")], fixed = TRUE)))
})
