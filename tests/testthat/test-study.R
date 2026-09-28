sample_planner <- function() {
  d <- system.file("extdata", "ard-spec", package = "tflspec")
  read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
}

# a fresh home and study root for one test
local_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

test_that("setup makes the home and remembers where studies go", {
  home <- local_home()
  expect_true(dir.exists(file.path(home, "studies")))
  expect_equal(studies_root(), normalizePath(file.path(home, "ws"), "/"))
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws2")))
  expect_match(studies_root(), "ws2$")
})

test_that("a new study has its layout and is registered", {
  home <- local_home()
  s <- create_study("ABC-101", title = "A phase 2 study", phase = "2")
  for (d in study_layout()) expect_true(dir.exists(file.path(s$path, d)))
  expect_true(file.exists(file.path(s$path, "study.yml")))
  expect_true(file.exists(file.path(s$path, "ABC-101.Rproj")))
  expect_true(file.exists(file.path(s$path, "spec", "table_spec.xlsx")))
  expect_true(file.exists(file.path(home, "studies", "ABC-101",
                                    "state.json")))
  expect_error(create_study("ABC-101"), "already registered")
  expect_error(create_study("bad id"), "study id")

  l <- list_studies()
  expect_equal(l$study_id, "ABC-101")
  expect_equal(l$title, "A phase 2 study")
  expect_true(l$folder)
  expect_equal(tflplanner_config()$last_study, "ABC-101")
})

test_that("a study opens as it was last saved, not from its workbooks", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  s$planner$outputs$data_code[1] <- "data <- tfl_ard_normalize(my_ard)\n# 2"
  s$planner$outputs$description[2] <- "有害事象"
  s$planner$setup <- "library(cards)"
  s$planner$sheets$variables$label[1] <- "Age"
  s$meta$title <- "Title"
  s <- save_study(s)

  # the workbooks in spec/ are an output: editing them changes nothing
  unlink(file.path(s$path, "spec"), recursive = TRUE)
  o <- open_study("S1")
  expect_identical(o$planner$sheets, s$planner$sheets)
  expect_identical(o$planner$outputs, s$planner$outputs)
  expect_identical(o$planner$setup, s$planner$setup)
  expect_identical(o$planner$study, s$planner$study)
  expect_equal(o$meta$title, "Title")
  expect_equal(o$path, s$path)
})

test_that("each save that changes the study keeps the previous state", {
  home <- local_home()
  s <- create_study("S1", planner = sample_planner())
  hist <- file.path(home, "studies", "S1", "history")
  expect_length(list.files(hist), 0)
  s <- save_study(s)
  expect_length(list.files(hist), 0)              # nothing changed
  s$planner$outputs$description[1] <- "changed"
  s <- save_study(s)
  expect_length(list.files(hist), 1)
})

test_that("an existing folder is registered from its workbooks", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  unregister_study("S1")
  expect_equal(nrow(list_studies()), 0)
  expect_true(dir.exists(s$path))
  expect_error(open_study("S1"), "not registered")

  r <- open_study(s$path)                         # registers it
  expect_equal(nrow(list_studies()), 1)
  expect_identical(r$planner$sheets, s$planner$sheets)

  # a moved folder is followed when opened from where it is
  to <- file.path(dirname(s$path), "moved")
  file.rename(s$path, to)
  expect_false(list_studies()$folder)
  m <- open_study(to)
  expect_equal(m$path, normalizePath(to, "/"))
  expect_true(list_studies()$folder)
})

test_that("workbooks export and import", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  paths <- export_spec(s, withr_tempdir())
  e <- create_study("S2")
  e <- import_spec(e, paths)
  expect_identical(e$planner$sheets, s$planner$sheets)
  expect_equal(unname(e$planner$study[["output_path"]]), "output/tfl")
})

test_that("saving keeps an edited program unless asked to regenerate", {
  local_home()
  s <- create_study("S1", planner = sample_planner())
  # the workbooks, the programs, batch.R, the two autoexec programs, fig_setup.R
  expect_equal(sum(s$files$status == "written"), 2 + 5 + 4)
  s0 <- save_study(s)
  expect_true(all(s0$files$status == "unchanged"))
  f <- file.path(s$path, "programs", "tfl", "DM.R")
  expect_equal(study_status(s)$program_state, rep("current", 5))

  # an untouched program follows the definition
  s$planner$outputs$data_code[1] <- "data <- tfl_ard_normalize(my_ard)"
  expect_equal(study_status(s)$program_state[1], "generated")
  expect_equal(study_status(s)$status[1], "unsaved")
  s <- save_study(s)
  expect_true("data <- tfl_ard_normalize(my_ard)" %in% readLines(f))
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
  local_home()
  p <- sample_planner()
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
    "data <- tfl_ard_normalize(ard)", sep = "\n")
  p <- add_output(p, "L1", type = "listing")
  s <- create_study("S2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  expect_equal(study_files(s, "data")$file, "adsl.rds")
  expect_equal(nrow(read_data_head(study_files(s)$path, n = 5)), 5)

  expect_equal(study_status(s)$status, c("not run", "todo"))
  st <- run_study(s)
  expect_equal(st$status, c("ok", "todo"))
  expect_true(file.exists(file.path(s$path, "output", "tfl", "DM.rtf")))
  expect_true(file.exists(file.path(s$path, "output", "ard", "DM.rds")))
  expect_true(file.exists(file.path(s$path, "logs", "preview", "DM.log")))

  # saving an unchanged study leaves the RTF current
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "ok")

  # a definition changed after the RTF was made
  Sys.sleep(1.1)
  s$planner$sheets$variables$label[1] <- "Age"
  s <- save_study(s)
  expect_equal(study_status(s)$status[1], "outdated")

  # a program that fails
  writeLines("stop('boom')", file.path(s$path, "programs", "tfl", "DM.R"))
  st <- run_study(s, "DM")
  expect_equal(st$status[1], "error")
})
