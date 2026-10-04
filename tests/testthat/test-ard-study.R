local_home2 <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

ard_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p <- add_output(p, "L1", type = "listing")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = c("DM", "DM"), analysis_id = c("BIGN", "AGE"),
    method = c("categorical", "continuous"), population_id = "SAF",
    by = c(NA, "TRT01A"), variables = c("TRT01A", "AGE"))
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("a report's analyses are edited like the other sheets", {
  p <- ard_planner()
  p <- add_output(p, "AE")
  expect_equal(nrow(ard_rows(p, "analyses", "DM")), 2)
  rows <- ard_rows(p, "analyses", "AE")
  rows$output_id <- NULL
  rows[1, ] <- NA
  rows$analysis_id[1] <- "ANY"
  rows$method[1] <- "subjects"
  p <- set_ard_rows(p, "analyses", "AE", rows)
  expect_equal(p$ard$analyses$output_id, c("DM", "DM", "AE"))
  q <- copy_output(p, "DM", "DM2")
  expect_equal(nrow(ard_rows(q, "analyses", "DM2")), 2)
  q <- rename_output(q, "DM2", "DM3")
  expect_equal(nrow(ard_rows(q, "analyses", "DM3")), 2)
  q <- remove_output(q, "DM3")
  expect_identical(q$ard, p$ard)
})

test_that("a table the ARD definition serves reads its part of the study ARD", {
  p <- ard_planner()
  code <- data_lines(p, "DM")
  expect_true(any(grepl("readRDS(\"output/ard/ard.rds\")", code, fixed = TRUE)))
  expect_true(any(grepl("data <- normalize_ard(ard)", code, fixed = TRUE)))
  expect_true(any(grepl("still to be written", data_lines(p, "L1"))))
  p$outputs$data_code[1] <- "ard <- my_ard()"
  expect_false(any(grepl("ard.rds", data_lines(p, "DM"))))
  expect_false(any(grepl("make_ard", autoexec_code(p))))
})

test_that("the definition is saved, reopened and run", {
  skip_if_not_installed("cards")
  skip_on_cran()
  local_home2()
  s <- create_study("A1", planner = ard_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  # no workbook: the ARD definition's copy in the folder, and the programs
  expect_false(file.exists(file.path(s$path, "spec", "ard_spec.xlsx")))
  expect_true(file.exists(file.path(s$path, "spec", "ard_definition.json")))
  for (f in c("ard_setup.R", "DM.R", "autoexec_ard.R")) {
    expect_true(file.exists(file.path(s$path, "programs", "ard", f)))
  }
  expect_true(file.exists(file.path(s$path, "programs", "tfl", "DM.R")))
  s2 <- save_study(s)
  expect_true(all(s2$files$status[grepl("ard", s2$files$file)] == "unchanged"))
  o <- open_study("A1")
  expect_identical(o$planner$ard, s$planner$ard)

  r <- run_ard(o, "DM")
  expect_null(r$error)
  expect_setequal(unique(r$ard$analysis_id), c("BIGN", "AGE"))
  v <- ard_view(r$ard)
  expect_true(all(c("analysis_id", "variable", "stat_name", "stat") %in%
                    names(v)))
  expect_false(file.exists(file.path(s$path, "output", "ard", "ard.rds")))
  # a preview of one output: the working ARD, no log
  u <- update_study_ard(o, "DM")
  expect_true(u$ok)
  expect_true(file.exists(file.path(s$path, "output", "ard", "ard.rds")))
  expect_equal(ard_status(o)$state, "built")
  expect_equal(nrow(list_batches(o)), 0L)

  # an official run: a batch folder with the logs, the ARD and the code
  b <- run_batch(o, "ard")
  expect_true(b$ok)
  expect_match(basename(b$batch), "^[0-9]{8}_[0-9]{6}_ard$")
  for (f in c("logs/ard/DM.log", "output/ard/ard.rds",
              "output/ard/ard_status.csv", "run.csv", "batch.txt",
              "code/programs/ard/DM.R", "code/programs/ard/ard_setup.R",
              "code/programs/batch.R")) {
    expect_true(file.exists(file.path(b$batch, f)), label = f)
  }
  expect_false(file.exists(file.path(b$batch, "code", "programs", "tfl",
                                     "autoexec_report.R")))
  expect_equal(b$result$status, "OK")
  Sys.sleep(1.1)
  b2 <- run_batch(o, "ard", code = FALSE)
  expect_false(dir.exists(file.path(b2$batch, "code")))
  expect_equal(nrow(list_batches(o)), 2L)

  # the reports: previews keep what they print in logs/preview
  st <- run_study(o, "DM")
  expect_equal(st$status[st$output_id == "DM"], "error")   # no table spec
  expect_true(file.exists(file.path(s$path, "logs", "preview", "DM.log")))

  # a failing ARD program leaves the study ARD and records its error
  o2 <- o
  o2$planner$ard$analyses$method[2] <- "cards::ard_nope"
  o2 <- save_study(o2)
  u <- update_study_ard(o2, "DM")
  expect_false(u$ok)
  expect_match(u$error, "ard_nope")
  expect_equal(ard_status(o2)$state, "error")
  expect_true(file.exists(file.path(s$path, "output", "ard", "ard.rds")))

  bad <- o
  bad$planner$ard$analyses$method[2] <- "cards::ard_nope"
  r <- run_ard(bad, "DM")
  expect_null(r$ard)
  expect_match(r$error, "ard_nope")

  # registering a folder reads its ARD definition
  unregister_study("A1")
  g <- register_study(s$path)
  expect_identical(g$planner$ard, o2$planner$ard)

  # ard_spec.xlsx: an export, and an import
  d <- withr_tempdir()
  f <- export_spec(g, d)
  expect_true(file.exists(file.path(d, "ard_spec.xlsx")))
  g2 <- g
  g2$planner$ard$analyses <- g2$planner$ard$analyses[0, ]
  g2 <- import_spec(g2, file.path(d, "ard_spec.xlsx"))
  expect_identical(g2$planner$ard, g$planner$ard)
  expect_identical(g2$planner$sheets, g$planner$sheets)
})

test_that("the study's analyses are exported as CDISC ARS", {
  local_home2()
  p <- ard_planner()
  p$ard$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  s <- create_study("A2", planner = p)
  d <- withr_tempdir()
  f <- export_ars(s, d)
  expect_true(all(file.exists(f)))
  expect_identical(basename(f[["json"]]), "A2_ars.json")
  ars <- tflspec::tfl_read_ars_json(f[["json"]])
  expect_identical(ars$id, "A2")
  expect_identical(vapply(ars$outputs, `[[`, "", "id"), "DM")
  expect_true("ReportingEvent" %in% readxl::excel_sheets(f[["xlsx"]]))
  ck <- utils::read.csv(f[["check"]], stringsAsFactors = FALSE)
  expect_false(any(ck$kind == "check" & ck$item == "purpose"))
  # a blank purpose is listed
  s$planner$ard$analyses$purpose <- NA
  ck <- utils::read.csv(export_ars(s, d)[["check"]], stringsAsFactors = FALSE)
  expect_true(any(ck$item == "purpose"))
  s$planner$ard$analyses <- s$planner$ard$analyses[0, ]
  expect_error(export_ars(s, d), "no analyses")
})

test_that("the study's code lists reach the ARD programs", {
  skip_if_not_installed("cards")
  skip_on_cran()
  skip_if(utils::packageVersion("tflspec") < "0.0.24.9031")
  local_home2()
  p <- ard_planner()
  p$ard$analyses$method[1] <- "categorical"
  p$ard$analyses <- rbind(p$ard$analyses, p$ard$analyses[1, ])
  p$ard$analyses$analysis_id[3] <- "AGEGR"
  p$ard$analyses$by[3] <- "TRT01A"
  p$ard$analyses$variables[3] <- "AGEGR1"
  p <- set_codelist(p, data.frame(
    variable = "AGEGR1", value = c("<65", "65-80", ">80", "unknown"),
    order = c("1", "2", "3", "4"), stringsAsFactors = FALSE))
  s <- create_study("A2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  prog <- readLines(file.path(s$path, "programs", "ard", "DM.R"))
  expect_true(any(grepl(".codelists <- list(", prog, fixed = TRUE)))
  expect_true(any(grepl("adsl <- .levels(adsl)", prog, fixed = TRUE)))
  # the ARD counts the value no record has, in the code list's order
  o <- open_study("A2")
  u <- update_study_ard(o, "DM")
  expect_true(u$ok)
  a <- readRDS(file.path(s$path, "output", "ard", "ard.rds"))
  a <- a[a$analysis_id == "AGEGR" & a$stat_name == "n", ]
  lv <- vapply(a$variable_level, as.character, "")
  expect_identical(unique(lv), c("<65", "65-80", ">80", "unknown"))
  expect_true(all(unlist(a$stat[lv == "unknown"]) == 0))
  expect_equal(ard_status(o)$state, "built")
  # a new code list makes the ARD out of date
  o$planner <- set_codelist(o$planner, data.frame(variable = "AGEGR1",
    value = "none", order = "5", stringsAsFactors = FALSE))
  expect_equal(ard_status(o)$state, "outdated")
})
