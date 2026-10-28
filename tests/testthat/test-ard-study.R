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
    output_id = c("DM", "DM"), analysis_id = c("GROUPN", "AGE"),
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
  # normalized in the table's plan (tflspec #302), not here
  expect_false(any(grepl("normalize_ard(", code, fixed = TRUE)))
  expect_true(any(grepl("still to be written", data_lines(p, "L1"))))
  # its data step its own code: that code, which makes the data itself (no
  # study ARD read for it)
  p <- set_step_code(p, "DM", "data", "ard <- my_ard()")
  expect_true("ard <- my_ard()" %in% data_lines(p, "DM"))
  expect_false(any(grepl("ard.rds", data_lines(p, "DM"))))
  expect_false(any(grepl("make_ard", autoexec_code(p))))
})

test_that("the definition is saved, reopened and run", {
  skip_if_not_installed("cards")
  skip_on_cran()
  local_home2()
  s <- create_study("A1", planner = ard_planner())
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  # no workbook: the ARD definition in the report's YAML file, the catalog
  # in data.yml, and the programs
  expect_false(file.exists(file.path(s$path, "spec", "ard_spec.xlsx")))
  expect_false(file.exists(file.path(s$path, "spec", "ard_definition.json")))
  expect_true(file.exists(file.path(s$path, "spec", "data.yml")))
  expect_false(is.null(tflspec::tfl_read_output(file.path(s$path, "spec", "outputs", "DM.yml"))$ard))
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
  expect_setequal(unique(r$ard$analysis_id), c("GROUPN", "AGE"))
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

  # the files exported, and imported back: the ARD definition the same
  d <- withr_tempdir()
  export_spec_files("A1", d)
  expect_true(file.exists(file.path(d, "spec", "outputs", "DM.yml")))
  g2 <- import_spec_files("A1", file.path(d, "spec"))
  expect_identical(.study_parts(g2)[grep("^ard:", names(.study_parts(g2)))],
                   .study_parts(g)[grep("^ard:", names(.study_parts(g)))])
})

test_that("the study's analyses are exported as CDISC ARS", {
  skip_on_cran()
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
  # a study's own ARD function: its file and its statistics reach the ARS
  # (read from the study folder, not run)
  fdir <- file.path(s$path, "programs", "ard", "functions")
  dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ard_mine <- cards::as_cards_fn(",
               "  function(data, by, variables, ...) stop(\"not run\"),",
               "  stat_names = c(\"estimate\", \"p.value\"))"),
             file.path(fdir, "ard_mine.R"))
  s$planner$ard$study <- rbind(s$planner$ard$study, data.frame(
    key = "source", value = "programs/ard/functions/ard_mine.R"))
  s$planner$ard$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  s$planner$ard$analyses$method[2] <- "ard_mine"
  ars <- tflspec::tfl_read_ars_json(export_ars(s, d)[["json"]])
  code <- unlist(lapply(ars$methods, function(m) m$codeTemplate$code))
  expect_true(any(grepl("programs/ard/functions/ard_mine.R", code, fixed = TRUE)))
  expect_true(any(grepl("ard_mine(data", code, fixed = TRUE)))
  s$planner$ard$analyses <- s$planner$ard$analyses[0, ]
  expect_error(export_ars(s, d), "no analyses")
})

test_that("the ARS is built once with its problems, each with its report", {
  skip_on_cran()
  local_home2()
  p <- ard_planner()
  s <- create_study("A3", planner = p)
  b <- .ars_build(s)
  expect_identical(b$profile, "cdisc")
  expect_identical(names(b$problems), c("kind", "output_id", "where", "item", "note"))
  # a blank purpose: a check on each analysis, about its report
  pu <- b$problems[b$problems$kind == "check" & b$problems$item %in% "purpose", ]
  expect_gt(nrow(pu), 0L)
  expect_true(all(pu$output_id == "DM"))
  # the row, as a Review item: the report's analyses, the analysis, the field
  j <- .ars_jump_row(pu[pu$where == "Analysis An_DM_AGE", ])
  expect_identical(j[c("output_id", "sheet", "row", "field")],
                   list(output_id = "DM", sheet = "analyses", row = "AGE",
                        field = "purpose"))
  # what it writes is what it built; the files as export_ars() wrote them
  d <- withr_tempdir()
  f <- .ars_write(b, d, parts = "json")
  expect_identical(names(f), "json")
  expect_identical(basename(f[["json"]]), "A3_ars.json")
  expect_identical(names(export_ars(s, d)), c("json", "xlsx", "check"))
  # the siera profile, built again
  expect_identical(.ars_build(s, "siera")$profile, "siera")
  expect_match(basename(.ars_write(.ars_build(s, "siera"), d, "json")[["json"]]),
               "_siera.json$")
})

test_that("an ARS row names its report: the longest output id that fits", {
  ids <- c("T-1", "T-1-1", "F-2")
  expect_identical(
    .ars_output_of(c("Analysis An_T-1-1_AGE", "Analysis An_T-1_AGE",
                     "F-2 / KM", "F-2", "SAF", NA), ids),
    c("T-1-1", "T-1", "F-2", "F-2", NA, NA))
  # a population and an analysis data are found in their own sheets
  expect_identical(.ars_jump_row(list(output_id = NA, where = "SAF",
                                      item = "populations$derive", note = "x"))$sheet,
                   "populations")
  j <- .ars_jump_row(list(output_id = "T-1", where = "T-1 / SYSBP",
                          item = "analysis_data$subjects",
                          note = "advs_saf: `adsl_saf` -- ..."))
  expect_identical(j[c("sheet", "row")], list(sheet = "analysis_data", row = "advs_saf"))
})

test_that("a report's code lists reach its ARD program", {
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
  p <- set_codelist(p, "DM", data.frame(
    variable = "AGEGR1", value = c("<65", "65-80", ">80", "unknown"),
    order = c("1", "2", "3", "4"), stringsAsFactors = FALSE))
  s <- create_study("A2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  prog <- readLines(file.path(s$path, "programs", "ard", "DM.R"))
  # (the code lists put on as tflspec writes it: the list at the head,
  # set_levels() on the data the analyses read)
  expect_true("cl_agegr1 <- c(\"<65\", \"65-80\", \">80\", \"unknown\")" %in% prog)
  expect_true(any(grepl("set_levels(", prog, fixed = TRUE)))
  expect_true(any(grepl("AGEGR1 = cl_agegr1", prog, fixed = TRUE)))
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
  o$planner <- set_codelist(o$planner, "DM", data.frame(variable = "AGEGR1",
    value = "none", order = "5", stringsAsFactors = FALSE))
  expect_equal(ard_status(o)$state, "outdated")
})

test_that("the analysis data is saved, reopened and run (tflspec #135)", {
  skip_if_not_installed("cards")
  skip_on_cran()
  local_home2()
  p <- ard_planner()
  a <- p$ard$analyses
  p$ard$analysis_data <- .normalize_ard_sheet(data.frame(
    output_id = a$output_id[a$analysis_id == "AGE"][1L],
    data_id = "adsl_saf", from = "ADSL", population_id = "SAF",
    where = "AGE >= 18"), "analysis_data")
  a$data[a$analysis_id == "AGE"] <- "adsl_saf"
  a$population_id[a$analysis_id == "AGE"] <- NA
  p$ard$analyses <- .normalize_ard_sheet(a, "analyses")
  s <- create_study("A2", planner = p)
  saveRDS(cards::ADSL, file.path(s$path, "data", "adam", "adsl.rds"))
  o <- open_study("A2")
  expect_identical(o$planner$ard$analysis_data, p$ard$analysis_data)
  expect_identical(o$planner$ard$analyses$data, p$ard$analyses$data)
  code <- readLines(file.path(s$path, "programs", "ard", "DM.R"))
  # (the analysis set's data: pop_saf before tflspec#459, adsl_sub after)
  expect_true(any(code %in% c("adsl_saf <- filter(pop_saf, AGE >= 18)",
                              "adsl_saf <- filter(adsl_sub, AGE >= 18)",
                              "  filter(AGE >= 18) |>")))
  r <- run_ard(o, "DM")
  expect_null(r$error)
  expect_identical(unique(r$ard$population_id), "SAF")
})

test_that("a report's ARD program: only the code lists of what its analyses read (tflspec #170)", {
  skip_on_cran()
  skip_if(utils::packageVersion("tflspec") < "0.0.24.9058")
  local_home2()
  p <- ard_planner()
  p <- set_codelist(p, "DM", data.frame(
    variable = c("SEX", "SEX", "TRT01A", "TRT01A"), value = c("F", "M", "Drug", "Placebo"),
    order = c("1", "2", "2", "1"), stringsAsFactors = FALSE))
  expect_identical(unique(.study_codelists(p)$output_id), "DM")
  s <- create_study("A3", planner = p)
  prog <- readLines(file.path(s$path, "programs", "ard", "DM.R"))
  expect_true("cl_trt01a <- c(\"Placebo\", \"Drug\")" %in% prog)
  expect_true(any(grepl("TRT01A = cl_trt01a", prog, fixed = TRUE)))
  # SEX only prints (DM's analyses do not read it): not in the ARD program
  expect_false(any(grepl("cl_sex", prog, fixed = TRUE)))
})

test_that("an ARD definition that does not hold is said: on save, in the Review, by the runner (#323)", {
  skip_on_cran()
  local_home2()
  p <- ard_planner()
  # an analysis data made by code, `from` left blank: not a valid definition
  p$ard$analysis_data <- .normalize_ard_sheet(data.frame(
    output_id = "DM", data_id = "adsl_x", code = "adsl"), "analysis_data")
  s <- create_study("A2", planner = p)
  # saving: no ARD program, and a message that says why (the rest is saved)
  expect_message(s2 <- save_study(s), "no ARD program was written")
  expect_match(s2$ard_problem, "from", fixed = TRUE)
  expect_false(file.exists(file.path(s$path, "programs", "ard", "DM.R")))
  expect_true(file.exists(file.path(s$path, "programs", "tfl", "DM.R")))
  # the Review: an error row saying no ARD program is written
  pr <- .check_spec(open_study("A2"))
  hit <- grepl("saving writes no ARD program", pr$message, fixed = TRUE)
  expect_true(any(hit))
  expect_identical(unique(pr$severity[hit]), "error")
  # an official run of a study never saved with a valid one: says why
  expect_error(run_batch(open_study("A2"), "ard"), "does not hold writes no ARD program")
  # a valid definition saves without the message
  s3 <- create_study("A3", planner = ard_planner())
  expect_null(suppressMessages(save_study(s3))$ard_problem)
  # the runner: a program it is to run that is not there is said as such,
  # not failed on a log never written
  unlink(file.path(s3$path, "programs", "ard", "DM.R"))
  b <- run_batch(open_study("A3"), "ard", code = FALSE)
  expect_false(b$ok)
  r <- b$result[b$result$program == "programs/ard/DM.R", ]
  expect_identical(r$status, "ERROR")
  expect_match(r$note, "no program programs/ard/DM.R: save the study", fixed = TRUE)
})
