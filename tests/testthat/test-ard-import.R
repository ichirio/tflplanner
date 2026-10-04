# ARDs made elsewhere, taken into a study (input/ard/).

imp_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

imp_planner <- function() {
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(output_id = "DM", analysis_id = "AGE",
                               method = "continuous", population_id = "SAF",
                               by = "TRT01A", variables = "AGE")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p
}

test_that("an ARD made elsewhere is taken in, checked, recorded and used", {
  skip_if_not_installed("cards")
  skip_on_cran()
  imp_home()
  s <- create_study("IMP1", planner = imp_planner())
  # a CRO's ARD for DM, written as JSON
  cro <- cards::ard_summary(cards::ADSL, by = TRT01A, variables = AGE)
  cro <- dplyr::mutate(cro, output_id = "DM", .before = 1L)
  f <- tempfile(fileext = ".json")
  suppressWarnings(tflspec::tfl_write_ard(cro, f))
  row <- import_ard(s, f, source = "CRO X")
  expect_identical(row$import_id, "IMP001")
  expect_identical(row$outputs, "DM")
  expect_identical(row$state, "in use")
  dest <- file.path(s$path, "input", "ard", row$file)
  expect_true(file.exists(dest))
  expect_identical(unname(tools::md5sum(dest)), row$md5)
  expect_identical(nrow(ard_imports(s)), 1L)
  # a second with the same name is kept apart
  row2 <- import_ard(s, f, source = "CRO X, again")
  expect_false(identical(row2$file, row$file))
  expect_identical(ard_imports(s)$import_id, c("IMP001", "IMP002"))

  # the report reads it once its report row says so
  expect_null(.ard_import_of(s$planner, "DM"))
  s$planner <- use_imported_ard(s$planner, "DM", row$file)
  expect_identical(.ard_import_of(s$planner, "DM"), row$file)
  code <- data_lines(s$planner, "DM")
  expect_true(any(grepl(paste0("tfl_read_ard(\"input/ard/", row$file, "\")"),
                        code, fixed = TRUE)))
  expect_false(any(grepl("ard.rds", code, fixed = TRUE)))
  a <- study_ard(s, "DM")
  expect_false("output_id" %in% names(a))
  expect_identical(nrow(a), nrow(cro))
  # back to its own ARD definition
  s$planner <- use_imported_ard(s$planner, "DM", NULL)
  expect_null(.ard_import_of(s$planner, "DM"))
  expect_true(any(grepl("ard.rds", data_lines(s$planner, "DM"), fixed = TRUE)))

  # removed: on the record still, the file kept
  d <- remove_imported_ard(s, "IMP002")
  expect_identical(d$state, c("in use", "removed"))
  expect_true(file.exists(file.path(s$path, "input", "ard", row2$file)))
  expect_error(remove_imported_ard(s, "IMP999"), "No import")
})

test_that("the check of an ARD taken in is on the record", {
  skip_if_not_installed("cards")
  skip_on_cran()
  imp_home()
  s <- create_study("IMP2", planner = imp_planner())
  # no stat_name: not an ARD (a group without its level is not an error
  # since tflspec 0.0.24.9035 -- a test across groups has that shape)
  bad <- data.frame(output_id = "DM", group1 = "TRT01A", variable = "AGE",
                    stat = 50)
  f <- tempfile(fileext = ".rds")
  saveRDS(bad, f)
  row <- import_ard(s, f)
  expect_match(row$check, "error")
  expect_true(any(grepl("stat_name", attr(row, "check")$message)))
})

test_that("a company's own ARD function is used by a study, as a copy", {
  skip_if_not_installed("cards")
  skip_on_cran()
  home <- imp_home()
  dir.create(file.path(home, "standards", "ard_functions"), recursive = TRUE)
  writeLines(c(
    "ard_mean_only <- function(data, by, variables, ...) {",
    "  cards::ard_summary(data, by = {{ by }}, variables = {{ variables }},",
    "    statistic = ~ cards::continuous_summary_fns(\"mean\"))",
    "}",
    "helper <- 1"),
    file.path(home, "standards", "ard_functions", "means.R"))
  own <- company_ard_functions(home)
  expect_identical(own$name, "ard_mean_only")
  p <- check_company_ard_function("ard_mean_only", cards::ADSL, by = ARM,
                                  variables = AGE, stat_names = "mean", home = home)
  expect_identical(p$level[p$level != "note"], character())
  s <- create_study("OWN1", planner = imp_planner())
  s <- use_company_ard_function(s, "ard_mean_only", home = home)
  rel <- "programs/ard/functions/means.R"
  expect_true(file.exists(file.path(s$path, rel)))
  st <- s$planner$ard$study
  expect_identical(st$value[st$key == "source"], rel)
  # again: no second copy, no second entry; the study's own file is kept
  writeLines("ard_mean_only <- function(...) NULL", file.path(s$path, rel))
  s <- suppressMessages(use_company_ard_function(s, "ard_mean_only", home = home))
  expect_identical(s$planner$ard$study$value[s$planner$ard$study$key == "source"], rel)
  expect_identical(readLines(file.path(s$path, rel)), "ard_mean_only <- function(...) NULL")
  expect_error(use_company_ard_function(s, "nope", home = home), "no ARD function")
})

test_that("the check's messages are shown in the session's language", {
  p <- data.frame(
    level = c("error", "warning", "warning", "note", "note"),
    check = c("shape", "statistics", "rows", "cards", "cards"),
    message = c("no column stat_name: not an ARD",
                "a template reads {mean}, a statistic the ARD does not have",
                "the table's rows name AGEGR1, which is neither a group nor a variable of the ARD",
                "Expecting a row with `stat_name = 'method'`, but it is not present.",
                "something else cards says"),
    stringsAsFactors = FALSE)
  # the note about cards' own method rows says nothing about the report
  q <- .drop_method_note(p)
  expect_identical(nrow(q), 4L)
  ja <- function(x) tr(x, "ja")
  v <- .check_view(q, ja)
  expect_identical(v$message[1:3], c(
    "stat_name の列がありません（ARD ではありません）",
    "テンプレートが {mean} を読みますが、ARD にその統計量がありません",
    "表の rows に AGEGR1 とありますが、ARD のグループにも変数にもありません"))
  expect_match(v$message[4], "^cards による ARD の形の確認: something else")
  # English: as tflspec wrote them
  expect_identical(.check_view(q)$message[1:3], q$message[1:3])
})
