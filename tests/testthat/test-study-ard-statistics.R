# The study's own statistics (spec/defaults.yml `ard_statistics:`,
# tflspec#1006) reach every place tflplanner uses the statistics catalog:
# the checks, the programs, their cache and the analysis screen's pickers.

# a tflspec with the study layer
skip_without_study_layer <- function() {
  if (is.null(get0(".ard_statistics_of_study", envir = asNamespace("tflspec"),
                   mode = "function", inherits = FALSE))) {
    skip("tflspec without the study's own statistics (tflspec#1006)")
  }
}

# a fictional study: one continuous analysis of AGE, with `stats` asked
gm_study <- function(id, stats = "N|gmean_pos") {
  s <- create_study(id)
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner$ard$datasets <- .normalize_ard_sheet(data.frame(
    dataset = "ADSL", level = "adam", path = "data/adam/adsl.rds"), "datasets")
  s$planner$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""), "populations")
  s$planner <- add_output(s$planner, "T1", type = "table")
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", data.frame(
    analysis_id = "A1", method = "continuous", population_id = "SAF",
    by = "ARM", variables = "AGE", statistics = stats))
  s
}

# the study's defaults.yml with an `ard_statistics:` block of one row
# (the geometric mean of the values > 0) appended
gm_block <- function(path, fun = "function(x) exp(mean(log(x[x > 0])))",
                     label = "Geometric mean (values > 0)") {
  f <- file.path(path, study_layout()[["spec"]], "defaults.yml")
  old <- if (file.exists(f)) readLines(f, encoding = "UTF-8", warn = FALSE)
  writeLines(c(old, "ard_statistics:",
               "  - statistic: gmean_pos",
               "    kind: continuous",
               paste0("    label: ", label),
               "    fmt: xx.xx",
               paste0("    fun: \"", fun, "\"")), f, useBytes = TRUE)
  f
}

test_that("without an open study, or a study without the block, the catalog is the company's", {
  local_home()
  company <- company_standards()$ard_statistics
  rownames(company) <- NULL
  .use_study(NULL)
  expect_identical(.std_ard_statistics(), company)
  s <- create_study("NB")
  .use_study(s$path)
  expect_identical(.std_ard_statistics(), company)
  expect_identical(.ard_standards()$ard_statistics, company)
})

test_that("a study's own statistic is in the catalog, checked and written into its programs", {
  skip_on_cran()
  skip_without_study_layer()
  local_home()
  s <- gm_study("GM")
  # without the block: the check calls it unknown
  .use_study(s$path)
  expect_false("gmean_pos" %in% .std_ard_statistics()$statistic)
  expect_error(.ard_spec(s$planner$ard), "gmean_pos")
  gm_block(s$path)
  st <- .std_ard_statistics("continuous")
  row <- st[st$statistic == "gmean_pos", , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$group, "study")
  expect_identical(row$fun, "function(x) exp(mean(log(x[x > 0])))")
  # the company's rows stay where they were; the study's comes after them
  company <- company_standards()$ard_statistics
  all <- .std_ard_statistics()
  expect_identical(all$statistic[seq_len(nrow(company))], company$statistic)
  expect_identical(utils::tail(all$statistic, 1L), "gmean_pos")
  # the check accepts it
  spec <- .ard_spec(s$planner$ard)
  expect_s3_class(spec, "tfl_ard_spec")
  # the ARD setup carries its function, the report's ARD program asks for it
  setup <- paste(ard_setup_code(spec), collapse = "\n")
  expect_match(setup, "gmean_pos", fixed = TRUE)
  expect_match(setup, "exp(mean(log(x[x > 0])))", fixed = TRUE)
  prog <- paste(ard_program_code(spec, "T1", dir = s$path), collapse = "\n")
  expect_match(prog, "gmean_pos", fixed = TRUE)
  # closed: the company's again
  .use_study(NULL)
  expect_false("gmean_pos" %in% .std_ard_statistics()$statistic)
})

test_that("a change of the study's rows changes the programs' cache keys", {
  skip_on_cran()
  skip_without_study_layer()
  local_home()
  s <- gm_study("GK")
  .use_study(s$path)
  f <- file.path(s$path, study_layout()[["spec"]], "defaults.yml")
  base <- if (file.exists(f)) readLines(f, encoding = "UTF-8", warn = FALSE)
  gm_block(s$path)
  spec1 <- .ard_spec_last(s$planner$ard)
  key1 <- .ard_spec_cache$key
  std1 <- .report_standards()
  ard1 <- .ard_standards()
  expect_match(paste(ard_setup_code(spec1), collapse = "\n"), "x > 0", fixed = TRUE)
  # the same function of another threshold
  if (length(base)) writeLines(base, f, useBytes = TRUE) else unlink(f)
  gm_block(s$path, fun = "function(x) exp(mean(log(x[x > 1])))")
  spec2 <- .ard_spec_last(s$planner$ard)
  expect_false(identical(.ard_spec_cache$key, key1))
  expect_false(identical(.report_standards(), std1))
  expect_false(identical(.ard_standards(), ard1))
  expect_match(paste(ard_setup_code(spec2), collapse = "\n"), "x > 1", fixed = TRUE)
})

test_that("rows tflspec does not take leave the company's catalog, with a warning", {
  skip_without_study_layer()
  local_home()
  s <- create_study("GB")
  gm_block(s$path, fun = "exp(mean(log(x)))")
  .use_study(s$path)
  company <- company_standards()$ard_statistics
  rownames(company) <- NULL
  expect_warning(st <- .std_ard_statistics(), "own statistics are not used")
  expect_identical(st, company)
})

test_that("the analysis screen offers the study's statistic in its own group", {
  skip_without_study_layer()
  local_home()
  s <- gm_study("GU", stats = "N|mean")
  save_study(s)
  gm_block(s$path)
  shiny::testServer(server_for("GU"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_ol_pick = "A1")
    h <- output$ard_stat_part$html
    expect_match(h, "gmean_pos", fixed = TRUE)
    expect_match(h, "\"study\"", fixed = TRUE)
  })
})
