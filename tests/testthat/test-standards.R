home_for_test <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

test_that("the draft writes and reads back as it is", {
  home_for_test()
  f <- file.path(withr_tempdir(), "std.xlsx")
  standards_template(f)
  expect_true("_README" %in% readxl::excel_sheets(f))
  s <- read_standards(f)
  b <- .builtin_standards()
  expect_setequal(names(s), names(b))
  norm <- function(d) {
    d[] <- lapply(d, function(v) ifelse(is.na(v) | !nzchar(v), NA, v))
    rownames(d) <- NULL
    d
  }
  for (n in names(b)) expect_equal(norm(s[[n]]), norm(b[[n]]), label = n)
  # without an installed workbook the draft applies
  expect_identical(company_standards(), b)
})

test_that("a company's workbook changes what the app offers", {
  home <- home_for_test()
  f <- file.path(withr_tempdir(), "acme.xlsx")
  s <- .builtin_standards()
  s$cell_presets <- rbind(s$cell_presets, data.frame(
    preset = "ACME: n / Mean (SD)", variable = "continuous",
    row = c("n", "Mean (SD)"), template = c("{N}", "{mean} ({sd})"),
    digits = c("0", "1,2")))
  s$default_digits$digits[s$default_digits$statistic == "mean"] <- "2"
  s$ard_methods <- rbind(s$ard_methods, data.frame(
    method = "ae_socpt", label = "AE by SOC / PT",
    call = "cards::ard_stack_hierarchical",
    kind = "categorical",
    defaults = "denominator = population, id = <id>, over_variables = TRUE",
    statistics = NA, formats = NA, note = "AE by SOC / PT, with the Any row"))
  s$settings$value[s$settings$key == "subject_id"] <- "SUBJID"
  s$default_header$left[1] <- "ACME Pharma"
  s$choices <- rbind(s$choices, data.frame(sheet = "page",
                                           column = "orientation",
                                           value = "portrait-wide"))
  writexl::write_xlsx(s, f)
  suppressMessages(setup_tflplanner(standards = f))
  expect_true(file.exists(file.path(home, "standards",
                                    "company_standards.xlsx")))

  expect_true("ACME: n / Mean (SD)" %in% names(cell_presets()))
  expect_equal(.std_digits()[["mean"]], 2L)
  expect_true("ae_socpt" %in% .std_ard_methods()$method)
  expect_true("portrait-wide" %in% .std_choices("page")$orientation)

  st <- create_study("NEW-1")
  expect_equal(st$planner$sheets$header$left[1], "ACME Pharma")
  # each statistic's decimals: the study's, from the standards
  d <- sheet_rows(st$planner, "digits", NA)
  expect_equal(d$digits[d$statistic == "mean"], "2")
  # the protocol is a token, its value set once (the study tab)
  expect_equal(st$planner$sheets$header$left[2], "PROTOCOL: {STUDY_ID}")
  expect_equal(study_token(st$planner, "STUDY_ID"), "NEW-1")
  expect_equal(st$planner$ard$study$value[st$planner$ard$study$key == "id"],
               "SUBJID")
  expect_true("SAF" %in% st$planner$ard$populations$population_id)
  expect_true(all(c("ADaM", "SDTM") %in% st$planner$ard$datasets$level))

  # the company keyword becomes its call with its defaults
  st$planner$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = "AE", method = "ae_socpt",
    dataset = "ADAE", population_id = "SAF", by = "TRTA",
    variables = "AEBODSYS | AEDECOD"), "analyses")
  code <- .ard_spec_code(.ard_spec(st$planner$ard))
  expect_true(any(grepl("cards::ard_stack_hierarchical(", code,
                        fixed = TRUE)))
  expect_true(any(grepl("id = SUBJID", code, fixed = TRUE)))
  expect_true(any(grepl("over_variables = TRUE", code, fixed = TRUE)))

  # and back to the draft
  suppressMessages(setup_tflplanner(standards = "builtin"))
  expect_false("ae_socpt" %in% .std_ard_methods()$method)
  expect_equal(.std_digits()[["mean"]], 1L)
})

test_that("a workbook that does not read is refused", {
  home_for_test()
  f <- file.path(withr_tempdir(), "bad.xlsx")
  writexl::write_xlsx(list(statistics = data.frame(key = "n")), f)
  expect_error(setup_tflplanner(standards = f), "lacks column")
})

test_that("a table without data code starts from the company's template", {
  home <- home_for_test()
  f <- file.path(withr_tempdir(), "acme.xlsx")
  s <- .builtin_standards()
  s$code_templates$code[s$code_templates$name == "table_process"] <-
    "data <- tfl_ard_normalize(ard)\ndata <- acme_rework(data)"
  s$code_templates$code[s$code_templates$name == "setup"] <-
    "library(acme)  # {STUDY_ID}"
  writexl::write_xlsx(s, f)
  suppressMessages(setup_tflplanner(standards = f))
  p <- add_output(new_planner(), "T1")
  code <- data_lines(p, "T1")
  expect_true(any(grepl('ard$output_id == "T1"', code, fixed = TRUE)))
  expect_true(any(grepl("acme_rework(data)", code, fixed = TRUE)))
  # a template saved with the former name is read with the new one
  expect_true(any(grepl("data <- normalize_ard(ard)", code, fixed = TRUE)))
  expect_false(any(grepl("tfl_ard_normalize", code, fixed = TRUE)))
  st <- create_study("NEW-2")
  expect_equal(st$planner$setup, "library(acme)  # NEW-2")
  suppressMessages(setup_tflplanner(standards = "builtin"))
  expect_false(any(grepl("acme", data_lines(p, "T1"))))
})

test_that("the figure style is part of the company standards", {
  home_for_test()
  s <- company_standards()
  for (sh in c("figure_settings", "figure_colors", "figure_markers")) {
    expect_true(nrow(s[[sh]]) > 0, label = sh)
  }
  code <- tflspec::tfl_fig_setup_code(.std_fig_style())
  expect_true(any(grepl('"twodash"', code, fixed = TRUE)))

  # a company's colour reaches the helper script
  f <- file.path(withr_tempdir(), "acme.xlsx")
  b <- .builtin_standards()
  b$figure_colors$colour[b$figure_colors$palette == "response" &
                           b$figure_colors$value %in% "CR"] <- "#00AA00"
  writexl::write_xlsx(b, f)
  suppressMessages(setup_tflplanner(standards = f))
  code <- tflspec::tfl_fig_setup_code(.std_fig_style())
  expect_true(any(grepl('"CR" = "#00AA00"', code, fixed = TRUE)))
  suppressMessages(setup_tflplanner(standards = "builtin"))
})

test_that("a company's methods sheet written before labels still reads", {
  home_for_test()
  f <- file.path(withr_tempdir(), "old.xlsx")
  s <- .builtin_standards()
  s$ard_methods$label <- NULL
  writexl::write_xlsx(s, f)
  m <- read_standards(f)$ard_methods
  expect_true("label" %in% names(m))
  expect_true(all(is.na(m$label)))
})
