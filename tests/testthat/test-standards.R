home_for_test <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

test_that("the draft writes and reads back as it is", {
  skip_on_cran()
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
  skip_on_cran()
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
  expect_true(any(grepl("ard_stack_hierarchical(", code,
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
  skip_on_cran()
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
  expect_true(any(grepl("filter(output_id == report_id)", code, fixed = TRUE)))
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
  skip_on_cran()
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

test_that("a study's figures are made and drawn with its figure style over the company's", {
  skip_on_cran()
  home_for_test()
  # the report programs and fig_setup.R get the company's figure tables
  rs <- .report_standards()
  expect_true(all(c("figure_settings", "figure_colors", "figure_markers") %in% names(rs)))
  expect_identical(rs$figure_settings, company_standards()$figure_settings)
  # a study's copy that says 30 for axis_title_width: the style the
  # preview sets and fig_setup.R writes say 30, the other keys the company's
  st <- structure(list(figure_style = list(settings = list(
    list(key = "axis_title_width", value = "30")))), class = "tfl_study_spec")
  fs <- tflspec::tfl_fig_style(st, .std_fig_standards())
  w <- fs$settings$value[fs$settings$key == "axis_title_width" & is.na(fs$settings$type)]
  expect_identical(w, "30")
  co <- company_standards()$figure_settings
  expect_setequal(paste(fs$settings$type, fs$settings$key), paste(co$type, co$key))
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

# ---- safe writes of the company workbook

good_std <- function(env = parent.frame()) {
  f <- file.path(withr_tempdir(env), "std.xlsx")
  standards_template(f)
  f
}

test_that("an install writes the file and keeps the one it replaces as .prev", {
  skip_on_cran()
  home <- home_for_test()
  f1 <- good_std()
  suppressMessages(setup_tflplanner(standards = f1))
  f <- .standards_file(home)
  expect_true(file.exists(f))
  expect_false(file.exists(paste0(f, ".prev")))
  f2 <- good_std()
  s <- .builtin_standards()
  s$about$value[s$about$key == "version"] <- "9.9"
  writexl::write_xlsx(s, f2)
  suppressMessages(setup_tflplanner(standards = f2))
  expect_identical(.standards_in_use(home)$text,
                   sprintf("%s 9.9 (%s)", s$about$value[s$about$key == "name"], f))
  expect_true(file.exists(paste0(f, ".prev")))
  expect_identical(length(list.files(dirname(f), all.files = TRUE, no.. = TRUE)), 2L)
  # swapped back
  restore_previous_standards(home)
  expect_false(identical(company_standards(home)$about$value[2], "9.9"))
  expect_true(file.exists(paste0(f, ".prev")))
})

test_that("a failed install leaves the old file as it was", {
  skip_on_cran()
  home <- home_for_test()
  suppressMessages(setup_tflplanner(standards = good_std()))
  f <- .standards_file(home)
  before <- unname(tools::md5sum(f))
  bad <- file.path(withr_tempdir(), "bad.xlsx")
  writexl::write_xlsx(list(statistics = data.frame(key = "n")), bad)
  expect_error(setup_tflplanner(standards = bad), "lacks column")
  expect_error(.install_standards_file(bad, f), "do not read")
  expect_error(.install_standards_file(file.path(tempdir(), "none.xlsx"), f),
               "not written")
  expect_identical(unname(tools::md5sum(f)), before)
  expect_false(file.exists(paste0(f, ".prev")))
})

test_that("back to the built-in moves the file to .prev", {
  skip_on_cran()
  home <- home_for_test()
  suppressMessages(setup_tflplanner(standards = good_std()))
  f <- .standards_file(home)
  suppressMessages(setup_tflplanner(standards = "builtin"))
  expect_false(file.exists(f))
  expect_true(file.exists(paste0(f, ".prev")))
  restore_previous_standards(home)
  expect_true(file.exists(f))
  expect_error(restore_previous_standards(home = tempfile()), "no previous")
})

test_that("remember_toc_map keeps the file it replaces as .prev", {
  skip_on_cran()
  home <- home_for_test()
  remember_toc_map(list(note = "Remark"), home = home)
  f <- .standards_file(home)
  expect_false(file.exists(paste0(f, ".prev")))
  remember_toc_map(list(note = "Memo"), home = home)
  expect_true(file.exists(paste0(f, ".prev")))
  expect_identical(toc_map_for("Memo", company_standards(home)$toc_map)$note, "Memo")
})

test_that("the settings block says when the workbook does not read", {
  skip_on_cran()
  home <- home_for_test()
  expect_null(.standards_in_use(home)$text)
  f <- .standards_file(home)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines("not a workbook", f)
  x <- .standards_in_use(home)
  expect_null(x$text)
  expect_true(nzchar(x$error))
})

test_that("an unknown sheet name is warned about, not ignored", {
  skip_on_cran()
  home_for_test()
  s <- .builtin_standards()
  names(s)[names(s) == "statistics"] <- "statistcs"
  f <- file.path(withr_tempdir(), "typo.xlsx")
  writexl::write_xlsx(c(list(`_README` = data.frame(a = 1)), s), f)
  expect_warning(read_standards(f), "statistcs.*Known sheets")
  g <- good_std()
  expect_no_warning(read_standards(g))
})

test_that("remember_toc_map refuses to rewrite a workbook with content it would drop", {
  skip_on_cran()
  home <- home_for_test()
  suppressMessages(setup_tflplanner(standards = good_std()))
  f <- .standards_file(home)
  s <- suppressWarnings(read_standards(f))
  # an extra sheet
  writexl::write_xlsx(c(s, list(notes = data.frame(a = "keep me"))), f)
  before <- unname(tools::md5sum(f))
  expect_error(remember_toc_map(list(note = "Memo"), home = home), "not remembered")
  expect_identical(unname(tools::md5sum(f)), before)
  expect_false(file.exists(paste0(f, ".prev")))
  # an extra column in a known sheet
  s$about$comment <- "mine"
  writexl::write_xlsx(s, f)
  before <- unname(tools::md5sum(f))
  expect_error(remember_toc_map(list(note = "Memo"), home = home), "not remembered")
  expect_identical(unname(tools::md5sum(f)), before)
  expect_false(file.exists(paste0(f, ".prev")))
})

test_that("remember_toc_map still works on a plain installed workbook", {
  skip_on_cran()
  home <- home_for_test()
  suppressMessages(setup_tflplanner(standards = good_std()))
  f <- .standards_file(home)
  expect_length(.standards_would_lose(f), 0L)
  expect_no_error(remember_toc_map(list(note = "Memo"), home = home))
  expect_true(file.exists(paste0(f, ".prev")))
  expect_identical(toc_map_for("Memo", company_standards(home)$toc_map)$note, "Memo")
})

test_that("a broken current file does not replace the valid .prev", {
  skip_on_cran()
  home <- home_for_test()
  suppressMessages(setup_tflplanner(standards = good_std()))
  f <- .standards_file(home)
  suppressMessages(setup_tflplanner(standards = good_std()))
  prev <- paste0(f, ".prev")
  expect_true(file.exists(prev))
  prev_md5 <- unname(tools::md5sum(prev))
  broken <- file.path(withr_tempdir(), "bad.xlsx")
  writexl::write_xlsx(list(statistics = data.frame(key = "n")), broken)
  # back to the built-in over a broken file
  file.copy(broken, f, overwrite = TRUE)
  suppressWarnings(suppressMessages(setup_tflplanner(standards = "builtin")))
  expect_false(file.exists(f))
  expect_identical(unname(tools::md5sum(prev)), prev_md5)
  expect_true(file.exists(paste0(f, ".broken")))
  expect_true(.standards_reads(prev))
  unlink(paste0(f, ".broken"))
  # an install over a broken file
  file.copy(broken, f, overwrite = TRUE)
  expect_warning(suppressMessages(setup_tflplanner(standards = good_std())), "did not read")
  expect_true(file.exists(f))
  expect_identical(unname(tools::md5sum(prev)), prev_md5)
  expect_true(file.exists(paste0(f, ".broken")))
})
