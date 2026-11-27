test_that("the sample study is copied, registered and written", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  root <- file.path(home, "studies")
  suppressMessages(setup_tflplanner(studies_root = root))
  s <- suppressMessages(create_sample_study(run = FALSE))
  expect_equal(s$meta$study_id, "SAMPLE-01")
  expect_equal(normalizePath(dirname(s$path), "/"),
               normalizePath(root, "/"))
  expect_setequal(s$planner$outputs$output_id,
                  c("T-14-0-1", "T-14-1-1", "T-14-1-1S", "T-14-1-1C", "T-14-1-2", "T-14-1-3",
                    "T-14-1-4", "T-14-1-5", "T-14-1-6", "T-14-2-1",
                    "T-14-2-3", "T-14-2-2",
                    "T-14-3-1", "T-14-3-1C", "L-16-2-7", "F-14-2-1", "F-14-2-2", "F-14-2-3",
                    "F-14-2-4"))
  expect_true(nrow(s$planner$ard$analyses) > 0)
  # the figure made with the designer: the KM template, one layer added;
  # its design read back from its report's file (and kept by the save)
  expect_identical(report_info(s$planner, "F-14-2-3")$type, "figure")
  d <- fig_design(s$planner, "F-14-2-3")
  expect_identical(d$template, "km_simple")
  # (and the medians and hazard ratios of T-14-2-2's ARD, its ARD source)
  expect_identical(vapply(d$layers, `[[`, "", "layer"),
                   c("km_curve", "censor_mark", "hline", rep("ard_number", 3L)))
  expect_identical(vapply(d$layers[4:6], `[[`, "", "analysis_id"), c("KM", "KM", "HR"))
  # the HR is in T-14-2-2's ARD definition, not among its table's variables
  expect_true("HR" %in% ard_rows(s$planner, "analyses", "T-14-2-2")$analysis_id)
  expect_identical(.fig_ard_source(s$planner, "F-14-2-3"), list(kind = "table", id = "T-14-2-2"))
  expect_true(file.exists(file.path(s$path, "spec/outputs/F-14-2-3.yml")))
  for (f in c("data/adam/adsl.rds", "data/adam/adtte.rds",
              "programs/tfl/fig_setup.R", "programs/batch.R",
              "programs/ard/T-14-1-1.R", "programs/tfl/F-14-2-1.R")) {
    expect_true(file.exists(file.path(s$path, f)), label = f)
  }
  expect_equal(unique(study_status(s)$program_state), "current")
  # once only
  expect_error(create_sample_study(run = FALSE), "already registered")
  expect_message(setup_tflplanner(sample = TRUE), "already there")
})

test_that("the sample's T-14-1-1S is T-14-1-1's ARD as one ard_stack", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  a <- ard_rows(s$planner, "analyses", "T-14-1-1S")
  # the stack and the analyses inside it, read as the ARD tab reads them
  # (BIGN outside the stack, as T-14-1-1's: the column headers' N, tflspec#969)
  ol <- .an_outline(a)
  expect_identical(ol$analysis_id, c("BIGN", "DEMOG", "CONT", "CAT"))
  expect_identical(ol$depth, c(0L, 0L, 1L, 1L))
  expect_identical(a$method[a$analysis_id == "BIGN"], "categorical")
  st <- a[a$analysis_id == "DEMOG", ]
  expect_identical(st$method, "cards::ard_stack")
  fl <- .stack_flags_of(st$args)$flags
  # BIGN counts the subjects per group: the stack does not count them
  # again (as stack_group() writes it; review rule A13)
  expect_false(isTRUE(fl[[".by_stats"]]))
  expect_identical(nrow(stack_n_twice(a)), 0L)
  # no total N: no table of the sample prints one
  expect_false(isTRUE(fl[[".total_n"]]))
  # inside: their own variables and statistics, the stack's data and groups
  kids <- a[a$analysis_id %in% c("CONT", "CAT"), ]
  expect_true(all(is.na(kids$by) & is.na(kids$population_id)))
  # its table is T-14-1-1's
  for (sh in c("tables", "variables", "cells", "layout")) {
    x <- sheet_rows(s$planner, sh, "T-14-1-1")
    y <- sheet_rows(s$planner, sh, "T-14-1-1S")
    expect_identical(nrow(x), nrow(y), label = sh)
  }
  # one ard_stack() call in its program
  code <- readLines(file.path(s$path, "programs/ard/T-14-1-1S.R"))
  expect_identical(sum(grepl("  ard_stack(", code, fixed = TRUE)), 1L)
  # its ARD under a name that is not a function's (.ard_demog -- tflspec#867 -- not ard_stack)
  expect_true(any(startsWith(code, ".ard_demog <- ")))
  expect_false(any(grepl(".total_n = TRUE", code, fixed = TRUE)))
  # the ARD tab: the stack's own form, an analysis inside says so
  shiny::testServer(server_for("SAMPLE-01"), {
    session$setInputs(nav = "make", step = "ard", target = "T-14-1-1S")
    session$setInputs(ard_ol_pick = "DEMOG")
    h <- output$ard_stat_ui$html
    expect_match(h, "CONT", fixed = TRUE)
    expect_match(h, "CAT", fixed = TRUE)
    session$setInputs(ard_ol_pick = "CAT")
    expect_match(output$ard_stat_ui$html, "DEMOG", fixed = TRUE)
  })
})

test_that("the sample study makes its ARD and reports", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "st"),
                                    sample = TRUE))
  s <- open_study("SAMPLE-01")
  # its definition files are what its state records (#274)
  expect_identical(s$spec$status, "same")
  # and read in full, they are the same study
  r <- reload_from_spec("SAMPLE-01")
  expect_identical(r$spec$status, "same")
  expect_equal(unique(study_status(s)$status), "ok")
  expect_equal(unique(ard_status(s, "run")$state), "built")
  b <- list_batches(s)
  expect_equal(nrow(b), 1L)
  expect_equal(b$errors, 0L)
})

test_that("the sample study copies under another ID, its definition the YAML spec", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "st")))
  s <- suppressMessages(create_sample_study(study_id = "MY-1", title = "Mine",
                                            run = FALSE))
  expect_equal(s$meta$study_id, "MY-1")
  expect_equal(s$meta$title, "Mine")
  expect_true(file.exists(file.path(s$path, "MY-1.Rproj")))
  for (f in c("data.yml", "defaults.yml", "toc.yml", "outputs/T-14-1-1.yml")) {
    expect_true(file.exists(file.path(s$path, "spec", f)), label = f)
  }
  # (no workbook: the YAML spec only)
  expect_length(list.files(file.path(s$path, "spec"), "\\.(xlsx|json)$"), 0L)
  expect_true(nrow(ard_rows(s$planner, "analyses", "T-14-1-1")) > 0)
  # the sample itself can still be added alongside
  expect_equal(suppressMessages(create_sample_study(run = FALSE))$meta$study_id,
               "SAMPLE-01")
})

test_that("the sample's Study tab has keys and a setup code of its own", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  st <- s$planner$ard$study
  expect_identical(st$value[st$key == "id"], "USUBJID")
  expect_identical(st$value[st$key == "output"], "output/ard/ard.rds")
  # an ARD function of its own, named by the `source` key, written in the study
  src <- st$value[st$key == "source"]
  expect_identical(src, "programs/ard/functions/ard_cv_geomean.R")
  expect_true(file.exists(file.path(s$path, src)))
  # the setup code: the libraries; no rounding or font of its own (the
  # Study tab's, each table's plan_digits()); it parses
  expect_match(s$planner$setup, "library(cards)", fixed = TRUE)
  expect_false(grepl("rtfreporter.rounding", s$planner$setup, fixed = TRUE))
  expect_false(grepl("rtfreporter.font", s$planner$setup, fixed = TRUE))
  expect_silent(parse(text = s$planner$setup))
  # saved: it is what the study opens with
  o <- open_study(s$meta$study_id)
  expect_identical(o$planner$setup, s$planner$setup)
  expect_identical(o$planner$ard$study$value[o$planner$ard$study$key == "source"], src)
})

test_that("the sample's SPEC fixes of the 2026-11-25 reviews reach its programs", {
  skip_on_cran()
  local_home()
  s <- suppressMessages(create_sample_study(run = FALSE))
  p <- s$planner
  has <- function(code, x) any(grepl(x, code, fixed = TRUE))
  ard_prog <- function(id) readLines(file.path(s$path, "programs", "ard", paste0(id, ".R")),
                                     warn = FALSE)
  # L-16-2-7: the records of the safety set's subjects (its title says so),
  # joined to them as the tables' records are; AESEV is upper case in the data
  expect_identical(p$spec$outputs[["L-16-2-7"]]$data$population, "SAF")
  l <- program_code(p, "L-16-2-7")
  expect_true(has(l, "inner_join("))
  expect_true(has(l, "SAFFL == \"Y\""))
  expect_true(has(l, "AESEV == \"SEVERE\""))
  # T-14-1-5 / T-14-1-6: every Asian subject has a sub-category (Other when
  # the country is not China, Japan or Korea), and the code list has it
  for (id in c("T-14-1-5", "T-14-1-6")) {
    a <- ard_prog(id)
    expect_true(has(a, "COUNTRY == \"CHN\" ~ \"CHINESE\""), label = id)
    expect_true(has(a, "RACE == \"ASIAN\" ~ \"OTHER\""), label = id)
    expect_true(has(a, "OTHER = \"Other\""), label = id)
  }
  # F-14-2-4: a subgroup's failing Cox model does not stop the ARD; a
  # subgroup with no estimate prints NE (not dropped), by a named limit
  expect_identical(sum(grepl("tryCatch(", ard_prog("F-14-2-4"), fixed = TRUE)), 2L)
  f <- program_code(p, "F-14-2-4")
  expect_true(has(f, "ne_upper <- 1000"))
  expect_false(has(f, "conf.high < 1000"))
  expect_false(has(f, "filter(!is.na(conf.low))"))
  # T-14-1-1C: the header prints its label and title; no second title
  expect_false(has(program_code(p, "T-14-1-1C"), "rtf_titles("))
  # T-14-2-3: the p-value as the study formats it (fmt_pvalue: the ARD's stat_fmt)
  t3 <- program_code(p, "T-14-2-3")
  expect_true(has(t3, "{p.value:stat_fmt}"))
  expect_false(has(t3, "{p.value:.3f}"))
})

test_that("the sample's rounding is forced in defaults.yml force:, not in study.yml (canon 1.33)", {
  skip_on_cran()
  root <- system.file("sample", "SAMPLE-01", package = "tflplanner")
  st <- tflspec::tfl_read_study_spec(root)
  expect_length(st$force$content, 1L)
  it <- st$force$content[[1L]]
  expect_identical(it$fn, "plan_digits")
  expect_identical(it$args, list(rounding = "sas"))
  expect_identical(it$from, "force")
  # study.yml says no rounding; the study's content has no rounding card
  expect_null(st$study$settings$rounding)
  expect_false(any(grepl("rounding", readLines(file.path(root, "study.yml")), fixed = TRUE)))
  expect_false(any(vapply(st$defaults$content, function(x) !is.null(x$args$rounding), NA)))
  # the view: the study sheet's key; the setup: the forced rounding
  p <- .planner_from_yaml(root)
  expect_identical(unname(p$study[["rounding"]]), "sas")
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", tflspec::tfl_study_setup_code(st), fixed = TRUE)))
})
