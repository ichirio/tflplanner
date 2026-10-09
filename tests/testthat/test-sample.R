test_that("the sample study is copied, registered and written", {
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
                  c("T-14-0-1", "T-14-1-1", "T-14-1-1S", "T-14-1-2", "T-14-1-3",
                    "T-14-1-4", "T-14-2-1",
                    "T-14-2-3", "T-14-2-2",
                    "T-14-3-1", "L-16-2-7", "F-14-2-1", "F-14-2-2", "F-14-2-3"))
  expect_true(nrow(s$planner$ard$analyses) > 0)
  # the figure made with the designer: the KM template, one layer added;
  # its design read back from spec/figures/ (and kept by the save)
  expect_identical(report_info(s$planner, "F-14-2-3")$type, "figure")
  d <- fig_design(s$planner, "F-14-2-3")
  expect_identical(d$template, "km_simple")
  expect_identical(vapply(d$layers, `[[`, "", "layer"), c("km_curve", "censor_mark", "hline"))
  expect_true(file.exists(file.path(s$path, "spec/figures/F-14-2-3.yml")))
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
  ol <- .an_outline(a)
  expect_identical(ol$analysis_id, c("STACK", "CONT", "CAT"))
  expect_identical(ol$depth, c(0L, 1L, 1L))
  st <- a[a$analysis_id == "STACK", ]
  expect_identical(st$method, "cards::ard_stack")
  fl <- .stack_flags_of(st$args)$flags
  expect_true(isTRUE(fl[[".by_stats"]]))
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
  expect_false(any(grepl(".total_n = TRUE", code, fixed = TRUE)))
  # the ARD tab: the stack's own form, an analysis inside says so
  shiny::testServer(server_for("SAMPLE-01"), {
    session$setInputs(nav = "make", step = "ard", target = "T-14-1-1S")
    session$setInputs(ard_ol_pick = "STACK")
    h <- output$ard_stat_ui$html
    expect_match(h, "CONT", fixed = TRUE)
    expect_match(h, "CAT", fixed = TRUE)
    session$setInputs(ard_ol_pick = "CAT")
    expect_match(output$ard_stat_ui$html, "STACK", fixed = TRUE)
  })
})

test_that("the sample study makes its ARD and reports", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggplot2")
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
  expect_equal(unique(ard_status(s)$state), "built")
  b <- list_batches(s)
  expect_equal(nrow(b), 1L)
  expect_equal(b$errors, 0L)
})

test_that("the sample study copies under another ID, with every definition as Excel", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "st")))
  s <- suppressMessages(create_sample_study(study_id = "MY-1", title = "Mine",
                                            run = FALSE))
  expect_equal(s$meta$study_id, "MY-1")
  expect_equal(s$meta$title, "Mine")
  expect_true(file.exists(file.path(s$path, "MY-1.Rproj")))
  for (f in c("table_spec.xlsx", "report_spec.xlsx", "listing_figure_spec.xlsx",
              "ard_spec.xlsx")) {
    expect_true(file.exists(file.path(s$path, "spec", f)), label = f)
  }
  expect_true(nrow(ard_rows(s$planner, "analyses", "T-14-1-1")) > 0)
  # the sample itself can still be added alongside
  expect_equal(suppressMessages(create_sample_study(run = FALSE))$meta$study_id,
               "SAMPLE-01")
})
