# A figure's ARD (#293): where it comes from (the report row's
# ard_source: own, table:<id>, import:<file>), the program's lines that put
# it in `ard`, the checks of the design's ARD pieces, and the sample's
# F-14-2-3, which prints T-14-2-2's medians.

fig_ard_study <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  withr::local_options(tflplanner.home = home, .local_envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE))
}

test_that("a figure's ARD source: own, a table's, none; nothing else", {
  p <- new_planner()
  p <- add_output(p, "T-1", type = "table")
  p <- add_output(p, "F-1", type = "figure")
  p <- add_output(p, "L-1", type = "listing")
  expect_identical(.fig_ard_source(p, "F-1")$kind, "none")
  p <- set_fig_ard_source(p, "F-1", "table:T-1")
  expect_identical(.fig_ard_source(p, "F-1"), list(kind = "table", id = "T-1"))
  expect_identical(.figs_reading_table(p, "T-1"), "F-1")
  p <- set_fig_ard_source(p, "F-1", "own")
  expect_identical(.fig_ard_source(p, "F-1"), list(kind = "own", id = "F-1"))
  expect_error(set_fig_ard_source(p, "F-1", "table:L-1"), "not a table")
  expect_error(set_fig_ard_source(p, "F-1", "T-1"), "own")
  p <- set_fig_ard_source(p, "F-1", NULL)
  expect_identical(.fig_ard_source(p, "F-1")$kind, "none")
  # a table renamed: the figure reads it under its new id
  p <- set_fig_ard_source(p, "F-1", "table:T-1")
  p <- rename_output(p, "T-1", "T-2")
  expect_identical(.fig_ard_source(p, "F-1"), list(kind = "table", id = "T-2"))
  expect_identical(.ard_ids("KM, HR | X"), c("KM", "HR", "X"))
})

test_that("the sample's F-14-2-3 reads T-14-2-2's ARD in its data section", {
  s <- fig_ard_study()
  p <- s$planner
  expect_identical(.fig_ard_source(p, "F-14-2-3"), list(kind = "table", id = "T-14-2-2"))
  d <- fig_design(p, "F-14-2-3")
  expect_true(.fig_reads_ard(d, "F-14-2-3"))
  code <- program_code(p, "F-14-2-3")
  i_data <- grep("^# ---- data ", code)
  i_ard <- which(code == "ard <- readRDS(file.path(path_ard, \"ard.rds\")) |>")
  i_adam <- grep("^adtte <- readRDS", code)
  expect_length(i_ard, 1L)
  expect_true(i_data < i_ard && i_ard < i_adam)
  expect_identical(code[i_ard + 1L], "  filter(output_id == \"T-14-2-2\")")
  expect_match(code[i_ard + 2L], "run programs/ard/T-14-2-2.R first", fixed = TRUE)
  # the medians: one annotate a arm, its number the ARD's; the hazard
  # ratios: the estimate and its CI on one line (#311)
  expect_identical(sum(grepl("ard_value(ard, \"KM\", \"prob\", \"estimate\", TRT01A = ",
                             code, fixed = TRUE)), 3L)
  expect_identical(sum(grepl("ard_value(ard, \"HR\", \"TRT01A\", \"conf.high\", level = ",
                             code, fixed = TRUE)), 2L)
  expect_false(inherits(tryCatch(parse(text = code), error = function(e) e), "error"))
  # no source: the program stops, saying where to choose it
  p0 <- set_fig_ard_source(p, "F-14-2-3", NULL)
  code0 <- program_code(p0, "F-14-2-3")
  expect_true(any(grepl("stop(\"F-14-2-3 reads an ARD: choose it in the figure's step 2 (ARD).\")",
                        code0, fixed = TRUE)))
  expect_false(any(grepl("^ard <- ", code0)))
  # its own: its rows
  po <- set_fig_ard_source(p, "F-14-2-3", "own")
  expect_true("  filter(output_id == \"F-14-2-3\")" %in% program_code(po, "F-14-2-3"))
  # a figure that reads no ARD and has none: no lines at all
  expect_null(.fig_ard_lines(p, "F-14-2-1"))
})

test_that("the checks: no source, not made yet, then each piece against the rows", {
  s <- fig_ard_study()
  s0 <- s
  s0$planner <- set_fig_ard_source(s$planner, "F-14-2-3", NULL)
  pr <- .fig_ard_problems(s0, "F-14-2-3")
  expect_identical(pr$severity, "error")
  expect_match(pr$problem, "has none: choose it in step 2")
  # T-14-2-2's ARD not made yet: a warning, not an error
  pr <- .fig_ard_problems(s, "F-14-2-3")
  expect_identical(pr$severity, "warning")
  expect_match(pr$problem, "not made yet")
  # made: the design's pieces are in it
  skip_if_not_installed("cardx")
  u <- suppressMessages(update_study_ard(s, "T-14-2-2"))
  expect_true(u$ok)
  expect_identical(nrow(.fig_ard_problems(s, "F-14-2-3")), 0L)
  d <- fig_design(s$planner, "F-14-2-3")
  k <- which(vapply(d$layers, function(l) identical(l$layer, "ard_number"), NA))[1L]
  d$layers[[k]]$group <- "TRT01A = Nobody"
  s$planner <- set_fig_design(s$planner, "F-14-2-3", d)
  pr <- .fig_ard_problems(s, "F-14-2-3")
  expect_identical(pr$severity, "error")
  expect_match(pr$problem, "no TRT01A = Nobody")
  # the study's spec check names them too
  ck <- .check_fig_ards(s, "F-14-2-3")
  expect_match(ck$message, "F-14-2-3: no TRT01A = Nobody")
})

test_that("the preview prints the table's medians", {
  skip_if_not_installed("cardx")
  skip_if_not_installed("ggsurvfit")
  s <- fig_ard_study()
  suppressMessages(update_study_ard(s, "T-14-2-2"))
  r <- preview_figure(s, "F-14-2-3", max_px = 600)
  expect_null(r$error)
  expect_true(file.exists(r$png))
  expect_identical(nrow(r$problems), 0L)
})

test_that("the designer: an ARD piece's code is its whole term, its summary its address", {
  d <- tflspec::tfl_read_fig_design(system.file("sample/SAMPLE-01/spec/figures/F-14-2-3.yml",
                                                package = "tflplanner"))
  make <- function(d, codelists = TRUE) .fig_design_script(d, "F-14-2-3")
  code <- make(d)
  k <- which(vapply(d$layers, function(l) identical(l$layer, "ard_number"), NA))
  for (i in k) {
    x <- strsplit(.piece_code(code, d, list(sec = "layers", i = i), make), "\n", fixed = TRUE)[[1L]]
    expect_identical(x[[1L]], "  annotate(")
    expect_match(x[[length(x)]], "^  [)]")
    # (a median's label: its text, then the number)
    if (identical(d$layers[[i]]$analysis_id, "KM")) {
      expect_match(x[[3L]], d$layers[[i]]$label |> sub(pattern = "[{]value[}]", replacement = "") |>
                     sub(pattern = ": $", replacement = ""), fixed = TRUE)
    }
  }
  expect_identical(.pd_summary(d$layers[[k[1L]]]), "KM prob estimate TRT01A = Placebo")
})

test_that("a table deleted, or an analysis dropped from it: the figure's checks say so", {
  s <- fig_ard_study()
  p <- s$planner
  # deleted: report_info() of a missing id is the default row (a table), so
  # the id itself is looked for
  expect_false(.is_table(p, "T-99"))
  expect_error(set_fig_ard_source(p, "F-14-2-3", "table:T-99"), "not a table")
  s$planner <- remove_output(p, "T-14-2-2")
  pr <- .fig_ard_problems(s, "F-14-2-3")
  expect_identical(pr$severity, "error")
  expect_match(pr$problem, "T-14-2-2 is not a table of the study [(]deleted")
  # the figures reading a table, for the delete dialog
  expect_identical(.figs_reading_table(p, "T-14-2-2"), "F-14-2-3")
  # its KM analysis dropped from the definition: an error even while the
  # ARD made before still has the rows
  skip_if_not_installed("cardx")
  s$planner <- p
  suppressMessages(update_study_ard(s, "T-14-2-2"))
  an <- p$ard$analyses
  p2 <- p
  p2$ard$analyses <- an[!(an$output_id %in% "T-14-2-2" & an$analysis_id == "KM"), , drop = FALSE]
  s$planner <- p2
  pr <- .fig_ard_problems(s, "F-14-2-3")
  expect_identical(unique(pr$severity), "error")
  expect_match(pr$problem[1], "T-14-2-2 has no analysis KM")
})

test_that("the ARS: F-14-2-3 is an output naming T-14-2-2's analyses it prints (#293 phase 5)", {
  s <- fig_ard_study()
  r <- .fig_ars_references(s$planner)
  expect_identical(unique(r$output_id), "F-14-2-3")
  expect_identical(unique(r$source), "T-14-2-2")
  expect_setequal(r$analysis_id, c("KM", "HR"))
  d <- withr::local_tempdir()
  f <- export_ars(s, d)
  ars <- tflspec::tfl_read_ars_json(f[["json"]])
  expect_true("F-14-2-3" %in% vapply(ars$outputs, `[[`, "", "id"))
  it <- Filter(function(z) identical(z$outputId, "F-14-2-3"),
               ars$mainListOfContents$contentsList$listItems)[[1L]]
  got <- vapply(it$sublist$listItems, `[[`, "", "analysisId")
  # the table's own analyses, the same ids its list item has
  tab <- Filter(function(z) identical(z$outputId, "T-14-2-2"),
                ars$mainListOfContents$contentsList$listItems)[[1L]]
  expect_true(length(got) > 0L)
  expect_true(all(got %in% vapply(tab$sublist$listItems, `[[`, "", "analysisId")))
  ck <- utils::read.csv(f[["check"]], stringsAsFactors = FALSE)
  expect_false("F-14-2-3" %in% ck$where[ck$kind == "not in ARS"])
})

test_that("the study review lists a figure's ARD problems as F04-F08, each with its place", {
  s <- fig_ard_study()
  rv <- function(s, lang = "en") {
    r <- study_review(s, "F-14-2-3", data = "none", lang = lang)
    r[r$rule %in% c("F04", "F05", "F06", "F07", "F08"), , drop = FALSE]
  }
  # T-14-2-2's ARD not made yet: F07, to check, on the report row
  r <- rv(s)
  expect_identical(r$rule, "F07")
  expect_identical(r$level, "check")
  expect_identical(c(r$sheet, r$field), c("report", "ard_source"))
  expect_identical(r$message, "the ARD of T-14-2-2 is not made yet: make it first (its step 2)")
  expect_identical(r$template, "the ARD of %s is not made yet: make it first (its step 2)")
  expect_identical(.review_target(r, s$planner)$go, "ard")
  # in Japanese: the sentence translated, the value put in
  expect_identical(rv(s, "ja")$message,
                   "T-14-2-2 \u306e ARD \u306f\u307e\u3060\u4f5c\u3089\u308c\u3066\u3044\u307e\u305b\u3093\u3002\u5148\u306b\u4f5c\u3063\u3066\u304f\u3060\u3055\u3044\uff08\u305d\u306e\u30b9\u30c6\u30c3\u30d7 2\uff09")
  # no source: F05, by hand
  s0 <- s
  s0$planner <- set_fig_ard_source(s$planner, "F-14-2-3", NULL)
  r <- rv(s0)
  expect_identical(r$rule, "F05")
  expect_identical(r$level, "hand")
  # a table that is gone: F04
  s4 <- s
  rep <- s4$planner$sheets$report
  rep$ard_source[rep$output_id %in% "F-14-2-3"] <- "table:T-GONE"
  s4$planner$sheets$report <- rep
  r <- rv(s4)
  expect_identical(r$rule, "F04")
  expect_identical(r$args[[1L]], "T-GONE")
  expect_match(r$message, "(deleted, or renamed by hand)", fixed = TRUE)
  # made, then a piece the ARD cannot answer: F08, in the designer
  skip_if_not_installed("cardx")
  expect_true(suppressMessages(update_study_ard(s, "T-14-2-2"))$ok)
  expect_identical(nrow(rv(s)), 0L)
  d <- fig_design(s$planner, "F-14-2-3")
  k <- which(vapply(d$layers, function(l) identical(l$layer, "ard_number"), NA))[1L]
  d$layers[[k]]$group <- "TRT01A = Nobody"
  s$planner <- set_fig_design(s$planner, "F-14-2-3", d)
  r <- rv(s)
  expect_identical(r$rule, "F08")
  expect_identical(r$level, "error")
  expect_identical(r$sheet, "design")
  expect_identical(r$row, sprintf("layers[%d] ard_number", k))
  expect_identical(.review_target(r, s$planner)$go, "designer")
})

test_that("set_fig_own_analyses() writes a design's analyses to the figure's ARD definition (#293 P6)", {
  skip_if(!"tfl_fig_forest_analyses" %in% getNamespaceExports("tflspec"), "tflspec has no tfl_fig_forest_analyses()")
  p <- add_output(new_planner(), "F-FOR", type = "figure")
  # another report's analysis data: left alone
  p$ard$analysis_data <- .normalize_ard_sheet(data.frame(
    output_id = "T1", data_id = "adsl_saf", from = "ADSL", population_id = "SAF"), "analysis_data")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(dataset = c("ADSL", "ADTTE"),
    path = c("data/adam/adsl.rds", "data/adam/adtte.rds")), "datasets")
  p$ard$populations <- .normalize_ard_sheet(data.frame(population_id = "SAF", dataset = "ADSL",
    where = "SAFFL == \"Y\""), "populations")
  an <- tflspec::tfl_fig_forest_analyses("ADTTE", "TTDE", "SAFFL", "TRT01A", c("SEX", "AGEGR1"))
  p <- set_fig_own_analyses(p, "F-FOR", an, population_id = "SAF")
  ad0 <- p$ard$analysis_data
  expect_identical(ad0$population_id[ad0$output_id == "F-FOR"], "SAF")
  expect_identical(attr(p, "written"), c("HR", "HR_SEX", "HR_AGEGR1"))
  expect_identical(ard_rows(p, "analyses", "F-FOR")$analysis_id, c("HR", "HR_SEX", "HR_AGEGR1"))
  ad <- p$ard$analysis_data
  expect_identical(ad$data_id[ad$output_id == "F-FOR"], "adtte_ttde")
  expect_identical(ad$data_id[ad$output_id == "T1"], "adsl_saf")
  expect_identical(nrow(ad), 2L)
  expect_identical(sheet_rows(p, "report", "F-FOR")$ard_source, "own")
  # the definition holds, and writes the figure's ARD program
  expect_silent(.ard_spec(p$ard))
  # applied again (another subgroup): the same ids replaced, the rest kept
  an2 <- tflspec::tfl_fig_forest_analyses("ADTTE", "TTDE", "SAF", "TRT01A", "RACE")
  p2 <- set_fig_own_analyses(p, "F-FOR", an2)
  expect_identical(ard_rows(p2, "analyses", "F-FOR")$analysis_id, c("HR_SEX", "HR_AGEGR1", "HR", "HR_RACE"))
  expect_match(ard_rows(p2, "analyses", "F-FOR")$code[3], "data = data", fixed = TRUE)
  expect_error(set_fig_own_analyses(p, "F-FOR", list()), "two data frames")
})

test_that("a template's population flag is the study's analysis set", {
  p <- new_planner()
  p$ard$populations <- .normalize_ard_sheet(data.frame(
    population_id = c("SAF", "ITT"), dataset = "ADSL",
    where = c("SAFFL == \"Y\"", "ITTFL == \"Y\"")), "populations")
  expect_identical(.population_of_flag(p, "SAFFL"), "SAF")
  expect_identical(.population_of_flag(p, "ITT"), "ITT")
  expect_identical(.population_of_flag(p, "FASFL"), NA_character_)
  expect_identical(.population_of_flag(new_planner(), "SAFFL"), NA_character_)
})
