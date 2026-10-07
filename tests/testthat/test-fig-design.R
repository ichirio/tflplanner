test_that("a figure's design is kept, saved as YAML, and makes its plot", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  expect_null(fig_design(s$planner, "F-14-2-1"))
  # the sample's F-14-2-1 is user code: a figure again, to be designed
  s$planner <- .set_report_type(s$planner, "F-14-2-1", "figure")
  d <- tflspec::tfl_fig_template("mean_ci", data = "ADVS", param = "SYSBP",
                                  value = "CHG", title = "SBP: mean change")
  d$plot$width <- 7L
  s$planner <- set_fig_design(s$planner, "F-14-2-1", d)

  # the program's plot is the design's code, reading what it needs
  code <- data_lines(s$planner, "F-14-2-1")
  expect_true(any(grepl("advs <- ", code, fixed = TRUE)))
  expect_true(any(grepl("adsl <- ", code, fixed = TRUE)))
  lines <- unlist(strsplit(code, "\n", fixed = TRUE))
  # the design's figure is the program's `plot` itself, its palette the
  # study's figure setup's
  expect_true(any(grepl("^plot <- p$", lines)))
  expect_false(any(grepl("^plot <- fig$|^fig <- p$", lines)))
  expect_true(any(grepl("tfl_colours(", lines, fixed = TRUE)))
  expect_false(any(grepl("ggsave(", code, fixed = TRUE)))

  # saved: the YAML, and the state (7 kept as 7, whatever the reader says)
  s <- save_study(s)
  f <- file.path(s$path, "spec", "figures", "F-14-2-1.yml")
  expect_true(file.exists(f))
  expect_equal(tflspec::tfl_read_fig_design(f)$plot$title, "SBP: mean change")
  s2 <- open_study("SAMPLE-01")
  expect_identical(s2$planner$fig_designs, s$planner$fig_designs)

  # a copy of the figure has it too; a renamed one takes it along
  p <- copy_output(s2$planner, "F-14-2-1", "F-14-2-9")
  expect_equal(fig_design(p, "F-14-2-9")$template, "mean_ci")
  p <- rename_output(p, "F-14-2-9", "F-14-2-8")
  expect_null(fig_design(p, "F-14-2-9"))
  expect_equal(fig_design(p, "F-14-2-8")$template, "mean_ci")
  p <- remove_output(p, "F-14-2-8")
  expect_null(fig_design(p, "F-14-2-8"))

  # dropped: the program's plot is hand-written again, the YAML goes
  s2$planner <- set_fig_design(s2$planner, "F-14-2-1", NULL)
  s2 <- save_study(s2)
  expect_false(file.exists(f))
})

test_that("a design is drawn on the study's data, as its program saves it", {
  skip_if_not_installed("ggplot2")
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP",
                                  value = "CHG")
  r <- preview_figure(s, "F-14-2-1", d)
  expect_null(r$error)
  expect_true(file.exists(r$png))
  expect_equal(nrow(r$problems), 0L)
  expect_equal(r$size$dpi, 300)
  # the design against the data
  bad <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "NOPE",
                                    value = "NOVAR")
  r <- preview_figure(s, "F-14-2-1", bad)
  expect_true(any(grepl("no PARAMCD NOPE", r$problems$problem)))
  expect_true(any(grepl("NOVAR", r$problems$problem)))
})

test_that("presets are kept with the company standards, and start a design", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  expect_equal(nrow(fig_presets()), 0L)
  d <- tflspec::tfl_fig_template("km_risk_table", param = "TTDE", group = "TRT01A")
  d$plot$x_max <- 26
  f <- save_fig_preset(d, "KM standard", "KM with number at risk, 26 weeks")
  expect_true(file.exists(f))
  p <- fig_presets()
  expect_equal(p$name, "KM standard")
  expect_equal(p$template, "km_risk_table")
  d2 <- read_fig_preset("KM standard")
  expect_equal(d2$plot$x_max, 26)
  expect_equal(unclass(d2)[c("data", "stats", "layers")], .fig_norm(unclass(d))[c("data", "stats", "layers")])
  expect_error(save_fig_preset(d, "a/b"), "name")
  remove_fig_preset("KM standard")
  expect_equal(nrow(fig_presets()), 0L)
})

test_that("the preview carries the advice, and a fix applies", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  d <- tflspec::tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                                  pop = "SAFFL", group = "TRT01A")
  r <- preview_figure(s, "F-14-2-2", d)
  expect_null(r$error)
  expect_true("km_risk" %in% r$advice$rule)
  d2 <- tflspec::tfl_fig_apply_fix(d, r$advice$fix[[which(r$advice$rule == "km_risk")]])
  expect_true("risk_table" %in% vapply(d2$layers, `[[`, "", "layer"))
})

test_that("a designed figure is copied to other parameters", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", value = "CHG")
  p <- set_fig_design(.set_report_type(s$planner, "F-14-2-1", "figure"), "F-14-2-1", d)
  p$outputs$description[p$outputs$output_id == "F-14-2-1"] <- "Mean change in SYSBP"
  p2 <- copy_fig_to_params(p, "F-14-2-1", c("DIABP", "PULSE"))
  expect_true(all(c("F-14-2-1-DIABP", "F-14-2-1-PULSE") %in% p2$outputs$output_id))
  d2 <- fig_design(p2, "F-14-2-1-DIABP")
  expect_equal(Filter(function(x) identical(x$step, "param"), d2$data)[[1L]]$value, "DIABP")
  expect_equal(p2$outputs$description[p2$outputs$output_id == "F-14-2-1-PULSE"], "Mean change in PULSE")
  # the copies are figures, as the original (copy_output keeps the type)
  expect_equal(report_info(p2, "F-14-2-1-DIABP")$type, "figure")
  expect_error(copy_fig_to_params(p, "F-14-2-2", "X"), "no design")
})

test_that("a preview for the screen is smaller, and says the saved size", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", value = "CHG")
  full <- preview_figure(s, "F-14-2-1", d)
  small <- preview_figure(s, "F-14-2-1", d, max_px = 800)
  expect_null(small$error)
  # a PNG's width: bytes 17-20 of its header (IHDR), big-endian
  w <- function(f) {
    b <- as.integer(readBin(f, "raw", 24L)[17:20])
    sum(b * 256^(3:0))
  }
  expect_lte(w(small$png), 800)
  expect_gt(w(full$png), 800)
  expect_equal(small$size$dpi, full$size$dpi)
})

test_that("a design's other parts (ggplot2 version, composed figures) are kept", {
  skip_if_not("ggplot2_version" %in% names(formals(tflspec::tfl_fig_design)))
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP")
  d$ggplot2_version <- "3.5"
  p <- set_fig_design(new_planner(), "F1", d)
  expect_equal(fig_design(p, "F1")$ggplot2_version, "3.5")
})

test_that("a template's defaults the data has not got are replaced by the data's own", {
  d <- tflspec::tfl_fig_template("km_risk_table", data = "ADTTE")
  # the dataset: no OS, no FASFL, the group only in ADSL
  x <- data.frame(USUBJID = "A", PARAMCD = c("TTDE", "TTDE"), AVAL = 1, CNSR = 0,
                  SAFFL = "Y")
  adsl <- data.frame(USUBJID = "A", TRT01P = "Placebo", SAFFL = "Y", ITTFL = "Y")
  fit <- .template_fit(d, x, adsl)
  expect_identical(fit[["param"]], "TTDE")
  expect_identical(fit[["pop"]], "SAFFL")       # the dataset's own flag
  expect_identical(fit[["group"]], "TRT01P")    # named: only ADSL has it
  # what the user chose stays
  expect_length(.template_fit(d, x, adsl, given = c("param", "pop", "group")), 0L)
  # the data has the template's defaults: nothing to change
  ok <- data.frame(USUBJID = "A", PARAMCD = "OS", FASFL = "Y", TRT01P = "P")
  expect_length(.template_fit(d, ok, adsl), 0L)
  # the join takes from ADSL only what the dataset lacks
  d2 <- tflspec::tfl_fig_template("km_risk_table", data = "ADTTE", param = "TTDE",
                                  pop = "SAFFL", group = "TRT01P", join_adsl = TRUE)
  j <- Filter(function(s) identical(s$step, "join"), .trim_join(d2, x)$data)
  expect_identical(j[[1L]]$vars, "TRT01P")
  j0 <- Filter(function(s) identical(s$step, "join"),
               .trim_join(d2, cbind(x, TRT01P = "P"))$data)
  expect_length(j0, 0L)
})

test_that("the designer shows a piece's own lines of the design's code", {
  d <- tflspec::tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                                 pop = "SAFFL", group = "TRT01A")
  code <- .fig_design_script(d, "F-1")
  base <- .piece_code(code, d, list(sec = "layers", i = 1L))
  expect_true(startsWith(base[1L], "p <- ggsurvfit("))
  expect_false(any(grepl("^# ----", base)))
  df <- .piece_code(code, d, list(sec = "data", i = 1L))
  expect_true(startsWith(df[1L], "df <- adtte |>"))
})
