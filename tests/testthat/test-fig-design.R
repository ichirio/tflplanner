test_that("a figure's design is kept, saved as YAML, and makes its plot", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE))
  expect_null(fig_design(s$planner, "F-14-2-1"))
  d <- tflspec::tfl_fig_template("mean_ci", data = "ADVS", param = "SYSBP",
                                  value = "CHG", title = "SBP: mean change")
  d$plot$width <- 7L
  s$planner <- set_fig_design(s$planner, "F-14-2-1", d)

  # the program's plot is the design's code, reading what it needs
  code <- data_lines(s$planner, "F-14-2-1")
  expect_true(any(grepl("advs <- ", code, fixed = TRUE)))
  expect_true(any(grepl("adsl <- ", code, fixed = TRUE)))
  expect_true(any(grepl("^plot <- fig$", unlist(strsplit(code, "\n", fixed = TRUE)))))
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
