# A code SPEC figure report's content key, `figure: {name, args}` (tflspec#834,
# #835): the GUI keeps it through a save, the checks do not call it an error
# (and say a real problem), and the program is tflspec's call.  Fictional data
# only (the sample study).

# The sample study with a figure report F-9 that has the key: written the way
# tflspec writes a series figure template out (tfl_template_to_spec()), added
# to the TOC, taken in
fk_study <- function(name = "km_curves", env = parent.frame()) {
  skip_if_not(exists(".fig_content_key", envir = asNamespace("tflspec"), inherits = FALSE),
              "tflspec has no figure: key yet")
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  f <- file.path(s$path, "spec", "outputs", "F-9.yml")
  writeLines(c("id: F-9", "kind: figure", "label: KM by arm", "extends:", "  - defaults",
               "data:", "  - id: adsl_sub", "    population: SAF",
               "template:", "  name: tflspec_v1/km_simple",
               "  data:", "    time_unit: months", "  analysis:", "    by: TRT01A"), f)
  spec <- tflspec::tfl_template_to_spec(tflspec::tfl_read_output(f),
                                        tflspec::tfl_read_study_spec(s$path))
  spec$figure$name <- name
  tflspec::tfl_write_output(spec, f)
  cat("  - id: F-9\n    kind: figure\n    label: KM by arm\n",
      file = file.path(s$path, "spec", "toc.yml"), append = TRUE)
  list(study = suppressMessages(reload_from_spec(s$meta$study_id, home = home)),
       home = home, file = f, figure = spec$figure)
}

test_that("the figure: key is kept in the planner's figure design", {
  skip_on_cran()
  k <- fk_study()
  p <- k$study$planner
  expect_true("F-9" %in% p$outputs$output_id)
  expect_identical(report_info(p, "F-9")$type, "figure")
  # the design the GUI reads (fig_design()) has it, and so has a design
  # the designer sets back (set_fig_design())
  d <- fig_design(p, "F-9")
  expect_identical(d$figure$name, "km_curves")
  expect_identical(d$figure$args$by, "TRT01A")
  p2 <- set_fig_design(p, "F-9", fig_design(p, "F-9"))
  expect_identical(fig_design(p2, "F-9")$figure, d$figure)
})

test_that("a save writes the figure: key back as it was (an edit of the report, a design set back)", {
  skip_on_cran()
  k <- fk_study()
  s <- k$study
  # the report's description edited and its design set back, as the GUI
  # does on a save of the figure
  i <- match("F-9", s$planner$outputs$output_id)
  s$planner$outputs$description[i] <- "KM by arm (changed)"
  s$planner <- set_fig_design(s$planner, "F-9", fig_design(s$planner, "F-9"))
  s <- suppressMessages(save_study(s, home = k$home))
  expect_true(s$files$status[match(normalizePath(k$file, "/"), normalizePath(s$files$file, "/"))] %in%
                c("written", "unchanged"))
  txt <- readLines(k$file)
  expect_true("figure:" %in% txt)
  back <- tflspec::tfl_read_output(k$file)
  expect_identical(back$figure, k$figure)
  expect_null(back$template)
  # and it still reads back into the planner, the same
  s2 <- suppressMessages(open_study(s$path, home = k$home))
  expect_identical(fig_design(s2$planner, "F-9")$figure, k$figure)
})

test_that("the checks pass a figure: report, and say an unknown name and a missing argument", {
  skip_on_cran()
  k <- fk_study()
  fig_rows <- function(s) {
    r <- .check_spec(s, "F-9")
    r[grepl("figure", r$column) | grepl("F-9", r$file), , drop = FALSE]
  }
  expect_identical(nrow(fig_rows(k$study)), 0L)
  expect_identical(nrow(tflspec::tfl_check_spec(tflspec::tfl_read_study_spec(k$study$path),
                                                output_id = "F-9")), 0L)
  # an unknown name in the file: the study does not take it in, saying why
  expect_error(fk_study("nonesuch"), "the figure `nonesuch` is not a figure content",
               class = "tflplanner_spec_invalid")
  # one set in the design (the GUI's): the check says it, as an error of the
  # figure's own
  p <- k$study$planner
  d <- fig_design(p, "F-9")
  d$figure$name <- "nonesuch"
  r <- .check_fig("F-9", d, p)
  expect_identical(nrow(r), 1L)
  expect_identical(r$severity, "error")
  expect_identical(r$column, "figure")
  expect_match(r$message, "not a figure content")
  expect_match(r$message, "km_curves")
  # an argument the model needs, left out
  d <- fig_design(p, "F-9")
  d$figure$args$time <- NULL
  d$figure$args$censor <- NULL
  expect_match(.check_fig("F-9", d, p)$message, "needs time, censor")
})

test_that("the program is tflspec's call, the model's fit first", {
  skip_on_cran()
  k <- fk_study()
  code <- program_code(k$study$planner, "F-9")
  fit <- grep("km_fit <-", code, fixed = TRUE)
  call <- grep("^content <- tflspec::km_curves\\(", code)
  expect_length(call, 1L)
  expect_true(length(fit) >= 1L && fit[1L] < call)
  expect_match(paste(code, collapse = "\n"), "Surv_CNSR(AVAL, CNSR) ~ TRT01A", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), "time_unit = \"months\"", fixed = TRUE)
  # not the designer's empty plot
  expect_false(any(grepl("^plot <- ggplot\\(\\)", code)))
  # (placed and drawn at its size: the study's figure style for its type,
  # tflspec#883)
  expect_true(any(grepl("rtf_figures(content", code, fixed = TRUE)))
  # it reads as R
  expect_no_error(parse(text = code))
})
