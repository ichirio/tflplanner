# A report's argument saved as the text of R code (rtfreporter#958) is found
# and warned about on open (open_study(); the app says it in set_study()).

bad_text <- "list(structure(\"n == 0 ~ \\\"0\\\"\", class = \"tfl_r\", tag = \"!r\"), \"{n} ({p:.1f%}%)\")"

test_that(".deparsed_args() finds deparsed values at any depth", {
  o1 <- list(id = "T-1", template = list(name = "demog", args = list(
    by = "TRT01A", count_cell = bad_text)))
  o2 <- list(id = "T-2", template = list(name = "demog", args = list(
    cells = list(n = "{n}", "Mean (SD)" = bad_text))))
  o3 <- list(id = "T-3", template = NULL, content = list(
    list(fn = "plan_cells", args = list(default = bad_text))))
  f <- .deparsed_args(list(o1, o2, o3))
  expect_identical(f$output_id, c("T-1", "T-2", "T-3"))
  expect_identical(f$where[1:2], c("template.args.count_cell", "template.args.cells.Mean (SD)"))
  expect_match(f$where[3], "^content\\.plan_cells\\.default$")
})

test_that(".deparsed_args() finds nothing in real values", {
  o <- list(id = "T-1", template = list(name = "demog", args = list(
    count_cell = list(tflspec::tfl_r("n == 0 ~ \"0\""), "{n} ({p:.1f%}%)"),
    other = "{n} ({p:.1f%}%)", note = "list of things")))
  f <- .deparsed_args(list(o))
  expect_identical(nrow(f), 0L)
  expect_identical(names(f), c("output_id", "where"))
})

test_that("opening a study with such a value warns with the report id", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  f <- file.path(s$path, "spec", "outputs", "T-BAD.yml")
  tflspec::tfl_write_output(tflspec::tfl_output(
    "T-BAD", "table", label = "x", common = list(population = "SAF"),
    template = list(name = "demog", args = list(
      by = "TRT01A", categorical = "SEX", count_cell = bad_text))), f)
  # (taken in from the files, then again from the state: each open says it)
  expect_warning(suppressMessages(open_study(s$meta$study_id, home = home)), "T-BAD")
  expect_warning(suppressMessages(open_study(s$meta$study_id, home = home)), "T-BAD")
})
