# The old forms of the SPEC (tflspec's `tflspec_old_form`, spec_version 2):
# what tflplanner writes is the current form, and a study of an old form
# says what to write instead -- not a spec that quietly does not read.

skip_on_cran()

of_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

of_study <- function(id) {
  p <- add_output(new_planner(), "T-1")
  save_study(create_study(id, planner = add_output(p, "T-2")))
}

test_that("a new study is written in the current form, and tflspec reads it", {
  of_home()
  s <- of_study("OF1")
  y <- yaml::read_yaml(file.path(s$path, "study.yml"))
  expect_identical(as.integer(y$spec_version), .tflspec_spec_version())
  expect_gte(as.integer(y$spec_version), 2L)
  st <- expect_no_error(tflspec::tfl_read_study_spec(s$path))
  expect_null(st$patterns)
  out <- expect_no_error(tflspec::tfl_read_outputs(s$path))
  expect_setequal(names(out), c("T-1", "T-2"))
  expect_false(dir.exists(file.path(s$path, "spec", "patterns")))
  # (no report file has an old key: a report-level settings:, last_step:)
  for (f in list.files(file.path(s$path, "spec", "outputs"), "\\.yml$", full.names = TRUE)) {
    k <- names(yaml::read_yaml(f))
    expect_false(any(c("settings", "last_step", "population") %in% k), info = basename(f))
  }
  # and opened again as it was saved
  s2 <- open_study("OF1")
  expect_identical(s2$spec$status, "same")
  expect_false(is.null(s2$planner$spec))
})

test_that("a study.yml with no spec_version opens with the old-form stop, as files that do not read", {
  home <- of_home()
  s <- of_study("OF2")
  f <- file.path(s$path, "study.yml")
  x <- readLines(f, encoding = "UTF-8")
  writeLines(x[!startsWith(x, "spec_version:")], f)
  e <- tryCatch(tflspec::tfl_read_study_spec(s$path), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  # the files changed outside tflplanner: read, and the stop said
  s2 <- suppressMessages(open_study("OF2"))
  expect_identical(s2$spec$status, "invalid")
  expect_true(s2$spec$old_form)
  expect_true(any(grepl("spec_version", s2$spec$problems$message, fixed = TRUE)))
  expect_message(.spec_note(s2), "older form of the SPEC", fixed = TRUE)
  expect_error(reload_from_spec("OF2"), "spec_version", class = "tflplanner_spec_invalid")
  # the files as the state last recorded them (no change outside): the same
  # stop, not a spec that quietly does not read
  st <- .study_from_state(.read_state("OF2", home))
  .write_state(st, home)
  s3 <- suppressMessages(open_study("OF2"))
  expect_identical(s3$spec$status, "invalid")
  expect_true(s3$spec$old_form)
  expect_identical(s3$spec$problems$file, "study.yml")
  expect_match(s3$spec$problems$message, "spec_version", fixed = TRUE)
  expect_null(s3$planner$spec)
  # a save does not write over it: the stop reaches the user
  expect_error(save_study(s3), class = "tflspec_old_form")
  expect_false(any(startsWith(readLines(f), "spec_version:")))
})

test_that("an old form found when a read may fail quietly is raised again", {
  e <- structure(class = c("tflspec_old_form", "error", "condition"),
                 list(message = "old", call = NULL, form = "x", where = "study.yml"))
  expect_error(.old_form_reraise(e), class = "tflspec_old_form")
  expect_null(.old_form_reraise(simpleError("other")))
  pr <- .old_form_problems(structure(class = c("tflspec_old_form", "error", "condition"),
                                     list(message = "m", call = NULL, form = "patterns_dir",
                                          where = "tfl_read_study_spec(): spec/patterns/")),
                           tempdir())
  expect_identical(pr$file, "spec/patterns/")
  expect_identical(pr$message, "m")
})

test_that("the sample study is of the current form: tflspec reads it, its notes too", {
  d <- system.file("sample", "SAMPLE-01", package = "tflplanner")
  y <- yaml::read_yaml(file.path(d, "study.yml"))
  expect_identical(as.integer(y$spec_version), .tflspec_spec_version())
  expect_no_error(tflspec::tfl_read_study_spec(d))
  out <- expect_no_error(tflspec::tfl_read_outputs(d))
  notes <- vapply(out, function(o) !is.null(o$note), NA)
  expect_gte(sum(notes), 13L)
})
