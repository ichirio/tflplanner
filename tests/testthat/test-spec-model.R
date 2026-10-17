# The study's model is its YAML spec; the sheets are a view of it (#407 B1)

model_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

spec_md5 <- function(root) {
  f <- .spec_files_yaml(root)
  stats::setNames(unname(tools::md5sum(file.path(root, f))), f)
}

changed_files <- function(a, b) {
  k <- union(names(a), names(b))
  k[!vapply(k, function(x) identical(a[x], b[x]), NA)]
}

test_that("an opened study holds its spec, in step with the sheets' view", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  sp <- s$planner$spec
  expect_s3_class(sp$study, "tfl_study_spec")
  expect_setequal(names(sp$outputs), s$planner$outputs$output_id)
  expect_true(all(vapply(sp$outputs, inherits, NA, "tfl_output")))
  expect_identical(sp$where[["T-14-1-1"]], "spec/outputs/T-14-1-1.yml")
  expect_true(all(c(.study_file, "spec/toc.yml") %in% names(sp$text)))
  expect_true(.same_planner(sp$base, s$planner))
  # the view made from the spec is the sheets the screens read
  v <- .planner_from_spec(sp$study, sp$outputs)
  expect_identical(v$sheets$tables, s$planner$sheets$tables)
  expect_identical(v$outputs, .planner_body(s$planner)$outputs)
  # the spec is not part of what is compared or kept
  expect_null(.state_of(s)$planner$spec)
  expect_true(.same_planner(s$planner, .planner_body(s$planner)))
})

test_that("opened and saved with no change, no spec file is written", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
  y <- s$files[grepl("\\.ya?ml$", s$files$file), ]
  expect_true(all(y$status == "unchanged"))
})

test_that("an edit of one report's sheet rows writes that report's file only", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  id <- "T-14-1-1"
  d <- sheet_rows(s$planner, "tables", id)
  d$output_id <- NULL
  d$note <- "an edit"
  s$planner <- set_sheet_rows(s$planner, "tables", id, d)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), "spec/outputs/T-14-1-1.yml")
  # the spec in memory follows, in step again
  expect_true(.same_planner(s$planner$spec$base, s$planner))
  expect_identical(s$planner$spec$text[["spec/outputs/T-14-1-1.yml"]],
                   readLines(file.path(s$path, "spec/outputs/T-14-1-1.yml"), encoding = "UTF-8"))
  # and reads back as saved
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  expect_identical(sheet_rows(s2$planner, "tables", id)$note[1L], "an edit")
})

test_that("what the sheets cannot hold stays in a report the edits do not touch", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  # a page key with no sheet, written straight into one report's spec
  o <- s$planner$spec$outputs[["T-14-3-1"]]
  o$page$kept_key <- "kept"
  s$planner <- .spec_set_output(s$planner, "T-14-3-1", o)
  expect_identical(s$planner$spec$outputs[["T-14-3-1"]]$page$kept_key, "kept")
  # an edit of another report through its sheets
  d <- sheet_rows(s$planner, "tables", "T-14-1-1")
  d$output_id <- NULL
  d$note <- "another edit"
  s$planner <- set_sheet_rows(s$planner, "tables", "T-14-1-1", d)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_setequal(changed_files(m0, spec_md5(s$path)),
                  c("spec/outputs/T-14-1-1.yml", "spec/outputs/T-14-3-1.yml"))
  back <- tflspec::tfl_read_output(file.path(s$path, "spec/outputs/T-14-3-1.yml"))
  expect_identical(back$page$kept_key, "kept")
  # the programs as before
  expect_identical(program_code(s$planner, "T-14-3-1"),
                   program_code(.planner_body(s$planner), "T-14-3-1"))
})

test_that("a report written into the spec has its view made again", {
  s <- model_sample()
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  p <- .spec_set_output(s$planner, id, o)
  # the same report: the same view
  expect_identical(.planner_body(p)$sheets, .planner_body(s$planner)$sheets)
  expect_identical(p$outputs, s$planner$outputs)
  # its population changed in the spec: the report list says it
  o$population <- "SAF"
  p <- .spec_set_output(s$planner, id, o)
  expect_identical(.spec_output(p, id)$population, "SAF")
  expect_true(.same_planner(p$spec$base, p))
  expect_error(.spec_set_output(.planner_body(s$planner), id, o), "no YAML spec")
})

test_that("a planner with no spec is written whole, and gets one", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s$planner <- .planner_body(s$planner)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_false(is.null(s$planner$spec))
  expect_setequal(names(s$planner$spec$outputs), s$planner$outputs$output_id)
  # the sample's files are what the bridge writes of it
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
})

test_that("a study with no report keeps its spec a tfl_study_spec (#413)", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_study("Z-01", home = home))
  expect_identical(nrow(s$planner$outputs), 0L)
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_identical(.study_format(s$path), "yaml")
  # reopened and saved again: still the spec, in step
  s <- suppressMessages(open_study("Z-01", home = home))
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_length(s$planner$spec$outputs, 0L)
  s <- suppressMessages(save_study(s, home = home))
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_true(.same_planner(s$planner$spec$base, s$planner))
  # its programs are written from the spec
  expect_false(is.null(.code_spec(s$planner)))
})
