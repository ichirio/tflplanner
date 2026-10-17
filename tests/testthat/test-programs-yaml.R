# The programs written from the YAML spec's items (#411, step B2)

yaml_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

test_that("the programs from the spec's items are the sheets' programs", {
  s <- yaml_sample()
  p <- s$planner
  expect_false(is.null(.code_spec(p)))
  sheets <- .planner_body(p)
  expect_null(.code_spec(sheets))
  d <- as.Date("2026-01-01")
  for (id in c("T-14-1-1", "T-14-2-1", "T-14-2-2", "L-16-2-7", "F-14-2-1")) {
    expect_identical(program_code(p, id, date = d), program_code(sheets, id, date = d))
  }
  expect_identical(report_setup_code(p, date = d), report_setup_code(sheets, date = d))
})

test_that("a table's program writes what only its file holds", {
  s <- yaml_sample()
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  k <- which(vapply(o$content, function(it) identical(it$fn, "table_plan"), NA))
  o$content[[k]]$args$kept_arg <- "kept"
  p <- .spec_set_output(s$planner, id, o)
  code <- program_code(p, id)
  expect_true(any(grepl("kept_arg = \"kept\"", code, fixed = TRUE)))
  expect_false(any(grepl("(study default)", code, fixed = TRUE)))
  # the sheets cannot hold it
  expect_false(any(grepl("kept_arg", program_code(.planner_body(p), id), fixed = TRUE)))
})

test_that("a report's analysis set is its token when its page says none", {
  s <- yaml_sample()
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  o$page$tokens$OUTPUT_POPULATION <- NULL
  p <- .spec_set_output(s$planner, id, o)
  code <- program_code(p, id)
  expect_true(any(grepl("OUTPUT_POPULATION = \"Safety Analysis Set\"", code, fixed = TRUE)))
  # (not written into the report)
  expect_null(p$spec$outputs[[id]]$page$tokens$OUTPUT_POPULATION)
})

test_that("a view with edits not yet in the spec is written from the sheets", {
  s <- yaml_sample()
  id <- "T-14-1-1"
  p <- set_sheet_rows(s$planner, "footnotes", id,
                      data.frame(line = "1", left = "An edited footnote."))
  expect_null(.code_spec(p))
  code <- program_code(p, id)
  expect_true(any(grepl("An edited footnote.", code, fixed = TRUE)))
  expect_identical(code, program_code(.planner_body(p), id))
})
