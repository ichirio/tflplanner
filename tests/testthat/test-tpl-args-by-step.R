# A template report may write its arguments flat (`template: args:`) or by
# step (`template: analysis:` / `content:`), as tflspec reads them (#751,
# item 1).  The cards read both; tflplanner writes them flat.

tpl_by_step <- function() {
  yaml::read_yaml(testthat::test_path("fixtures", "twins", "T-14-1-1T.yml"))
}

test_that("a template's arguments by step are read as one map, flat ones first", {
  o <- tpl_by_step()
  a <- .tpl_args_of(o$template)
  expect_identical(a$continuous, "AGE")
  expect_identical(a$by, "TRT01A")
  expect_identical(a$labels$AGE, "Age (years)")
  expect_identical(a$max_rows, 30L)
  # (flat `args:` alone, as before)
  expect_identical(.tpl_args_of(list(name = "x", args = list(by = "ARM"))), list(by = "ARM"))
  # (a name in both: the flat value)
  t2 <- o$template
  t2$args <- list(max_rows = 10L)
  expect_identical(.tpl_args_of(t2)$max_rows, 10L)
  # (not a map: not a step section)
  expect_identical(.tpl_args_of(list(name = "x", content = "a")), list())
})

test_that(".tpl_of() gives T-14-1-1T's values, and .tpl_output() writes them flat", {
  o <- tpl_by_step()
  p <- list(spec = list(outputs = list(`T-14-1-1T` = o)))
  tp <- .tpl_of(p, "T-14-1-1T")
  expect_identical(tp$name, "tflspec_v1/demog")
  expect_true(all(c("by", "continuous", "categorical", "labels", "cells") %in% names(tp$args)))
  args <- tp$args
  args$max_rows <- 20L
  w <- .tpl_output(p, "T-14-1-1T", args)
  expect_false(any(c("analysis", "content") %in% names(w$template)))
  expect_identical(w$template$args$max_rows, 20L)
  expect_identical(w$template$args$continuous, "AGE")
  # (the report's own fields stay)
  expect_identical(w$page$tokens$OUTPUT_POPULATION, "Safety Analysis Set")
  expect_identical(w$style, "standard")
})

test_that("T-14-1-1T in SAMPLE-01 has no empty required argument and its preview code runs", {
  skip_on_cran()
  ok <- tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")), error = function(e) FALSE)
  skip_if(!isTRUE(ok), "this tflspec has no series templates (tflspec#824)")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  f <- testthat::test_path("fixtures", "twins", "T-14-1-1T.yml")
  expect_true(file.copy(f, file.path(s$path, "spec", "outputs", "T-14-1-1T.yml")))
  toc_file <- file.path(s$path, "spec", "toc.yml")
  toc <- yaml::read_yaml(toc_file)
  toc$outputs <- c(toc$outputs, list(list(id = "T-14-1-1T", kind = "table", label = "Demographics",
                                          population = "SAF")))
  yaml::write_yaml(toc, toc_file)
  s <- suppressMessages(reload_from_spec(s, home = home))
  p <- s$planner
  tp <- .tpl_of(p, "T-14-1-1T")
  expect_identical(tp$args$continuous, "AGE")
  expect_length(.tpl_missing(tp$name, tp$args, p$spec$study), 0L)
  # (written flat, tflspec reads it as it read the steps)
  w <- .tpl_output(p, "T-14-1-1T", tp$args)
  sp <- .code_spec(p)
  a <- tflspec::tfl_resolve_output(sp$outputs[["T-14-1-1T"]], sp$study)
  b <- tflspec::tfl_resolve_output(w, sp$study)
  expect_identical(b$content, a$content)
  expect_identical(b$ard, a$ard)
})
