# A template report may write its arguments flat (`template: args:`) or by
# step (`template: analysis:` / `content:`), as tflspec reads them (#751,
# item 1).  The cards read both; tflplanner writes a report back in the form
# it has, and a new report by step (the canon's form).

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

test_that(".tpl_of() gives T-14-1-1T's values, and .tpl_output() writes it back by step", {
  o <- tpl_by_step()
  p <- list(spec = list(outputs = list(`T-14-1-1T` = o)))
  tp <- .tpl_of(p, "T-14-1-1T")
  expect_identical(tp$name, "tflspec_v1/demog")
  expect_true(all(c("by", "continuous", "categorical", "labels", "cells") %in% names(tp$args)))
  # (unchanged: the same file)
  expect_identical(.tpl_output(p, "T-14-1-1T", tp$args)$template, o$template)
  # (an edit: in its section, not lost, no flat `args:`)
  args <- tp$args
  args$max_rows <- 20L
  args$continuous <- c("AGE", "HEIGHTBL")
  w <- .tpl_output(p, "T-14-1-1T", args)
  expect_null(w$template$args)
  expect_identical(w$template$content$max_rows, 20L)
  expect_identical(w$template$analysis$continuous, c("AGE", "HEIGHTBL"))
  expect_identical(.tpl_args_of(w$template)$max_rows, 20L)
  # (a removed argument is gone; the report's own fields stay)
  args$max_rows <- NULL
  w <- .tpl_output(p, "T-14-1-1T", args)
  expect_false("max_rows" %in% names(.tpl_args_of(w$template)))
  expect_identical(w$page$tokens$OUTPUT_POPULATION, "Safety Analysis Set")
  expect_identical(w$style, "standard")
})

test_that("a flat report stays flat; a name in both goes flat; an emptied section is dropped", {
  t <- list(name = "tflspec_v1/demog", args = list(by = "TRT01A", max_rows = 30L))
  w <- .tpl_write_args(t, list(by = "TRT01A", max_rows = 12L, continuous = "AGE"))
  expect_identical(w, list(name = "tflspec_v1/demog",
                           args = list(by = "TRT01A", max_rows = 12L, continuous = "AGE")))
  t2 <- list(name = "x", args = list(by = "ARM"), analysis = list(by = "TRT01A", continuous = "AGE"))
  expect_identical(.tpl_args_of(t2)$by, "ARM")
  w <- .tpl_write_args(t2, .tpl_args_of(t2))
  expect_identical(w$args, list(by = "ARM"))
  expect_identical(w$analysis, list(continuous = "AGE"))
  w <- .tpl_write_args(t2, list(by = "ARM"))
  expect_false("analysis" %in% names(w))
})

test_that("a new argument of a by-step report, and a new report, go to their step's section", {
  ok <- tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")), error = function(e) FALSE)
  skip_if(!isTRUE(ok), "this tflspec has no series templates (tflspec#824)")
  st <- function(a) .tpl_arg_step("tflspec_v1/demog", a)
  expect_identical(st("continuous"), "analysis")
  expect_identical(st("max_rows"), "content")
  t <- list(name = "tflspec_v1/demog", analysis = list(by = "TRT01A"))
  w <- .tpl_write_args(t, list(by = "TRT01A", continuous = "AGE", max_rows = 9L))
  expect_identical(w$analysis, list(by = "TRT01A", continuous = "AGE"))
  expect_identical(w$content, list(max_rows = 9L))
  # (an older file's `ard:` takes the analysis step)
  w <- .tpl_write_args(list(name = "tflspec_v1/demog", ard = list(by = "TRT01A")),
                       list(by = "TRT01A", continuous = "AGE"))
  expect_identical(w$ard, list(by = "TRT01A", continuous = "AGE"))
  expect_null(w$analysis)
  # (a new report: by step)
  w <- .tpl_write_args(list(name = "tflspec_v1/demog"), list(continuous = "AGE", max_rows = 9L), by_step = TRUE)
  expect_identical(w, list(name = "tflspec_v1/demog", analysis = list(continuous = "AGE"),
                           content = list(max_rows = 9L)))
  # (an argument whose step the template does not say: flat)
  w <- .tpl_write_args(list(name = "tflspec_v1/demog"), list(nosuch = 1L), by_step = TRUE)
  expect_identical(w$args, list(nosuch = 1L))
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
  # (an edit written back by step: tflspec reads the new value)
  args <- tp$args
  args$max_rows <- 12L
  w <- .tpl_output(p, "T-14-1-1T", args)
  expect_identical(w$template$content$max_rows, 12L)
  w2 <- .tpl_output(p, "T-14-1-1T", tp$args)
  sp <- .code_spec(p)
  a <- tflspec::tfl_resolve_output(sp$outputs[["T-14-1-1T"]], sp$study)
  b <- tflspec::tfl_resolve_output(w2, sp$study)
  expect_identical(b$content, a$content)
  expect_identical(b$ard, a$ard)
  e <- tflspec::tfl_resolve_output(w, sp$study)
  expect_false(identical(e$content, a$content))
})
