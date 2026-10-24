# A template whose data stage is the report's (a KM type): the card's
# data fields and the report's `data:` steps (#483)

test_that("the card hides the parameters the template takes from the study standard", {
  skip_if_not("km_survival" %in% tflspec::tfl_templates()$name)
  prm <- .tpl_params("km_survival")
  expect_true(.tpl_own_data(prm))
  shown <- names(.tpl_shown(prm))
  expect_false(any(c("population", "data", "param", "where", "event", "theme") %in% shown))
  expect_true(all(c("by", "time", "censor", "time_unit") %in% shown))
  expect_false(.tpl_own_data(.tpl_params("km")))
})

test_that("the fields write the steps, and read them back", {
  pop <- list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  st <- .tpl_steps_from("adtte", pop, "PFS", "AVAL >= 0")
  expect_identical(vapply(st, `[[`, "", "step"), c("read", "join", "param", "filter"))
  expect_identical(st[[1L]]$dataset, "ADTTE")
  expect_identical(st[[2L]], list(step = "join", dataset = "ADSL", where = "SAFFL == \"Y\"",
                                  by = "USUBJID", how = "semi"))
  f <- .tpl_steps_fields(st)
  expect_true(f$fields)
  expect_identical(f[c("dataset", "param", "filter")], list(dataset = "ADTTE", param = "PFS", filter = "AVAL >= 0"))
  expect_true(f$join)
  # (ADSL itself: no join; no dataset: no steps)
  expect_length(.tpl_steps_from("ADSL", pop), 1L)
  expect_null(.tpl_steps_from("", pop, "PFS"))
  expect_identical(.tpl_steps_fields(NULL)$dataset, "")
})

test_that("steps the fields cannot say are not taken apart", {
  st <- list(list(step = "read", dataset = "ADSL"),
             list(step = "derive", variable = "CNSR", expr = "as.integer(EOTSTT != \"DISCONTINUED\")"))
  expect_false(.tpl_steps_fields(st)$fields)
  st2 <- list(list(step = "read", dataset = "ADTTE"), list(step = "join", dataset = "ADSL", how = "left"))
  expect_false(.tpl_steps_fields(st2)$fields)
})

test_that("a KM type report with the card's steps resolves", {
  skip_if_not("km_survival" %in% tflspec::tfl_templates()$name)
  s <- tflspec::tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  pop <- list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  o <- tflspec::tfl_output("F-9", "figure", extends = "none",
                           template = list(name = "km_survival", args = list(by = "TRT01A")))
  expect_error(tflspec::tfl_resolve_output(o, s), "data:")
  o$data <- list(steps = .tpl_steps_from("ADTTE", pop, "PFS"))
  expect_true(attr(tflspec::tfl_resolve_output(o, s), "template")$ok)
})
