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
  # (ADSL itself: its analysis set still kept; no dataset: no steps)
  expect_identical(.tpl_steps_from("ADSL", pop)[[2L]]$where, "SAFFL == \"Y\"")
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

test_that("a join no longer the analysis set's is told", {
  saf <- list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  itt <- list(population_id = "ITT", dataset = "ADSL", where = "ITTFL == \"Y\"")
  f <- .tpl_steps_fields(.tpl_steps_from("ADTTE", saf, "PFS"))
  expect_false(.tpl_join_stale(f, saf))
  expect_true(.tpl_join_stale(f, itt))
  # (no population: the report's own join, a flag the study does not define)
  expect_false(.tpl_join_stale(f, NULL))
  expect_true(.tpl_join_stale(.tpl_steps_fields(.tpl_steps_from("ADTTE", NULL, "PFS")), saf))
  expect_false(.tpl_join_stale(.tpl_steps_fields(.tpl_steps_from("ADSL", saf)), saf))
})

test_that("the study's treatment is the `by`, and ADSL's flags no population says are offered", {
  skip_if_not("km_survival" %in% tflspec::tfl_templates()$name)
  study <- list(study = list(settings = list(treatment = "TRT01P")))
  prm <- .tpl_params("km_survival")
  expect_identical(.tpl_study_value(study, prm$by), "TRT01P")
  expect_true("by" %in% .tpl_missing("km_survival", list()))
  expect_false("by" %in% .tpl_missing("km_survival", list(), study))
  adsl <- data.frame(SAFFL = c("Y", "N"), ITTFL = c("Y", "Y"), EFFFL = c("N", ""),
                     TRTFL = c("A", "B"), AGE = 1:2)
  pops <- data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  expect_identical(.tpl_flags_undefined(adsl, pops), c("ITTFL", "EFFFL"))
  expect_identical(.tpl_flags_undefined(NULL), character())
})

test_that("a report's analysis set changed on the card is its toc line's too", {
  study <- tflspec::tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  study$toc <- list(outputs = list(list(id = "F-1", kind = "figure", population = "SAF"),
                                   list(id = "F-2", kind = "figure")))
  p <- list(spec = list(study = study, text = list()),
            outputs = data.frame(output_id = c("F-1", "F-2"), population = c("SAF", NA)))
  p2 <- .spec_set_toc_population(p, "F-1", "ITT")
  expect_identical(p2$spec$study$toc$outputs[[1L]]$population, "ITT")
  expect_identical(p2$outputs$population, c("ITT", NA))
  p3 <- .spec_set_toc_population(p2, "F-1", NA_character_)
  expect_null(p3$spec$study$toc$outputs[[1L]]$population)
  expect_true(any(grepl("F-1", unlist(p3$spec$text), fixed = TRUE)))
  expect_identical(.spec_set_toc_population(p, "F-9", "ITT"), p)
})

test_that("a new report's ID gets its type's prefix once", {
  expect_identical(.id_with_prefix("14.1.1", "table"), "T-14.1.1")
  expect_identical(.id_with_prefix(" t-14.1.1 ", "table"), "t-14.1.1")
  expect_identical(.id_with_prefix("1", "figure"), "F-1")
  expect_identical(.id_with_prefix("1", "listing"), "L-1")
  expect_identical(.id_with_prefix("", "table"), "")
  expect_identical(.id_with_prefix("X1", NULL), "X1")
})
