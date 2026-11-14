# R/data_rows.R: the four-steps data step's table model (fictional reports)

# ---- fixtures ---------------------------------------------------------------

dm_report <- function(args = list(population = "SAF", by = "TRT01A",
                                  continuous = "AGE", categorical = c("SEX", "RACE"))) {
  list(common = list(population = "SAF", treatment = "TRT01A",
                     population_label = "Safety Analysis Set"),
       template = list(name = "demog", args = args))
}

ae_report <- function(args = list(population = "SAF",
                                  where = "TRTEMFL == \"Y\" & AESEV == \"SEVERE\"")) {
  list(common = list(population = "SAF", treatment = "TRT01A"),
       template = list(name = "ae_soc_pt", args = args))
}

code_report <- function() {
  list(
    common = list(population = "SAF", treatment = "TRT01A",
                  population_label = "Safety Analysis Set"),
    data = list(
      list(id = "adsl_sub", from = "ADSL"),
      list(id = "adae_sub", from = "ADAE", subjects = "adsl_sub",
           where = tflspec::tfl_r("TRTEMFL == \"Y\""),
           derive = list(list(name = "SEVN", code = "as.integer(AESEV == \"SEVERE\")")),
           keep = c("USUBJID", "TRT01A", "AEBODSYS", "AEDECOD", "SEVN")),
      list(id = "adae_den", from = "adae_sub", label = "Subjects with a record",
           code = tflspec::tfl_r("dplyr::distinct(adae_sub, USUBJID, TRT01A)"))
    ),
    content = list(list(fn = "plan_hierarchical")))
}

# a template's parameters given by hand (no tflspec lookup)
lab_template <- list(
  population = list(name = "population", default = NULL),
  subset = list(name = "subset", default = NULL),
  data = list(name = "data", default = "ADLB"),
  where = list(name = "where", default = "ANL01FL == \"Y\""),
  data_id = list(name = "data_id", default = NULL))

# ---- template reports -------------------------------------------------------

test_that("a DM template report has adsl_sub only, its cells mapped to the arguments", {
  rows <- .data_rows(dm_report(), list())
  expect_length(rows, 1L)
  r <- rows[[1L]]
  expect_identical(names(r), c("id", "from", "population", "subjects", "where",
                               "source", "param", "optional"))
  expect_identical(r$id, "adsl_sub")
  expect_identical(r$from, "ADSL")
  expect_identical(r$population, "SAF")
  expect_null(r$subjects)
  expect_identical(r$where, "")
  expect_identical(r$source, "template")
  expect_identical(r$param, list(population = "population", where = "subset"))
  expect_false(r$optional)
})

test_that("a DM template report takes common: population when its args have none", {
  rep <- dm_report(list(continuous = "AGE"))
  rep$common$population <- "ITT"
  expect_identical(.data_rows(rep, list())[[1L]]$population, "ITT")
})

test_that("a DM template report's edits are template arguments", {
  rep <- dm_report()
  rows <- .data_rows(rep, list())
  rows[[1L]]$where <- "AGE >= 65"
  rows[[1L]]$population <- "ITT"
  ch <- .data_rows_args(rep, rows)
  expect_identical(ch, list(population = "ITT", subset = "AGE >= 65"))
  out <- .data_rows_write(rep, rows)
  expect_identical(out$template$args$subset, "AGE >= 65")
  expect_identical(out$template$args$population, "ITT")
  expect_null(out$data)
  expect_identical(out[names(out) != "template"], rep[names(rep) != "template"])
  # cleared again: the argument goes, its default applies
  rows2 <- .data_rows(out, list())
  rows2[[1L]]$where <- ""
  back <- .data_rows_write(out, rows2)
  expect_false("subset" %in% names(back$template$args))
})

test_that("an AE template report: adsl_sub, the records with where, an optional denominator", {
  rows <- .data_rows(ae_report(), list())
  ids <- vapply(rows, `[[`, "", "id")
  expect_identical(ids, c("adsl_sub", "adae_sub", "adae_den"))
  rec <- rows[[2L]]
  expect_identical(rec$from, "ADAE")
  expect_identical(rec$subjects, "adsl_sub")
  expect_identical(rec$population, "SAF")
  expect_identical(rec$where, "TRTEMFL == \"Y\" & AESEV == \"SEVERE\"")
  # (ae_soc_pt has no data_id: the id is not edited)
  expect_identical(rec$param, list(from = "data", where = "where"))
  expect_false(rec$optional)
  den <- rows[[3L]]
  expect_true(den$optional)
  expect_identical(den$from, "adae_sub")
  expect_identical(den$subjects, "adsl_sub")
  expect_identical(den$where, "")
  expect_length(den$param, 0L)
  expect_identical(vapply(rows, `[[`, "", "source"), rep("template", 3L))
})

test_that("an AE template report's records show the template's default where", {
  rows <- .data_rows(ae_report(list(population = "SAF")), list())
  expect_identical(rows[[2L]]$where, "TRTEMFL == \"Y\"")
})

test_that("an AE template report's edits: where and data", {
  rep <- ae_report()
  rows <- .data_rows(rep, list())
  rows[[2L]]$where <- "TRTEMFL == \"Y\""
  rows[[2L]]$from <- "ADAEX"
  ch <- .data_rows_args(rep, rows)
  expect_identical(ch, list(data = "ADAEX", where = "TRTEMFL == \"Y\""))
  out <- .data_rows_write(rep, rows)
  expect_identical(vapply(.data_rows(out, list()), `[[`, "", "id"),
                   c("adsl_sub", "adaex_sub", "adaex_den"))
})

test_that("a template with data_id: the records' id edits it, and back", {
  rep <- list(common = list(population = "SAF"), template = list(name = "lab_x", args = list()))
  rows <- .data_rows(rep, list(), template = lab_template)
  expect_identical(rows[[2L]]$param, list(id = "data_id", from = "data", where = "where"))
  rows[[2L]]$id <- "adlb_alt"
  out <- .data_rows_write(rep, rows, template = lab_template)
  expect_identical(out$template$args, list(data_id = "adlb_alt"))
  expect_identical(.data_rows(out, list(), template = lab_template)[[2L]]$id, "adlb_alt")
  rows2 <- .data_rows(out, list(), template = lab_template)
  rows2[[2L]]$id <- "adlb_sub"
  expect_length(.data_rows_write(out, rows2, template = lab_template)$template$args, 0L)
  expect_identical(.data_rows_write(rep, .data_rows(rep, list(), template = lab_template),
                                    template = lab_template), rep)
})

test_that("a template's parameters can be given; R code in an argument stays R code", {
  rep <- list(common = list(population = "SAF"),
              template = list(name = "lab_x", args = list(
                where = tflspec::tfl_r("ANL01FL == \"Y\" & PARAMCD == \"ALT\""))))
  rows <- .data_rows(rep, list(), template = lab_template)
  expect_identical(vapply(rows, `[[`, "", "id"), c("adsl_sub", "adlb_sub"))
  expect_identical(rows[[2L]]$where, "ANL01FL == \"Y\" & PARAMCD == \"ALT\"")
  expect_identical(.data_rows_args(rep, rows, template = lab_template), list())
  rows[[2L]]$where <- "PARAMCD == \"AST\""
  ch <- .data_rows_args(rep, rows, template = lab_template)
  expect_s3_class(ch$where, "tfl_r")
  expect_identical(as.character(unclass(ch$where)), "PARAMCD == \"AST\"")
})

test_that("template reports round-trip unchanged", {
  for (rep in list(dm_report(), dm_report(list(continuous = "AGE")),
                   ae_report(), ae_report(list(population = "SAF")))) {
    expect_identical(.data_rows_write(rep, .data_rows(rep, list())), rep)
    expect_identical(.data_rows_args(rep, .data_rows(rep, list())), list())
  }
})

# ---- code SPEC reports ------------------------------------------------------

test_that("a code SPEC report's rows are its data:, in build order", {
  rows <- .data_rows(code_report(), list())
  expect_identical(vapply(rows, `[[`, "", "id"), c("adsl_sub", "adae_sub", "adae_den"))
  expect_identical(vapply(rows, `[[`, "", "source"), rep("own", 3L))
  expect_identical(vapply(rows, `[[`, "", "population"), rep("SAF", 3L))
  expect_null(rows[[1L]]$subjects)
  expect_identical(rows[[2L]]$subjects, "adsl_sub")
  expect_identical(rows[[2L]]$where, "TRTEMFL == \"Y\"")
  expect_identical(rows[[3L]]$where, "")
  expect_identical(rows[[3L]]$from, "adae_sub")
  expect_null(rows[[1L]]$param)
  expect_false(any(vapply(rows, `[[`, NA, "optional")))
})

test_that("a code SPEC report round-trips unchanged, derive and keep kept", {
  rep <- code_report()
  expect_identical(.data_rows_write(rep, .data_rows(rep, list())), rep)
})

test_that("a code SPEC report's edits keep the other keys and R code's class", {
  rep <- code_report()
  rows <- .data_rows(rep, list())
  rows[[2L]]$where <- "TRTEMFL == \"Y\" & AESER == \"Y\""
  out <- .data_rows_write(rep, rows)
  e <- out$data[[2L]]
  expect_s3_class(e$where, "tfl_r")
  expect_identical(as.character(unclass(e$where)), "TRTEMFL == \"Y\" & AESER == \"Y\"")
  expect_identical(e$derive, rep$data[[2L]]$derive)
  expect_identical(e$keep, rep$data[[2L]]$keep)
  expect_identical(names(e), names(rep$data[[2L]]))
  expect_identical(out$data[[3L]], rep$data[[3L]])
  expect_identical(out[names(out) != "data"], rep[names(rep) != "data"])
  # a where cleared: the key goes
  rows[[2L]]$where <- ""
  expect_false("where" %in% names(.data_rows_write(rep, rows)$data[[2L]]))
})

test_that("a code SPEC report: rows added and removed", {
  rep <- code_report()
  rows <- .data_rows(rep, list())
  rows[[4L]] <- .data_row("adcm_sub", "ADCM", "SAF", "adsl_sub", "CMTRT != \"\"")
  out <- .data_rows_write(rep, rows)
  expect_length(out$data, 4L)
  expect_identical(out$data[[4L]], list(id = "adcm_sub", from = "ADCM", subjects = "adsl_sub",
                                        where = "CMTRT != \"\""))
  out2 <- .data_rows_write(rep, rows[-3L])
  expect_identical(vapply(out2$data, `[[`, "", "id"), c("adsl_sub", "adae_sub", "adcm_sub"))
  # a population of its own is written; the inherited one is not
  rows[[4L]]$population <- "ITT"
  expect_identical(.data_rows_write(rep, rows)$data[[4L]]$population, "ITT")
})

test_that("a code SPEC report without adsl_sub gets it from common:, not written back", {
  rep <- code_report()
  rep$data <- rep$data[-1L]
  rows <- .data_rows(rep, list())
  expect_identical(vapply(rows, `[[`, "", "id"), c("adsl_sub", "adae_sub", "adae_den"))
  expect_identical(rows[[1L]]$source, "common")
  expect_identical(rows[[1L]]$population, "SAF")
  expect_identical(rows[[1L]]$from, "ADSL")
  expect_identical(.data_rows_write(rep, rows), rep)
})

test_that("adsl_sub is row 1 even when data: has it later; data: keeps its order", {
  rep <- code_report()
  rep$data <- rep$data[c(2L, 1L, 3L)]
  rows <- .data_rows(rep, list())
  expect_identical(rows[[1L]]$id, "adsl_sub")
  expect_identical(.data_rows_write(rep, rows), rep)
})

test_that("a row of subjects takes its subjects' population", {
  rep <- list(common = list(population = "SAF"),
              data = list(list(id = "adsl_sub", from = "ADSL", population = "ITT"),
                          list(id = "adae_sub", from = "ADAE", subjects = "adsl_sub")))
  rows <- .data_rows(rep, list())
  expect_identical(vapply(rows, `[[`, "", "population"), c("ITT", "ITT"))
  expect_identical(.data_rows_write(rep, rows), rep)
})

test_that("a report with no data: and no template has the common adsl_sub", {
  rep <- list(common = list(population = "SAF"))
  rows <- .data_rows(rep, list())
  expect_length(rows, 1L)
  expect_identical(rows[[1L]]$source, "common")
  expect_identical(.data_rows_write(rep, rows), rep)
})
