# The card model (R/card_model.R): a template step's rows and the write-back
# into the report's `template: args:`.  demog, tflspec's installed template.

row_names <- function(m) vapply(m$rows, `[[`, "", "name")
row_of <- function(m, n) m$rows[[match(n, row_names(m))]]
row_values <- function(m) stats::setNames(lapply(m$rows, `[[`, "value"), row_names(m))

# demog's arguments of a step: the CSV's, in definition order, no population
# and no hidden one
demog_step <- function(step) {
  prm <- tflspec::tfl_template_params("demog")
  tab <- utils::read.csv(system.file("extdata", "template_arg_sections.csv", package = "tflplanner"),
                         stringsAsFactors = FALSE)
  tab <- tab[tab$template == "demog" & tab$section == step, ]
  keep <- vapply(prm, function(p) !isTRUE(p$hidden), NA)
  setdiff(intersect(names(prm)[keep], tab$argument), "population")
}

test_that("every step of demog: its arguments, in definition order", {
  prm <- tflspec::tfl_template_params("demog")
  seen <- character()
  for (s in c("data", "ard", "content", "page")) {
    m <- .card_model("demog", list(), s)
    expect_identical(m$template, "demog")
    expect_identical(m$step, s)
    expect_null(names(m$rows))
    expect_identical(row_names(m), demog_step(s))
    expect_identical(row_names(m), intersect(names(prm), row_names(m)))
    expect_false("population" %in% row_names(m))
    seen <- c(seen, row_names(m))
  }
  expect_identical(anyDuplicated(seen), 0L)
  expect_identical(row_names(.card_model("demog", list(), "data")), c("subset", "data"))
  expect_true(all(c("continuous", "categorical", "by", "total") %in% row_names(.card_model("demog", NULL, "ard"))))
  expect_length(.card_model("demog", list(), "page")$rows, 0L)
  expect_error(.card_model("demog", list(), "layout"))
})

test_that("a row's fields: type, label, help, value, default, required", {
  prm <- tflspec::tfl_template_params("demog")
  m <- .card_model("demog", list(by = "TRT01A", total = TRUE), "ard")
  expect_named(m$rows[[1L]], c("name", "type", "label", "help", "value", "default", "from",
                               "inherited", "required", "key", "locked", "group"))
  for (r in m$rows) {
    p <- prm[[r$name]]
    expect_identical(r$type, .tpl_field_type(p)$type)
    expect_identical(r$default, p$default)
    expect_identical(r$required, isTRUE(p$required))
    expect_identical(r$key, identical(p$tier, "key"))
    expect_identical(r$help, p$doc %||% "")
    expect_identical(r$from, "template")
    expect_false(r$locked)
  }
  expect_identical(row_of(m, "by")$value, "TRT01A")
  expect_true(row_of(m, "by")$required)
  expect_identical(row_of(m, "by")$type, "column")
  expect_identical(row_of(m, "continuous")$type, "list<column>")
  expect_identical(row_of(m, "total")$type, "logical")
  expect_identical(row_of(m, "total")$value, TRUE)
  expect_false(row_of(m, "total")$default)
  expect_null(row_of(m, "continuous")$value)
  expect_identical(row_of(m, "by")$label, "Variable that splits the columns")
  expect_identical(row_of(.card_model("demog", list(), "content"), "max_rows")$label, "Rows per page")
})

test_that("group: top when required, key or set; else bottom", {
  m <- .card_model("demog", list(total = FALSE, formats = list(AGE = 1L)), "ard")
  g <- stats::setNames(vapply(m$rows, `[[`, "", "group"), row_names(m))
  expect_identical(g[["by"]], "top")             # required
  expect_identical(g[["total"]], "top")          # set (to its default value)
  expect_identical(g[["formats"]], "top")        # set
  expect_identical(g[["continuous"]], "bottom")  # not required, not set
  expect_identical(g[["missing_row"]], "bottom")
  expect_true(all(g[!names(g) %in% c("by", "total", "formats")] == "bottom"))
})

test_that("key comes from tier; a locked row keeps the group rule", {
  real <- tflspec::tfl_template_params
  local_mocked_bindings(tfl_template_params = function(name) {
    p <- real(name)
    p$total$tier <- "key"
    p$missing_row$fixed_value <- TRUE
    p$missing_label$fixed_value <- TRUE
    p$missing_label$tier <- "key"
    p
  }, .package = "tflspec")
  m <- .card_model("demog", list(), "ard")
  expect_true(row_of(m, "total")$key)
  expect_identical(row_of(m, "total")$group, "top")
  expect_false(row_of(m, "by")$key)
  expect_true(row_of(m, "missing_row")$locked)
  expect_identical(row_of(m, "missing_row")$group, "bottom")
  expect_true(row_of(m, "missing_label")$locked)
  expect_identical(row_of(m, "missing_label")$group, "top")
  m2 <- .card_model("demog", list(missing_row = TRUE), "ard")
  expect_true(row_of(m2, "missing_row")$locked)
  expect_identical(row_of(m2, "missing_row")$group, "top")
})

test_that("hidden arguments are no rows (demog's denominator, split)", {
  for (s in c("data", "ard", "content", "page")) {
    expect_false(any(c("population", "denominator", "split") %in% row_names(.card_model("demog", list(), s))))
  }
})

test_that("names that are no template argument: no row, in attr unknown", {
  args <- list(by = "TRT01A", colour = "red", population = "SAF", zzz = 1)
  m <- .card_model("demog", args, "ard")
  expect_identical(attr(m, "unknown"), c("colour", "zzz"))
  expect_false(any(c("colour", "zzz") %in% row_names(m)))
  expect_identical(attr(.card_model("demog", list(), "ard"), "unknown"), character())
})

test_that("defaults from the page settings (config) and the study settings", {
  ctx <- list(config = list(max_rows = 18L),
              study = list(settings = list(sd_missing = "NE"),
                           plan_stub = list(name = "Characteristic")))
  m <- .card_model("demog", list(), "content", ctx)
  r <- row_of(m, "max_rows")
  expect_identical(r$from, "config")
  expect_identical(r$inherited, 18L)
  expect_identical(r$default, 30L)
  expect_identical(r$group, "bottom")
  expect_identical(row_of(m, "sd_missing")$from, "study")
  expect_identical(row_of(m, "sd_missing")$inherited, "NE")
  expect_identical(row_of(m, "stub_name")$from, "study")
  expect_identical(row_of(m, "total_label")$from, "template")
  expect_null(row_of(m, "total_label")$inherited)
  # a flat study settings list: settings.<name> is its <name>
  m2 <- .card_model("demog", list(), "content", list(study = list(sd_missing = "-")))
  expect_identical(row_of(m2, "sd_missing")$from, "study")
  # the page settings before the study's
  m3 <- .card_model("demog", list(), "content",
                    list(config = list(max_rows = 20L), study = list(plan_paginate_rows = list(max_rows = 40L))))
  expect_identical(row_of(m3, "max_rows")$inherited, 20L)
  # the report's own value is the value; the source of the default stays
  m4 <- .card_model("demog", list(max_rows = 12L), "content", ctx)
  expect_identical(row_of(m4, "max_rows")$value, 12L)
  expect_identical(row_of(m4, "max_rows")$group, "top")
})

test_that("write-back: one step's names replaced, NULL dropped, others kept, definition order", {
  # (demog's definition order: continuous, categorical, by, subset, data, ...)
  args <- list(by = "TRT01A", subset = "AGE >= 18", total = TRUE, max_rows = 20L, colour = "red")
  out <- .card_args_write(args, "ard", "demog",
                          list(continuous = c("AGE", "WEIGHTBL"), total = NULL, by = "TRT01P"))
  expect_identical(out, list(continuous = c("AGE", "WEIGHTBL"), by = "TRT01P",
                             subset = "AGE >= 18", max_rows = 20L, colour = "red"))
  # only the step's own names; an empty values list keeps args as they are
  expect_identical(.card_args_write(args, "data", "demog", list()), args)
  out2 <- .card_args_write(args, "data", "demog", list(subset = NULL, data = "ADSL"))
  expect_identical(out2, list(by = "TRT01A", data = "ADSL", total = TRUE, max_rows = 20L, colour = "red"))
  # names out of definition order come back in it
  expect_identical(.card_args_write(list(colour = "red", total = TRUE, by = "X"), "page", "demog", list()),
                   list(by = "X", total = TRUE, colour = "red"))
  expect_error(.card_args_write(args, "data", "demog", list(by = "X")), "Not an argument")
  # from empty
  expect_identical(.card_args_write(list(), "content", "demog", list(max_rows = 25L, total_label = NULL)),
                   list(max_rows = 25L))
  # a report's population (no row) is kept
  expect_identical(.card_args_write(list(population = "SAF"), "data", "demog", list(data = "ADSL")),
                   list(population = "SAF", data = "ADSL"))
})

test_that("round trip: each row's value written back gives args unchanged", {
  args <- list(continuous = "AGE", by = "TRT01A", subset = "SEX == 'F'", data = "ADSL",
               formats = list(AGE = 1L), total = TRUE, max_rows = 18L, style = "tfl_standard",
               colour = "red")
  for (s in c("data", "ard", "content", "page")) {
    m <- .card_model("demog", args, s, list(config = list(max_rows = 30L)))
    expect_identical(.card_args_write(args, s, "demog", row_values(m)), args)
  }
  expect_identical(.card_args_write(list(), "ard", "demog", row_values(.card_model("demog", list(), "ard"))),
                   list())
})
