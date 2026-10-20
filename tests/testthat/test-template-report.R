# Template SPEC reports (#440): the inputs of a template's arguments, and
# the builder's card rules

test_that("an argument's input follows the type tflspec declares", {
  p <- function(...) utils::modifyList(list(name = "x", default = NULL, required = FALSE), list(...))
  expect_identical(.tpl_type(p(type = "vars", multiple = TRUE)), "vars")
  expect_identical(.tpl_type(p(type = "vars", multiple = FALSE)), "var")
  expect_identical(.tpl_type(p(type = "levels", of = "by")), "levels")
  expect_identical(.tpl_type(p(type = "flag")), "flag")
  expect_identical(.tpl_type(p(type = "text", multiple = TRUE)), "chips")
  expect_identical(.tpl_type(p(type = "choice", choices = c("a", "b"))), "choice")
  expect_identical(.tpl_type(p(type = "list", fields = c("name", "text"))), "list")
  # population and data: the study's
  expect_identical(.tpl_type(p(name = "population", type = "text")), "population")
  expect_identical(.tpl_type(p(name = "data", type = "text")), "data")
  # no type declared: guessed from the default
  expect_identical(.tpl_type(p(default = TRUE)), "flag")
  expect_identical(.tpl_type(p(default = 2)), "number")
})

test_that("a levels argument's variable is another argument's value, or a name", {
  params <- list(by = list(name = "by", default = "TRT01A"))
  expect_identical(.tpl_levels_of(list(of = "by"), params, list()), "TRT01A")
  expect_identical(.tpl_levels_of(list(of = "by"), params, list(by = "ARM")), "ARM")
  expect_identical(.tpl_levels_of(list(of = "PARAMCD"), params, list()), "PARAMCD")
  d <- list(ADSL = data.frame(TRT01A = c("B", "A", "B")))
  expect_identical(.tpl_levels(d, "TRT01A"), c("A", "B"))
})

test_that("a list argument reads and writes as lines of a small table", {
  named <- list(name = "labels", type = "list", fields = c("name", "text"))
  x <- c(AGE = "Age (years)", SEX = "Sex, n (%)")
  txt <- .tpl_list_text(x, named)
  expect_identical(txt, "AGE | Age (years)\nSEX | Sex, n (%)")
  expect_identical(.tpl_list_parse(txt, named), x)
  rec <- list(name = "columns", type = "list", multiple = TRUE,
              fields = c("vars", "label", "width"))
  v <- list(list(vars = "USUBJID", label = "Subject", width = 12),
            list(vars = c("AETERM", "AEDECOD"), label = "Term"))
  txt <- .tpl_list_text(v, rec)
  expect_identical(txt, "USUBJID | Subject | 12\nAETERM, AEDECOD | Term")
  expect_identical(.tpl_list_parse(txt, rec), v)
  # nested deeper: read only (NULL)
  expect_null(.tpl_list_text(list(a = list(b = 1)), named))
})

test_that("an argument's origin: the report's, the study standard's, the default", {
  study <- list(defaults = list(content = list(
    list(fn = "plan_digits", args = list(rounding = "sas")))))
  prm <- list(name = "rounding", study = "plan_digits.rounding")
  expect_identical(.tpl_arg_origin(study, prm, list())$from, "study")
  expect_identical(.tpl_arg_origin(study, prm, list(rounding = "r"))$from, "own")
  expect_identical(.tpl_arg_origin(study, list(name = "x"), list())$from, "default")
})

test_that("the cards: required + the items in effect + added; no switches", {
  it <- function(fn, from = "own", below = NULL, revert = FALSE) {
    list(fn = fn, args = list(), from = from, path = fn, below = below, revert = revert,
         present = TRUE)
  }
  items <- list(it("normalize_ard", "defaults"), it("table_plan"), it("plan_cells", "defaults"),
                it("plan_stub", "defaults"), it("plan_filter"))
  cards <- .builder_items_cards(items)
  fns <- vapply(cards, function(x) x$fn, "")
  expect_setequal(fns, c("normalize_ard", "table_plan", "plan_cells", "plan_stub", "plan_filter"))
  expect_false(any(vapply(cards, function(x) isTRUE(x$switch), NA)))
  rm <- stats::setNames(vapply(cards, function(x) isTRUE(x$removable), NA), fns)
  # only the report's own item can be removed; the study standard's cannot
  expect_true(rm[["plan_filter"]])
  expect_false(rm[["plan_stub"]])
  expect_false(rm[["table_plan"]])
  # a new report not saved: the usual ones too, empty (not written)
  fresh <- .builder_items_cards(items, fresh = TRUE)
  fns2 <- vapply(fresh, function(x) x$fn, "")
  expect_true(all(.plan_tier_fns("usual") %in% fns2))
  expect_false(any(vapply(fresh[fns2 == "plan_levels"], function(x) isTRUE(x$present), NA)))
})

test_that("a template report shows only the cards it overrides or adds", {
  it <- function(fn, from, below = NULL) {
    list(fn = fn, args = list(), from = from, path = fn, below = below,
         revert = identical(from, "own") && !is.null(below), present = TRUE)
  }
  items <- list(it("table_plan", "template:demog"), it("normalize_ard", "template:demog"),
                it("plan_cells", "defaults"), it("plan_sort", "own", below = list()),
                it("plan_labels", "own"))
  cards <- .builder_items_cards(items, template = TRUE)
  expect_identical(vapply(cards, function(x) x$fn, ""), c("plan_sort", "plan_labels"))
  # no required card in a template report
  expect_false(any(vapply(cards, function(x) "required" %in% names(x) && isTRUE(x$required), NA)))
  # + Add a card: the template's pipe first, never its call's own verbs
  menu <- .plan_add_menu(cards, made = c("table_plan", "normalize_ard", "plan_cells", "plan_sort"),
                         template = TRUE)
  fns <- vapply(menu, function(m) m$fn, "")
  expect_false(any(c("normalize_ard", "table_plan") %in% fns))
  expect_identical(menu[[1L]]$group, "made")
  expect_true("plan_cells" %in% fns)
  expect_false(menu[[which(fns == "plan_sort")]]$ok)
})

test_that("a template report: one card a line after its call -- the study standard's too", {
  it <- function(fn, from, arg_from = from) {
    list(fn = fn, args = list(), from = from, arg_from = list(x = arg_from), path = fn,
         below = list(), revert = FALSE, present = TRUE)
  }
  items <- list(it("table_plan", "template:demog"), it("plan_cells", "template:demog",
                                                       c("defaults", "template:demog")),
                it("plan_labels", "template:demog"), it("plan_digits", "defaults"))
  cards <- .builder_items_cards(items, template = TRUE, code_fns = c("plan_cells", "plan_digits"))
  expect_identical(vapply(cards, function(x) x$fn, ""), c("plan_cells", "plan_digits"))
  # the study standard's mark, and no Remove
  expect_true(all(vapply(cards, function(x) identical(x$from, "defaults"), NA)))
  expect_false(any(vapply(cards, function(x) isTRUE(x$removable), NA)))
})

test_that("a study standard's card edited in a template report: the report's own, in its YAML", {
  p <- .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
  o <- tflspec::tfl_output("T-99", "table", population = "SAF",
                           template = list(name = "demog", args = list(population = "SAF")))
  p <- .spec_add_output(p, o, population = "SAF")
  it <- Filter(function(x) identical(x$fn, "plan_stub"), builder_items(p, "T-99"))[[1L]]
  expect_identical(it$from, "defaults")
  q <- .builder_item_write(p, "T-99", "plan_stub", utils::modifyList(it$args, list(indent = 3L)))
  own <- q$spec$outputs[["T-99"]]$content
  expect_true("plan_stub" %in% vapply(own, function(x) x$fn, ""))
  expect_identical(q$spec$outputs[["T-99"]]$template$name, "demog")
  it2 <- Filter(function(x) identical(x$fn, "plan_stub"), builder_items(q, "T-99"))[[1L]]
  expect_identical(it2$from, "own")
  expect_true(any(grepl("indent: 3", unlist(q$spec$text), fixed = TRUE)))
  # the file written for it: the template, its arguments and the card
  txt <- paste(q$spec$text[[q$spec$where[["T-99"]]]], collapse = "\n")
  expect_match(txt, "template:", fixed = TRUE)
  expect_match(txt, "plan_stub", fixed = TRUE)
})
