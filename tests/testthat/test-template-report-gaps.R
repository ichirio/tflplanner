# The branches of R/template_report.R the other tests leave out: the input
# a parameter gets, the lines of a list argument, the cache of a study's
# data, a report written into the spec, the timing trace and the program's
# verbs

tpl_prm <- function(...) utils::modifyList(list(name = "x"), list(...))

tpl_sample_planner <- function() {
  .spec_rebase(.planner_from_yaml(system.file("sample", "SAMPLE-01", package = "tflplanner")), list())
}

tpl_demog_output <- function(id = "T-99") {
  tflspec::tfl_output(id, "table", population = "SAF",
                      template = list(name = "demog", args = list(population = "SAF")))
}

# ---- tflspec gives nothing ------------------------------------------------------

test_that("no templates from tflspec is an empty list of them", {
  testthat::local_mocked_bindings(tfl_templates = function(...) stop("none"), .package = "tflspec")
  x <- .tpl_list("table")
  expect_identical(names(x), c("name", "kind"))
  expect_identical(nrow(x), 0L)
})

test_that("the templates of a kind nobody has are an empty list too", {
  x <- .tpl_list("no-such-kind")
  expect_identical(names(x), c("name", "kind"))
  expect_identical(nrow(x), 0L)
})

test_that("parameters tflspec gives without names are named by their own name", {
  testthat::local_mocked_bindings(
    tfl_template_params = function(name) list(list(name = "by"), list(name = "data")),
    .package = "tflspec")
  expect_identical(names(.tpl_params("whatever")), c("by", "data"))
})

test_that("a template tflspec does not know has no parameters", {
  testthat::local_mocked_bindings(tfl_template_params = function(name) stop("unknown"),
                                  .package = "tflspec")
  expect_identical(.tpl_params("whatever"), list())
})

# ---- the study's data -----------------------------------------------------------

test_that("a study's data: the data frames of its .rds files, once; the rest left out", {
  expect_identical(.tpl_data_cols(NULL), list())
  root <- tempfile("tpl-gaps-root")
  dir.create(file.path(root, "data", "adam"), recursive = TRUE)
  saveRDS(data.frame(USUBJID = "1", AGE = 50), file.path(root, "data", "adam", "adsl.rds"))
  saveRDS(data.frame(USUBJID = "1", AEDECOD = "Headache"), file.path(root, "data", "adae.RDS"))
  saveRDS(1:3, file.path(root, "data", "numbers.rds"))
  writeLines("not an rds file", file.path(root, "data", "broken.rds"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  out <- .tpl_data_cols(root)
  expect_setequal(names(out), c("ADSL", "ADAE"))
  expect_identical(names(out$ADSL), c("USUBJID", "AGE"))
  # read once: a file added later is not seen
  saveRDS(data.frame(X = 1), file.path(root, "data", "late.rds"))
  expect_setequal(names(.tpl_data_cols(root)), c("ADSL", "ADAE"))
})

# ---- the input a parameter gets -------------------------------------------------

test_that("a variable-like default is a column name in capitals", {
  expect_true(.tpl_is_varlike(c("AGE", "SEX_1")))
  expect_false(.tpl_is_varlike("age"))
  expect_false(.tpl_is_varlike(character()))
  expect_false(.tpl_is_varlike(c(a = "AGE")))
  expect_false(.tpl_is_varlike(1))
})

test_that("an argument with no declared type: guessed from its default and doc", {
  # column names: one is a var, several (or a known name) are vars
  expect_identical(.tpl_type(tpl_prm(default = c("AGE", "SEX"))), "vars")
  expect_identical(.tpl_type(tpl_prm(name = "continuous", default = "AGE")), "vars")
  expect_identical(.tpl_type(tpl_prm(default = "AGE")), "var")
  expect_identical(.tpl_type(tpl_prm(default = "AGE"), columns = c("AGE", "SEX")), "var")
  # not a column of the data: just a text
  expect_identical(.tpl_type(tpl_prm(default = "AGE"), columns = "SEX"), "text")
  # no default: the doc says whether it is a variable
  expect_identical(.tpl_type(tpl_prm(doc = "The variable to count")), "var")
  expect_identical(.tpl_type(tpl_prm(doc = "A flag column")), "var")
  expect_identical(.tpl_type(tpl_prm(doc = "Anything else")), "text")
  expect_identical(.tpl_type(tpl_prm()), "text")
  # text defaults: one a text, several chips
  expect_identical(.tpl_type(tpl_prm(default = "n (%)")), "text")
  expect_identical(.tpl_type(tpl_prm(default = c("n", "mean"))), "chips")
  # what is left (named, or several numbers): a small table
  expect_identical(.tpl_type(tpl_prm(default = c(AGE = "Age"))), "list")
  expect_identical(.tpl_type(tpl_prm(default = c(1, 2))), "list")
})

test_that("a declared type tflspec does not know is a text", {
  expect_identical(.tpl_type(tpl_prm(type = "something-new")), "text")
  expect_identical(.tpl_type(tpl_prm(type = "choice", multiple = TRUE)), "choices")
})

test_that("a levels argument with no variable to look at has no levels", {
  expect_identical(.tpl_levels_of(list(of = ""), list(), list()), NA_character_)
  expect_identical(.tpl_levels_of(list(), list(), list()), NA_character_)
  # the other argument has no value and no default
  expect_identical(.tpl_levels_of(list(of = "by"), list(by = list(name = "by")), list()),
                   NA_character_)
  d <- list(ADSL = data.frame(ARM = "A"))
  expect_identical(.tpl_levels(d, NA_character_), character())
  expect_identical(.tpl_levels(d, ""), character())
  expect_identical(.tpl_levels(d, "NOPE"), character())
})

# ---- the lines of a list argument -----------------------------------------------

test_that("a list argument's fields: its own, else name and text", {
  expect_identical(.tpl_list_fields(list(fields = c("vars", "label"))), c("vars", "label"))
  expect_identical(.tpl_list_fields(list()), c("name", "text"))
})

test_that("a list that the lines cannot hold is not written as lines", {
  named <- list(name = "labels", type = "list")
  rec <- list(name = "columns", type = "list", multiple = TRUE, fields = c("vars", "label"))
  expect_identical(.tpl_list_text(NULL, named), "")
  expect_identical(.tpl_list_text(list(), rec), "")
  # records: not a list, a record that is not one, a field the template has not
  expect_null(.tpl_list_text(c(vars = "A"), rec))
  expect_null(.tpl_list_text(list("A"), rec))
  expect_null(.tpl_list_text(list(list(zzz = "A")), rec))
  # a cell of a bar or a name, or a missing cell
  expect_null(.tpl_list_text(list(list(vars = "a|b")), rec))
  expect_null(.tpl_list_text(list(list(vars = c(a = "x"))), rec))
  expect_identical(.tpl_list_text(list(list(vars = "A")), rec), "A")
  # named values: unnamed, a bar in a name or in a value
  expect_null(.tpl_list_text(c("a", "b"), named))
  expect_null(.tpl_list_text(stats::setNames(c("a", "b"), c("x", "")), named))
  expect_null(.tpl_list_text(c("x|y" = "a"), named))
  expect_null(.tpl_list_text(c(x = "a\nb"), named))
})

test_that("the lines read back: numbers, flags, blank cells and no lines at all", {
  named <- list(name = "labels", type = "list")
  rec <- list(name = "columns", type = "list", multiple = TRUE, fields = c("vars", "label", "width"))
  expect_null(.tpl_list_parse("", named))
  expect_null(.tpl_list_parse(" \n  \n", rec))
  expect_null(.tpl_list_parse(NULL, named))
  # lines with no name are dropped, the rest kept
  expect_null(.tpl_list_parse("| text only", named))
  expect_identical(.tpl_list_parse("AGE | Age (years)\n| lost\nSEX", named),
                   c(AGE = "Age (years)", SEX = ""))
  # a text with a bar in it keeps its later cells
  expect_identical(.tpl_list_parse("A | b | c", named), c(A = "b | c"))
  r <- .tpl_list_parse("USUBJID | Subject | 12\nAETERM, AEDECOD | | 2.5\nX | Flag | TRUE", rec)
  expect_identical(r[[1L]], list(vars = "USUBJID", label = "Subject", width = 12L))
  # a blank cell is not a field; a decimal is a number; several values a vector
  expect_identical(r[[2L]], list(vars = c("AETERM", "AEDECOD"), width = 2.5))
  expect_identical(r[[3L]]$width, TRUE)
  # more cells than fields: the rest is ignored
  expect_identical(.tpl_list_parse("A | B | 1 | extra", rec)[[1L]],
                   list(vars = "A", label = "B", width = 1L))
})

# ---- what an input gives --------------------------------------------------------

test_that("a value shown on one line", {
  expect_identical(.tpl_show(NULL), "NULL")
  expect_identical(.tpl_show("n (%)"), "n (%)")
  expect_identical(.tpl_show(c("a", "b")), "[a, b]")
  expect_identical(.tpl_show(c(x = "a")), "{x: a}")
  expect_identical(.tpl_show(1:3), "[1, 2, 3]")
})

test_that("an input's value as the argument: flags, numbers, vectors, texts", {
  expect_null(.tpl_value(NULL, "text"))
  expect_true(.tpl_value(TRUE, "flag"))
  expect_false(.tpl_value(FALSE, "flag"))
  expect_identical(.tpl_value("2.5", "number"), 2.5)
  expect_null(.tpl_value("abc", "number"))
  expect_null(.tpl_value(character(), "number"))
  expect_identical(.tpl_value(c("A", " ", "B"), "vars"), c("A", "B"))
  expect_identical(.tpl_value(c("n", "mean"), "chips"), c("n", "mean"))
  expect_null(.tpl_value(c("", " "), "choices"))
  # a levels argument of one value takes the first; several (multiple) all
  expect_identical(.tpl_value(c("Y", "N"), "levels", list(multiple = FALSE)), "Y")
  expect_identical(.tpl_value(c("Y", "N"), "levels", list(multiple = TRUE)), c("Y", "N"))
  expect_identical(.tpl_value("  Title  ", "text"), "Title")
  expect_null(.tpl_value("   ", "text"))
  expect_identical(.tpl_value("AGE | Age", "list", list(name = "labels")), c(AGE = "Age"))
})

# ---- the report's data steps ----------------------------------------------------

test_that("a join that is the report's own stays; one the population no longer says is stale", {
  f <- list(dataset = "ADSL", join = TRUE, join_dataset = "ADSL", join_where = "")
  # the population has no dataset: no join wanted, but the report has one
  expect_true(.tpl_join_stale(f, list(dataset = "", where = "")))
  # no population: the join is the report's own
  expect_false(.tpl_join_stale(f, NULL))
  # no join in the report, one wanted
  g <- utils::modifyList(f, list(join = FALSE))
  expect_true(.tpl_join_stale(g, list(dataset = "ADSL", where = "SAFFL == \"Y\"")))
  expect_false(.tpl_join_stale(g, list(dataset = "", where = "")))
})

# ---- a report in the spec -------------------------------------------------------

test_that("a report's file with the template and arguments set, in the template's order", {
  skip_on_cran()
  p <- tpl_sample_planner()
  p <- .spec_add_output(p, tpl_demog_output(), population = "SAF")
  o <- .tpl_output(p, "T-99", list(zzz = 1, population = "FAS"), name = "disposition")
  expect_identical(o$template$name, "disposition")
  expect_identical(names(o$template$args), c("population", "zzz"))
  expect_identical(o$population, "FAS")
  # the name kept when none is given
  expect_identical(.tpl_output(p, "T-99", list())$template$name, "demog")
})

test_that("what tflspec filled from the study: nothing for a report that is not there", {
  skip_on_cran()
  p <- tpl_sample_planner()
  expect_null(.tpl_filled(p, "T-NOPE"))
})

test_that("a report added with a section, and one added twice is refused", {
  skip_on_cran()
  p <- tpl_sample_planner()
  q <- .spec_add_output(p, tpl_demog_output(), label = "Demographics", section = "Safety",
                        population = "SAF")
  entry <- Filter(function(e) identical(e$id, "T-99"), q$spec$study$toc$outputs)[[1L]]
  expect_identical(entry$section, "Safety")
  expect_identical(entry$label, "Demographics")
  expect_identical(entry$population, "SAF")
  expect_identical(.tpl_of(q, "T-99")$name, "demog")
  expect_error(.spec_add_output(q, tpl_demog_output()), "already on the list")
})

test_that("a population said again, or none where there was none, changes nothing", {
  skip_on_cran()
  p <- .spec_add_output(tpl_sample_planner(), tpl_demog_output(), population = "SAF")
  expect_identical(.spec_set_toc_population(p, "T-99", "SAF"), p)
  q <- .spec_add_output(tpl_sample_planner(), tpl_demog_output("T-98"))
  expect_identical(.spec_set_toc_population(q, "T-98", NA_character_), q)
})

# ---- the timing trace -----------------------------------------------------------

test_that("the trace says each step with its time, only when asked", {
  old <- options(tflplanner.tpl_trace = TRUE, tflplanner.tpl_slow = 0)
  on.exit(options(old), add = TRUE)
  expect_message(.tpl_trace("asked", start = TRUE), "\\[tpl\\] +[0-9.]+ s  asked")
  expect_message(.tpl_trace("drawn"), "\\[tpl\\] +[0-9.]+ s  drawn")
  options(tflplanner.tpl_trace = NULL)
  expect_no_message(.tpl_trace("quiet"))
})

test_that("a slow machine is made: each drawing step waits, the first call does not", {
  old <- options(tflplanner.tpl_trace = NULL, tflplanner.tpl_slow = 0.2)
  on.exit(options(old), add = TRUE)
  expect_lt(system.time(.tpl_trace("asked", start = TRUE))[["elapsed"]], 0.15)
  expect_gte(system.time(.tpl_trace("drawn"))[["elapsed"]], 0.15)
})

# ---- the program's verbs and call -----------------------------------------------

test_that("the verbs after a template's call: the ones its program has", {
  skip_on_cran()
  p <- .spec_add_output(tpl_sample_planner(), tpl_demog_output(), population = "SAF")
  # a report that is not there, or is not a template report: none
  expect_identical(.tpl_code_fns(p, "T-NOPE"), character())
  code_id <- names(Filter(function(o) is.null(o$template), p$spec$outputs))[1L]
  skip_if(is.na(code_id), "the sample study has no code report")
  expect_identical(.tpl_code_fns(p, code_id), character())
})

test_that("the verbs after a template's call in the program tflspec writes now", {
  skip_on_cran()
  p <- .spec_add_output(tpl_sample_planner(), tpl_demog_output(), population = "SAF")
  fns <- .tpl_code_fns(p, "T-99")
  expect_true(all(c("plan_cells", "plan_digits") %in% fns))
})

test_that("a program with no template call, or that tflspec cannot make, has no verbs", {
  skip_on_cran()
  p <- .spec_add_output(tpl_sample_planner(), tpl_demog_output(), population = "SAF")
  testthat::local_mocked_bindings(tfl_content_code = function(...) c("plan_cells(x)", "plan_labels(y)"),
                                  .package = "tflspec")
  expect_identical(.tpl_code_fns(p, "T-99"), character())
  testthat::local_mocked_bindings(
    tfl_content_code = function(...) c("tpl_demog(x)", "  plan_cells(x)", "not a verb", "plan_labels(y)"),
    .package = "tflspec")
  expect_identical(.tpl_code_fns(p, "T-99"), c("plan_cells", "plan_labels"))
  testthat::local_mocked_bindings(tfl_content_code = function(...) stop("cannot"),
                                  .package = "tflspec")
  expect_identical(.tpl_code_fns(p, "T-99"), character())
})

test_that("a template's name in a report's file, and its call in the program", {
  expect_identical(.tpl_spec_name(list(template = list(name = "demog"))), "demog")
  expect_null(.tpl_spec_name(list(template = list(args = list()))))
  expect_null(.tpl_spec_name(list(template = "km_simple")))
  expect_null(.tpl_spec_name(list()))
  expect_identical(.tpl_call_name("demog"), "tfl_tpl()")
  expect_identical(.tpl_call_name("no_such_template"), "tfl_template(no_such_template)")
})
