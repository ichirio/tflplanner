# The cards' argument definitions from tflspec's tfl_arg_defs() (R/arg_defs.R):
# the schema with its defs, each input form's field, the choices of the 14
# source words, and the fallbacks without defs (no tfl_arg_defs(), a
# template of no series file).

html <- function(...) as.character(htmltools::tagList(...))

# a tfl_arg_defs() table of the rows given (each a list of its fields)
mk_defs <- function(...) {
  rows <- list(...)
  get <- function(r, k, d) if (is.null(r[[k]])) d else r[[k]]
  d <- data.frame(name = vapply(rows, function(r) r$name, ""), stringsAsFactors = FALSE)
  d$label_key <- paste0("x.", d$name)
  d$step <- vapply(rows, get, "", k = "step", d = "content")
  d$input <- vapply(rows, get, "", k = "input", d = "text")
  d$type <- vapply(rows, get, "", k = "type", d = "string")
  d$required <- vapply(rows, get, NA, k = "required", d = FALSE)
  d$default <- I(lapply(rows, function(r) r$default))
  d$default_kind <- vapply(rows, get, "", k = "default_kind", d = NA_character_)
  d$choices <- I(lapply(rows, function(r) as.character(r$choices)))
  d$values <- I(lapply(rows, function(r) r$values))
  d$free <- vapply(rows, get, NA, k = "free", d = NA)
  d$ordered <- vapply(rows, get, NA, k = "ordered", d = NA)
  d$columns <- I(lapply(rows, function(r) r$columns))
  d$tier <- vapply(rows, get, "", k = "tier", d = "usual")
  d$description <- vapply(rows, get, "", k = "description", d = "")
  d
}

# the defs cache forgotten before and after a test
local_defs_reset <- function(env = parent.frame()) {
  .arg_defs_reset()
  do.call(on.exit, list(quote(.arg_defs_reset()), add = TRUE), envir = env)
}

fake_schema <- list(label = "A function", rank = 7, stage = "content",
                    args = list(cols = list(label = "Columns kept", help = "The columns.",
                                            type = "list<column>"),
                                note = list(label = "Note", type = "string"),
                                how = list(label = "How", type = "enum[a|b]")))

test_that("the schema's arguments gain their defs; label, help and the function's fields stay", {
  local_defs_reset()
  testthat::local_mocked_bindings(
    tfl_schema = function(fn = NULL) if (identical(fn, "plan_x")) fake_schema,
    tfl_arg_defs = function(fn = NULL, template = NULL) mk_defs(
      list(name = "cols", input = "pick", type = "list<column>", choices = "vars(data)",
           ordered = TRUE, free = FALSE, description = "Def's cols"),
      list(name = "note", input = "text", description = "Def's note", tier = "other"),
      list(name = "how", input = "pick", type = "enum[a|b]", choices = "fixed",
           values = c("a", "b"), default = "a", default_kind = "value", required = TRUE)),
    .package = "tflspec")
  s <- .schema_of("plan_x")
  expect_identical(s$label, "A function")
  expect_identical(s$rank, 7)
  expect_identical(s$stage, "content")
  expect_identical(s$args$cols$label, "Columns kept")
  expect_identical(s$args$cols$help, "The columns.")
  expect_identical(s$args$cols$input, "pick")
  expect_identical(s$args$cols$choices, "vars(data)")
  expect_true(s$args$cols$ordered)
  expect_false(s$args$cols$free)
  expect_identical(s$args$cols$step, "content")
  # (the schema has no help: the def's description)
  expect_identical(s$args$note$help, "Def's note")
  expect_identical(s$args$note$tier, "other")
  expect_identical(s$args$how$values, c("a", "b"))
  expect_identical(s$args$how$default, "a")
  expect_identical(s$args$how$default_kind, "value")
  expect_true(s$args$how$required)
  # (one read a session)
  n <- 0L
  testthat::local_mocked_bindings(
    tfl_arg_defs = function(fn = NULL, template = NULL) {
      n <<- n + 1L
      mk_defs(list(name = "note"))
    },
    .package = "tflspec")
  .arg_defs_reset()
  .arg_defs(fn = "plan_x")
  .arg_defs(fn = "plan_x")
  expect_identical(n, 1L)
  expect_identical(.arg_def(.arg_defs(fn = "plan_x"), "note")$input, "text")
  expect_null(.arg_def(.arg_defs(fn = "plan_x"), "nothing"))
})

test_that("without defs the schema is tflspec's as it is", {
  local_defs_reset()
  testthat::local_mocked_bindings(
    tfl_schema = function(fn = NULL) fake_schema,
    tfl_arg_defs = function(fn = NULL, template = NULL) stop("no such function"),
    .package = "tflspec")
  expect_null(.arg_defs(fn = "plan_x"))
  expect_identical(.schema_of("plan_x"), fake_schema)
  # a tflspec with no tfl_arg_defs()
  .arg_defs_reset()
  testthat::local_mocked_bindings(tfl_arg_defs = NULL, .package = "tflspec")
  expect_null(.arg_defs(fn = "plan_x"))
  expect_identical(.schema_of("plan_x"), fake_schema)
  expect_null(.arg_defs())
  expect_null(.arg_defs(fn = "a", template = "s/t"))
})

test_that("each input form draws its field", {
  # pick of an enum / fixed values: the select of them
  d <- list(input = "pick", choices = "fixed", values = c("cells", "rows"), free = FALSE)
  h <- html(arg_field("c", "stats", "enum[cells|rows]", def = d, default = "cells"))
  expect_match(h, "<select", fixed = TRUE)
  expect_match(h, "rp-field-enum", fixed = TRUE)
  expect_match(h, "cells (default)", fixed = TRUE)
  # (fixed values of a string type)
  h <- html(arg_field("c", "type", "string", def = list(input = "pick", choices = "fixed",
                                                      values = c("ard", "model"))))
  expect_match(h, "<option value=\"model\">model</option>", fixed = TRUE)
  expect_false(grepl("selectize", h, fixed = TRUE))
  # free: a selectize that takes a typed value
  h <- html(arg_field("c", "method", "string",
                      def = list(input = "pick", choices = "fixed", values = c("continuous", "categorical"),
                                 free = TRUE)))
  expect_match(h, "\"create\":true", fixed = TRUE)
  expect_match(h, "categorical", fixed = TRUE)
  # pick of a list: several, the order hint when ordered
  d <- list(input = "pick", choices = "vars(data)", ordered = TRUE, free = FALSE)
  h <- html(arg_field("c", "keep", "list<column>", def = d,
                      ctx = list(vars = list(adsl = c("AGE", "SEX")), args = list(data = "adsl"))))
  expect_match(h, "multiple=\"multiple\"", fixed = TRUE)
  expect_match(h, "rp-field-list", fixed = TRUE)
  expect_match(h, "<option value=\"AGE\">AGE</option>", fixed = TRUE)
  expect_match(h, "In order, top to bottom", fixed = TRUE)
  expect_false(grepl("\"create\":true", h, fixed = TRUE))
  d$ordered <- FALSE
  expect_false(grepl("top to bottom", html(arg_field("c", "keep", "list<column>", def = d)), fixed = TRUE))
  # (the order hint in Japanese)
  h <- html(arg_field("c", "keep", "list<column>", def = list(input = "pick", ordered = TRUE), lang = "ja"))
  expect_match(h, "上から順に", fixed = TRUE)
  # the caller's choices before the def's
  h <- html(arg_field("c", "keep", "list<column>", choices = c("X1", "X2"), def = d,
                      ctx = list(vars = c("AGE"))))
  expect_match(h, "X1", fixed = TRUE)
  expect_false(grepl("AGE", h, fixed = TRUE))
  # pick of one from sources: a select of the groups
  h <- html(arg_field("c", "from", "string", def = list(input = "pick", choices = c("report_data", "study_data")),
                      ctx = list(report_data = "adsl_sub", study_data = c("ADSL", "ADAE"))))
  expect_match(h, "<optgroup label=\"This report&#39;s data\">", fixed = TRUE)
  expect_match(h, "<optgroup label=\"Study data\">", fixed = TRUE)
  expect_match(h, "rp-field-column", fixed = TRUE)
  # (a pick of text with nothing to choose from stays a text field)
  h <- html(arg_field("c", "pop", "string", def = list(input = "pick", choices = "populations")))
  expect_match(h, "type=\"text\"", fixed = TRUE)
  expect_match(h, "rp-field-string", fixed = TRUE)
  # a logical pick: the switch; a number: the number field (the type decides)
  expect_match(html(arg_field("c", "notes", "logical", def = list(input = "pick"))), "form-switch", fixed = TRUE)
  expect_match(html(arg_field("c", "n", "integer", def = list(input = "text"))), "type=\"number\"", fixed = TRUE)
  # condition: the R field, the columns offered as it is typed
  h <- html(arg_field("c", "where", "r", def = list(input = "condition", choices = "vars(from)"),
                      ctx = list(vars = c("AGE", "SAFFL"))))
  expect_match(h, "rp-condition", fixed = TRUE)
  expect_match(h, "rp-field-condition", fixed = TRUE)
  expect_match(h, "<option value=\"SAFFL\"></option>", fixed = TRUE)
  expect_match(h, "rp-r-fixed", fixed = TRUE)
  # table: today's field of its type (P1b builds the table editor)
  h <- html(arg_field("c", "derive", "map<column,r>", def = list(input = "table", choices = "vars(from)")))
  expect_identical(h, html(arg_field("c", "derive", "map<column,r>")))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = list(input = "table")))
  expect_identical(h, html(arg_field("c", "conditions", "rows<map<string,string|r>>")))
  # text: as the type says, the same as without a def
  for (ty in c("string", "number", "r", "template", "list<string>")) {
    expect_identical(html(arg_field("c", "a", ty, value = NULL, def = list(input = "text"))),
                     html(arg_field("c", "a", ty)), info = ty)
  }
})

test_that("without a def a field is today's", {
  for (ty in c("string", "list<column>", "enum[a|b]", "logical", "map<aes, column|r>", "r")) {
    expect_identical(html(arg_field("c", "a", ty, def = NULL, ctx = list(vars = "X"))),
                     html(arg_field("c", "a", ty)), info = ty)
    # (a def with no input: none)
    expect_identical(html(arg_field("c", "a", ty, def = list(type = ty))),
                     html(arg_field("c", "a", ty)), info = ty)
  }
})

test_that("the 14 source words give their choices", {
  ctx <- list(
    vars = list(adsl = c("AGE", "SEX"), adae = c("AEDECOD")),
    levels = list(SEX = c(F = "Female", M = "Male")),
    codelist = list(NY = c("N", "Y")),
    columns = function(obj) if (identical(obj, "ard")) c("group1", "stat_name"),
    populations = c(SAF = "Safety", ITT = "ITT"),
    report_data = c("adsl_sub"),
    study_data = c("ADSL", "ADAE"),
    ard = c("demog_ard"),
    study_info = c("STUDYID", "PROTOCOL"),
    tokens = c("{N}", "{PAGE}"),
    stats = function() c("n", "mean"),
    styles = c("bold", "italic"),
    args = list(data = "adsl", var = "SEX", variables = c("AGE", "SEX"), by = "TRT01A"))
  v <- function(w, ...) .choice_values(w, ctx, ...)
  expect_identical(unname(v("vars(data)")), c("AGE", "SEX"))
  expect_identical(unname(v("vars(adae)")), "AEDECOD")
  # (a word's name read through the card's arguments: var is SEX)
  expect_identical(v("levels(var)"), v("levels(SEX)"))
  expect_identical(unname(v("levels(SEX)")), c("Female", "Male"))
  expect_identical(names(v("levels(SEX)")), c("F", "M"))
  expect_identical(unname(v("codelist(NY)")), c("N", "Y"))
  expect_identical(unname(v("columns(ard)")), c("group1", "stat_name"))
  expect_identical(unname(v("populations")), c("Safety", "ITT"))
  expect_identical(names(v("populations")), c("SAF", "ITT"))
  expect_identical(unname(v("report_data")), "adsl_sub")
  expect_identical(unname(v("study_data")), c("ADSL", "ADAE"))
  expect_identical(unname(v("ard")), "demog_ard")
  expect_identical(unname(v("study_info")), c("STUDYID", "PROTOCOL"))
  expect_identical(unname(v("tokens")), c("{N}", "{PAGE}"))
  expect_identical(unname(v("stats")), c("n", "mean"))
  expect_identical(unname(v("styles")), c("bold", "italic"))
  expect_identical(unname(v("fixed", values = c("x", "y"))), c("x", "y"))
  expect_identical(unname(v("from_args(variables, by)")), c("AGE", "SEX", "TRT01A"))
  expect_identical(unname(v("from_args")), c("data", "var", "variables", "by"))
  expect_identical(unname(.choice_values("from_args", list(arg_names = c("a", "b")))), c("a", "b"))
  # several: the groups in the words' order, each named
  g <- v("report_data, study_data")
  expect_identical(names(g), c("This report's data", "Study data"))
  expect_identical(unlist(g[[2L]]), c(ADSL = "ADSL", ADAE = "ADAE"))
  g <- v(c("study_data", "report_data"), lang = "ja")
  expect_identical(names(g)[2L], "この帳票のデータ")
  # (one of several gives nothing: the other alone, no group)
  expect_identical(unname(.choice_values(c("report_data", "ard"), list(ard = "a1"))), "a1")
  # a source the ctx cannot answer: nothing
  expect_identical(.choice_values("vars(data)", list()), character())
  expect_identical(.choice_values("populations", NULL), character())
  expect_identical(.choice_values(character(), ctx), character())
  expect_identical(.choice_values("fixed", ctx), character())
  # an unknown word stops
  expect_error(.choice_values("datasets", ctx), "Unknown choice source \"datasets\"")
  expect_error(.choice_values("vars(data), colours", ctx), "colours")
})

test_that("a template of no series file: today's params and sections", {
  local_defs_reset()
  testthat::local_mocked_bindings(
    tfl_arg_defs = function(fn = NULL, template = NULL) stop("template: no file"),
    .package = "tflspec")
  old <- tryCatch(tflspec::tfl_template_params("demog"), error = function(e) list())
  p <- .tpl_params("demog")
  expect_identical(names(p), unname(vapply(old, function(x) x$name, "")))
  expect_identical(.tpl_arg_section("demog", "population"), "data")
  expect_identical(.tpl_arg_section("demog", "title"), "page")
  expect_null(.tpl_defs("demog"))
  expect_null(.tpl_defs("s1/demog"))
  expect_identical(.tpl_params("s1/none"), list())
})

test_that("a series template's params and sections come from its defs", {
  local_defs_reset()
  testthat::local_mocked_bindings(
    tfl_arg_defs = function(fn = NULL, template = NULL) {
      if (!identical(template, "s1/t1")) stop("no file")
      mk_defs(
        list(name = "data", step = "data", input = "pick", choices = "study_data", required = TRUE,
             tier = "required"),
        list(name = "by", step = "analysis", input = "pick", type = "list<column>",
             choices = "vars(data)", ordered = TRUE,
             default = structure(list(value = "settings.treatment"), class = "tfl_tagged", tag = "study"),
             default_kind = "study"),
        list(name = "cells", step = "content", input = "pick", type = "string", choices = "fixed",
             values = c("n", "pct"), default = "n", default_kind = "value"),
        list(name = "engine", step = "analysis", default = "cards", default_kind = "fixed"),
        list(name = "title", step = "page", description = "The title."))
    },
    .package = "tflspec")
  p <- .tpl_params("s1/t1")
  expect_identical(names(p), c("data", "by", "cells", "engine", "title"))
  expect_true(p$data$required)
  expect_identical(p$data$step, "data")
  expect_identical(p$by$type, "vars")
  expect_true(p$by$multiple)
  expect_identical(p$by$study, "settings.treatment")
  expect_null(p$by$default)
  expect_true(p$by$ordered)
  expect_identical(p$cells$type, "choice")
  expect_identical(p$cells$choices, c("n", "pct"))
  expect_identical(p$cells$default, "n")
  expect_true(p$engine$hidden)
  expect_identical(p$title$doc, "The title.")
  expect_identical(names(.tpl_shown(p)), c("data", "by", "cells", "title"))
  expect_identical(.tpl_arg_section("s1/t1", "data"), "data")
  expect_identical(.tpl_arg_section("s1/t1", "by"), "ard")
  expect_identical(.tpl_arg_section("s1/t1", "cells"), "content")
  expect_identical(.tpl_arg_section("s1/t1", "title"), "page")
  # (an argument the defs lack: today's lookup)
  expect_identical(.tpl_arg_section("s1/t1", "population"), "data")
})
