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
  d$label_en <- NA_character_
  d$label_ja <- NA_character_
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
  d$fixed_rows <- I(lapply(rows, function(r) r$fixed_rows))
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
      list(name = "cols", input = "pick", type = "list<column>", choices = "vars(from_args(data))",
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
  expect_identical(s$args$cols$choices, "vars(from_args(data))")
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
  d <- list(input = "pick", choices = "vars(from_args(data))", ordered = TRUE, free = FALSE)
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
  h <- html(arg_field("c", "from", "string", def = list(input = "pick", choices = c("objects(data)", "study_data")),
                      ctx = list(objects = list(data = "adsl_sub"), study_data = c("ADSL", "ADAE"))))
  expect_match(h, "<optgroup label=\"This report&#39;s objects\">", fixed = TRUE)
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
  # table: the table field (test-arg-defs-table.R); a rows<...> with no
  # columns to show stays today's field of its type
  h <- html(arg_field("c", "derive", "map<column,r>", def = list(input = "table", choices = "vars(from)")))
  expect_match(h, "rp-tbl", fixed = TRUE)
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = list(input = "table")))
  expect_identical(h, html(arg_field("c", "conditions", "rows<map<string,string|r>>")))
  # text: as the type says, the same as without a def
  for (ty in c("string", "number", "r", "template")) {
    expect_identical(html(arg_field("c", "a", ty, value = NULL, def = list(input = "text"))),
                     html(arg_field("c", "a", ty)), info = ty)
  }
  # (a list of text typed: the same select of several, but a value not among
  # the choices may be typed -- normalize_ard(drop_contexts = ); without a
  # def, or with another form, it cannot)
  h_text <- html(arg_field("c", "a", "list<string>", value = NULL, def = list(input = "text")))
  h_plain <- html(arg_field("c", "a", "list<string>"))
  expect_match(h_text, "\"create\":true", fixed = TRUE)
  expect_false(grepl("\"create\"", h_plain, fixed = TRUE))
  expect_identical(sub("\"create\":true,", "", h_text, fixed = TRUE), h_plain)
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
    objects = list(data = c("adsl_sub"), ard = c("demog_ard")),
    study_data = c("ADSL", "ADAE"),
    tokens = c("{N}", "{PAGE}"),
    figure_style = list(palette = c("#1b9e77", "#d95f02"), marker = c("circle", "square")),
    stats = function() c("n", "mean"),
    styles = c("bold", "italic"),
    args = list(data = "adsl", var = "SEX", variables = c("AGE", "SEX"), by = "TRT01A"))
  v <- function(w, ...) .choice_values(w, ctx, ...)
  # (a name in the brackets is itself, the data; from_args(<arg>) the value of
  # the card's argument: data is adsl, var is SEX)
  expect_identical(unname(v("vars(adae)")), "AEDECOD")
  expect_identical(unname(v("vars(from_args(data))")), c("AGE", "SEX"))
  expect_length(v("vars(data)"), 0L)
  expect_identical(v("levels(from_args(var))"), v("levels(SEX)"))
  # (an argument with no single value: nothing)
  expect_length(v("vars(from_args(variables))"), 0L)
  expect_length(v("vars(from_args(nothing))"), 0L)
  expect_identical(unname(v("levels(SEX)")), c("Female", "Male"))
  expect_identical(names(v("levels(SEX)")), c("F", "M"))
  expect_identical(unname(v("codelist(NY)")), c("N", "Y"))
  expect_identical(unname(v("columns(ard)")), c("group1", "stat_name"))
  expect_identical(unname(v("populations")), c("Safety", "ITT"))
  expect_identical(names(v("populations")), c("SAF", "ITT"))
  expect_identical(unname(v("objects(data)")), "adsl_sub")
  expect_identical(unname(v("objects(ard)")), "demog_ard")
  expect_length(v("objects(model)"), 0L)
  expect_identical(unname(v("study_data")), c("ADSL", "ADAE"))
  expect_identical(unname(v("figure_style(palette)")), c("#1b9e77", "#d95f02"))
  expect_identical(unname(v("figure_style(marker)")), c("circle", "square"))
  # (format_sets: no provider until format sets are built -- no choices, no error)
  expect_identical(v("format_sets"), character())
  expect_identical(unname(v("tokens")), c("{N}", "{PAGE}"))
  expect_identical(unname(v("stats")), c("n", "mean"))
  expect_identical(unname(v("styles")), c("bold", "italic"))
  expect_identical(unname(v("fixed", values = c("x", "y"))), c("x", "y"))
  expect_identical(unname(v("from_args(variables, by)")), c("AGE", "SEX", "TRT01A"))
  expect_identical(unlist(unname(.choice_values("vars(from_args(from)), fixed",
                                         list(vars = list(adsl = "AGE"), args = list(from = "adsl")),
                                                values = "none")[[1L]])), "AGE")
  # several: the groups in the words' order, each named
  g <- v("objects(data), study_data")
  expect_identical(names(g), c("This report's objects", "Study data"))
  expect_identical(unlist(g[[2L]]), c(ADSL = "ADSL", ADAE = "ADAE"))
  g <- v(c("study_data", "objects(data)"), lang = "ja")
  expect_identical(names(g), tflspec::tfl_choice_sources()$label_ja[c(6L, 7L)])
  # (one of several gives nothing: the other alone, no group)
  expect_identical(unname(.choice_values(c("objects(data)", "objects(ard)"),
                                         list(objects = list(ard = "a1")))), "a1")
  # a source the ctx cannot answer: nothing
  expect_identical(.choice_values("vars(data)", list()), character())
  expect_identical(.choice_values("populations", NULL), character())
  expect_identical(.choice_values(character(), ctx), character())
  expect_identical(.choice_values("fixed", ctx), character())
  # an unknown word stops
  expect_error(.choice_values("datasets", ctx), "Unknown choice source \"datasets\"")
  expect_error(.choice_values("vars(data), colours", ctx), "colours")
  # an old word stops naming the new one (tflspec's list, CG-4)
  expect_error(.choice_values("report_data", ctx), "write objects\\(data\\)")
  expect_error(.choice_values("ard", ctx), "columns\\(ard\\)")
  expect_error(.choice_values("study_info", ctx), "write tokens")
})

test_that("the choice words are tflspec's list, no copy (CG-4)", {
  expect_identical(.choice_sources()$word, tflspec::tfl_choice_sources()$word)
  expect_length(.choice_sources()$word, 14L)
  # (every word is served: a ctx of each gives its value, none stops)
  ctx <- list(vars = "V", levels = list(X = "L"), codelist = list(C = "c"), columns = "K",
              populations = "P", study_data = "S", objects = list(data = "O"), stats = "st",
              format_sets = "F", styles = "sy", figure_style = list(palette = "#000"),
              tokens = "{T}", args = list(a = "A", v = "X"))
  words <- c("vars", "levels(from_args(v))", "codelist(C)", "columns", "populations", "study_data",
             "objects(data)", "stats", "format_sets", "styles", "figure_style(palette)", "tokens",
             "fixed", "from_args(a)")
  expect_identical(sub("[(].*$", "", words), tflspec::tfl_choice_sources()$word)
  for (w in words) expect_length(.choice_values(w, ctx, values = "f"), 1L)
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
             choices = "vars(from_args(data))", ordered = TRUE,
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

test_that("a series template's table params (conditions, formats): the table field, read back as records", {
  local_defs_reset()
  # (the template's ALL row before the report's: tfl_arg_defs()'s fixed_rows,
  # tflspec#824 -- text values, `!r` its code, a position a row)
  all_row <- data.frame(subgroup = "ALL", subgroup_lvl = "ALL", where = "TRUE", stringsAsFactors = FALSE)
  attr(all_row, "position") <- "before"
  testthat::local_mocked_bindings(
    tfl_arg_defs = function(fn = NULL, template = NULL) {
      mk_defs(
        list(name = "conditions", step = "analysis", input = "table",
             type = "rows<subgroup,subgroup_lvl,where>", columns = "params", default = list(),
             default_kind = "value", fixed_rows = all_row),
        list(name = "formats", step = "analysis", input = "table", type = "map<string,number|string>",
             choices = "stats", free = TRUE),
        list(name = "odd", step = "analysis", input = "table", type = "call<rtf_col_header>"))
    },
    .package = "tflspec")
  p <- .tpl_params("s1/fh")
  expect_identical(p$conditions$type, "table")
  expect_identical(p$formats$type, "table")
  expect_identical(.tpl_type(p$conditions), "table")
  expect_identical(.tpl_type(p$formats), "table")
  # (a table input whose type is no table: as before)
  expect_identical(.tpl_type(p$odd), "text")
  # (the template's own rows: the def's; none for a def without them)
  expect_identical(p$conditions$fixed_rows$subgroup, "ALL")
  expect_null(p$formats$fixed_rows)
  # the field: the type's fields its columns, where an R cell offered the dataset's columns
  data <- list(ADSL = data.frame(USUBJID = "1", SEX = "F", AGEGR1 = "<65"))
  ctx <- .tpl_table_ctx(data, "ADSL", args = list(), params = p)
  expect_identical(ctx$vars("adsl_sub"), c("USUBJID", "SEX", "AGEGR1"))
  df <- data.frame(subgroup = c("SEX", "SEX"), subgroup_lvl = c("F", "M"),
                   where = c("SEX == \"F\"", "SEX == \"M\""), stringsAsFactors = FALSE)
  df$where <- lapply(df$where, tflspec::tfl_r)
  spec <- .tpl_table_spec(p$conditions, ctx, df)
  expect_identical(spec$cols, c("subgroup", "subgroup_lvl", "where"))
  expect_identical(spec$kinds[[3L]], "r")
  expect_true("AGEGR1" %in% spec$choices[[3L]])
  h <- as.character(.field_table("tpl_x__conditions", spec, df, fixed_rows = p$conditions$fixed_rows))
  expect_match(h, "value=\"M\"", fixed = TRUE)
  # (the ALL row read only, before the report's rows; "after": after them)
  expect_match(h, "rp-tbl-fixed[^$]*rp-tbl-body")
  expect_match(h, "<td class=\"rp-tbl-td rp-mono\">TRUE</td>", fixed = TRUE)
  # (a row before, a row after: each where its position says)
  two <- data.frame(subgroup = c("ALL", "OTHER"), subgroup_lvl = c("ALL", "OTHER"),
                    where = c("TRUE", "is.na(SEX)"), stringsAsFactors = FALSE)
  attr(two, "position") <- c("before", "after")
  h <- as.character(.field_table("tpl_x__conditions", spec, df, fixed_rows = two))
  expect_match(h, "rp-tbl-fixed[^$]*>TRUE<[^$]*rp-tbl-body[^$]*rp-tbl-fixed[^$]*is.na\\(SEX\\)")
  # (the R code as written, not quoted as a string)
  expect_false(grepl("&quot;TRUE&quot;", h, fixed = TRUE))
  # read back: the field's state -> records; untouched it says the value it shows
  st <- as.character(jsonlite::toJSON(.tbl_state(spec, df), auto_unbox = TRUE))
  got <- .tpl_value(st, "table", p$conditions)
  expect_length(got, 2L)
  expect_identical(got[[2L]]$subgroup_lvl, "M")
  expect_s3_class(got[[2L]]$where, "tfl_r")
  expect_identical(.tpl_untouched(df, "table", p$conditions), got)
  # (the data frame and its records read back compare the same; a row more does not)
  expect_identical(.tpl_table_key(got, p$conditions), .tpl_table_key(df, p$conditions))
  expect_false(identical(.tpl_table_key(got[1L], p$conditions), .tpl_table_key(df, p$conditions)))
  expect_null(.tpl_value("not a state", "table", p$conditions))
  # a map: its key and value; a number kept a number
  st <- as.character(jsonlite::toJSON(list(shape = "map", cols = c("key", "value"),
                                           kinds = c("text", "number_text"),
                                           rows = list(c("mean", "1"), c("pct", "xx.x"))), auto_unbox = TRUE))
  expect_identical(.tpl_value(st, "table", p$formats), list(mean = 1, pct = "xx.x"))
  expect_identical(.tpl_table_key(list(mean = 1, pct = "xx.x"), p$formats),
                   .tpl_table_key(.tpl_value(st, "table", p$formats), p$formats))
})

test_that("a series template's pick of source words: chosen from them (objects(data), study_data)", {
  defs <- mk_defs(
    list(name = "from", input = "pick", choices = "objects(data), study_data", free = FALSE),
    list(name = "population", input = "pick", choices = "populations"),
    list(name = "tags", input = "pick", type = "list<string>", choices = "objects(data)"),
    list(name = "note", input = "text"))
  prm <- .tpl_params_from_defs(defs)
  expect_identical(vapply(prm, function(x) x$type, ""),
                   c(from = "pick", population = "pick", tags = "pick", note = "text"))
  expect_identical(.tpl_type(prm$from), "pick")
  expect_identical(.tpl_type(prm$tags), "picks")
  # the choices: the report's data and the study's, in two groups
  data <- list(ADSL = data.frame(USUBJID = "1"), ADAE = data.frame(AEDECOD = "x"))
  ctx <- .tpl_table_ctx(data, "ADAE", args = list(), populations = c(SAF = "SAFFL"))
  ch <- .choice_values(prm$from$sources, ctx)
  expect_identical(unname(unlist(ch[[1L]])), "ADAE")
  expect_identical(unname(unlist(ch[[2L]])), c("ADSL", "ADAE"))
  # (the report's `data` argument before its dataset)
  ctx <- .tpl_table_ctx(data, "ADAE", args = list(data = "adsl"))
  expect_identical(unname(.choice_values("objects(data)", ctx)), "ADSL")
  expect_identical(unname(.choice_values(prm$population$sources,
                                         .tpl_table_ctx(data, populations = c(SAF = "SAFFL")))), "SAFFL")
  # the value read back: one a text, several a vector, none NULL
  expect_identical(.tpl_value("ADSL", "pick", prm$from), "ADSL")
  expect_null(.tpl_value("", "pick", prm$from))
  expect_identical(.tpl_value(c("ADSL", "ADAE"), "picks", prm$tags), c("ADSL", "ADAE"))
  expect_identical(.tpl_untouched(c("ADSL", "ADAE"), "picks", prm$tags), c("ADSL", "ADAE"))
})

test_that("a level def's choices word levels(from_args(<param>)) gives the card the variable to read", {
  defs <- mk_defs(
    list(name = "category", input = "pick", type = "column", choices = "vars(ADSL)"),
    list(name = "responder", input = "pick", type = "level", choices = "levels(from_args(category))", free = TRUE),
    list(name = "ref", input = "pick", type = "level", choices = "levels(from_args(category))"),
    list(name = "many", input = "pick", type = "list<level>", choices = "levels(SEX)"),
    list(name = "other", input = "pick", type = "level"))
  prm <- .tpl_params_from_defs(defs)
  expect_identical(prm$responder$of, "category")
  expect_identical(prm$ref$of, "category")
  expect_identical(prm$many$of, "SEX")
  expect_null(prm$other$of)
  expect_null(prm$category$of)
  expect_identical(.tpl_type(prm$responder), "levels")
  expect_identical(.tpl_type(prm$many), "levels")
  # the variable is the other param's value (or its default), its values the data's
  d <- list(ADSL = data.frame(ARM = c("A", "B", "A"), SEX = c("F", "M", "F")))
  expect_identical(.tpl_levels_of(prm$responder, prm, list(category = "ARM")), "ARM")
  expect_identical(.tpl_levels(d, .tpl_levels_of(prm$responder, prm, list(category = "ARM"))), c("A", "B"))
  expect_identical(.tpl_levels(d, .tpl_levels_of(prm$many, prm, list())), c("F", "M"))
})

test_that("a series template's level params (the test series' responder, forest_hr ref) read a variable", {
  skip_if_not("tfl_arg_defs" %in% getNamespaceExports("tflspec"))
  # (orr_subgroup's responder left tflspec_v1 with it, canon 2026-11-24: the
  # test series has the same param)
  local_test_series()
  for (x in list(c("tflplanner_test/params", "responder"), c("tflspec_v1/forest_hr", "ref"))) {
    prm <- .tpl_params(x[[1L]])
    expect_true(x[[2L]] %in% names(prm), info = x[[1L]])
    expect_identical(.tpl_type(prm[[x[[2L]]]]), "levels", info = x[[1L]])
    expect_true(nzchar(prm[[x[[2L]]]]$of %||% ""), info = x[[1L]])
  }
})

test_that("a map keyed by columns (demog analysis_ids) offers the data's columns as its keys", {
  defs <- mk_defs(list(name = "analysis_ids", step = "analysis", input = "table",
                       type = "map<column,string>", choices = "vars(adsl_sub)"))
  prm <- .tpl_params_from_defs(defs)
  expect_identical(.tpl_type(prm$analysis_ids), "table")
  data <- list(ADSL = data.frame(AGE = 1, SEX = "F"))
  ctx <- .tpl_table_ctx(data, "ADSL", args = list(), params = prm)
  spec <- .tpl_table_spec(prm$analysis_ids, ctx)
  expect_identical(spec$kinds[[1L]], "pick")
  expect_identical(unname(spec$choices[[1L]]), c("AGE", "SEX"))
})

test_that("a condition / R param (the test series' record_where) is the R field: its text kept as R, not YAML", {
  defs <- mk_defs(list(name = "record_where", step = "data", input = "condition", type = "r"),
                  list(name = "other", step = "data", input = "text", type = "r"))
  prm <- .tpl_params_from_defs(defs)
  expect_identical(.tpl_type(prm$record_where), "r")
  expect_identical(.tpl_type(prm$other), "r")
  for (txt in c('AVAL > 2 & PARAMCD == "ALT"', "TRUE", "x: y")) {
    v <- .tpl_value(txt, "r", prm$record_where)
    expect_s3_class(v, "tfl_r")
    expect_identical(as.character(unclass(v)), txt)
    # (read back as the shown text, and untouched it is no edit)
    expect_identical(.tpl_r_text(v), txt)
    expect_identical(.tpl_untouched(v, "r", prm$record_where), v)
  }
  expect_null(.tpl_value("  ", "r", prm$record_where))
  expect_identical(.tpl_r_text(TRUE), "TRUE")
  html <- as.character(.tpl_r_field("a", "lab", tflspec::tfl_r("TRUE"), c("AVAL", "PARAMCD")))
  expect_match(html, "rp-condition", fixed = TRUE)
  expect_match(html, "<option value=\"AVAL\"", fixed = TRUE)
  expect_match(html, "value=\"TRUE\"", fixed = TRUE)
})

test_that("every series template's condition / R params get the R field", {
  skip_if_not("tfl_arg_defs" %in% getNamespaceExports("tflspec"))
  # (no shipped template has a scalar R / condition param since
  # lab_tox_shift left tflspec_v1, canon 2026-11-24: the test series has one)
  local_test_series()
  n <- 0L
  tps <- c(paste0("tflspec_v1/", c("forest_hr", "km_simple", "tflspec_hierarchy", "tflspec_listing",
                                   "tflspec_shift", "tflspec_summary")),
           "tflplanner_test/params")
  for (tp in tps) {
    prm <- .tpl_params(tp)
    expect_gt(length(prm), 0L, label = tp)
    for (x in prm) {
      isr <- identical(x$def$input, "condition") || identical(x$def$type, "r")
      if (isr) n <- n + 1L
      expect_identical(.tpl_type(x) == "r", isr, info = paste(tp, x$name))
    }
  }
  expect_gte(n, 1L)
  expect_identical(.tpl_type(.tpl_params("tflplanner_test/params")$record_where), "r")
})
