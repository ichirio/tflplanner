# One field for both kinds of report: a template parameter is turned into the
# schema's type notation by one table (R/field_types.R) and drawn by arg_field()

prm <- function(...) c(list(name = "x"), list(...))
ft <- function(...) .tpl_field_type(prm(...))

test_that("the conversion table maps every input of a template to one notation", {
  expect_identical(ft(type = "vars")$type, "column")
  expect_identical(ft(type = "vars", multiple = TRUE)$type, "list<column>")
  expect_identical(ft(type = "levels", of = "by")$type, "level")
  expect_identical(ft(type = "levels", of = "by", multiple = TRUE)$type, "list<level>")
  expect_identical(ft(type = "flag")$type, "logical")
  expect_identical(ft(type = "number")$type, "number")
  expect_identical(ft(type = "text")$type, "string")
  expect_identical(ft(type = "text", multiple = TRUE)$type, "list<string>")
  expect_identical(ft(type = "choice", choices = c("a", "b"))$type, "enum[a|b]")
  expect_identical(ft(type = "choice", multiple = TRUE, choices = c("a", "b"))$type, "list<enum[a|b]>")
  expect_identical(ft(type = "choice")$type, "string")
  expect_identical(ft(type = "object")$type, "object")
  expect_identical(ft(type = "r")$type, "r")
  expect_identical(ft(type = "list")$type, "rows<name|text>")
  expect_identical(.tpl_field_type(list(name = "data", type = "text"))$type, "dataset")
  expect_identical(.tpl_field_type(list(name = "population", type = "text"))$type, "population")
  expect_identical(.tpl_field_type(list(name = "subset", type = "text"))$type, "r")
  expect_identical(.tpl_field_type(list(name = "where", type = "text"))$input, "expr")
})

test_that("every type of the table is a notation arg_field() knows", {
  for (i in seq_len(nrow(.field_input_types))) {
    ty <- sub("[.]{3}", "a|b", .field_input_types$type[[i]])
    expect_false(is.na(.arg_kind(ty)), info = ty)
  }
  expect_identical(.arg_kind("dataset"), "dataset")
  expect_identical(.arg_kind("population"), "population")
  expect_identical(.arg_kind("level"), "level")
  expect_identical(.arg_kind("list<level>"), "list")
  expect_identical(.arg_kind("list<enum[a|b]>"), "list")
  expect_identical(.arg_kind("rows<name|text>"), "r")
})

test_that("a template parameter and a code argument of one type draw the same field", {
  sel <- function(h) gsub("[[:space:]]+", " ", gsub("id=\"[^\"]*\"|data-[a-z]+=\"[^\"]*\"|for=\"[^\"]*\"", "", h))
  pairs <- list(
    list(prm(type = "vars", default = "TRT01A"), "column"),
    list(prm(type = "vars", multiple = TRUE), "list<column>"),
    list(prm(type = "flag", default = FALSE), "logical"),
    list(prm(type = "number", default = 3), "number"),
    list(prm(type = "text"), "string"),
    list(prm(type = "choice", choices = c("a", "b")), "enum[a|b]"))
  for (p in pairs) {
    a <- as.character(tpl_field("m", p[[1L]], options = c("A", "B"), lang = "en"))
    b <- as.character(arg_field("m", "x", p[[2L]], choices = c("A", "B"), default = p[[1L]]$default, lang = "en"))
    expect_identical(sel(a), sel(b), info = p[[2L]])
  }
})

test_that("the field's input id is the card's id and the parameter's name", {
  h <- as.character(tpl_field("mtpl_demog", list(name = "by", type = "vars"), options = c("A")))
  expect_match(h, "mtpl_demog__by", fixed = TRUE)
})

test_that("the locked state disables every input and says so; not set marks a required one", {
  h <- as.character(tpl_field("m", prm(type = "text"), value = "a", locked = TRUE, lang = "en"))
  expect_match(h, "rp-field-locked", fixed = TRUE)
  expect_match(h, "disabled", fixed = TRUE)
  expect_match(h, "Fixed by the template", fixed = TRUE)
  h <- as.character(tpl_field("m", prm(type = "vars", required = TRUE), options = "A", lang = "en"))
  expect_match(h, "rp-field-unset", fixed = TRUE)
  expect_match(h, "Not set", fixed = TRUE)
  h <- as.character(tpl_field("m", prm(type = "vars", required = TRUE), value = "A", options = "A", lang = "en"))
  expect_false(grepl("rp-field-unset", h, fixed = TRUE))
  # (the other kinds' fields are not locked unless asked)
  expect_false(grepl("rp-field-locked", as.character(arg_field("m", "x", "string")), fixed = TRUE))
})

test_that("the R-text switch and the source badge are the same slots on both sides", {
  v <- structure("AGE >= 18", class = "tfl_r")
  h <- as.character(arg_field("m", "x", "number", value = v, from = "template", lang = "en"))
  expect_match(h, "rp-field-r", fixed = TRUE)
  expect_match(h, "AGE &gt;= 18", fixed = TRUE)
  expect_match(h, "rp-badge-template", fixed = TRUE)
  # (on the template side the template is every default's source: no badge for it)
  h <- as.character(tpl_field("m", prm(type = "number"), value = v, from = "template", lang = "en"))
  expect_false(grepl("rp-badge-template", h, fixed = TRUE))
  h <- as.character(tpl_field("m", prm(type = "number"), value = v, from = "standards", lang = "en"))
  expect_match(h, "rp-badge-standards", fixed = TRUE)
  # an R-only argument is its own R field, never hidden by the R state's rule
  h <- as.character(arg_field("m", "where", "r"))
  expect_match(h, "rp-field-ronly", fixed = TRUE)
})

test_that("a list's default is the placeholder in full, a typed value is allowed", {
  h <- as.character(arg_field("m", "x", "list<column>", choices = c("A", "B"), default = c("A", "B")))
  expect_match(h, "A, B", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "list<string>"))
  expect_match(h, "create", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "list<enum[a|b]>"))
  expect_match(h, ">a<", fixed = TRUE)
})

test_that("an enum is a selectize select with its default as the placeholder", {
  h <- as.character(arg_field("m", "den", "enum[denominator|set|none]", default = "set"))
  expect_false(grepl("<select[^>]*form-select", h))
  expect_match(h, "\"placeholder\":\"set\"", fixed = TRUE)
  expect_match(h, "data-for=\"m__den\"", fixed = TRUE)
  expect_false(grepl("(default)", h, fixed = TRUE))
  # (a template's choice is the same field)
  h2 <- as.character(tpl_field("m", list(name = "den", type = "choice", choices = c("denominator", "set"),
                                         default = "set")))
  expect_match(h2, "\"placeholder\":\"set\"", fixed = TRUE)
  # (the value set is selected, and a value outside the entries is kept)
  h <- as.character(arg_field("m", "den", "enum[a|b]", value = "b", default = "a"))
  expect_match(h, "<option value=\"b\" selected>", fixed = TRUE)
  h <- as.character(arg_field("m", "den", "enum[a|b]", value = "zzz", default = "a"))
  expect_match(h, "zzz", fixed = TRUE)
  # (no default: the usual placeholder)
  h <- as.character(arg_field("m", "den", "enum[a|b]", lang = "en"))
  expect_match(h, "\"placeholder\":\"Choose\"", fixed = TRUE)
})

test_that("a value of the report's own is marked changed and has the reset; the default has neither", {
  is_set <- function(h) grepl("class=\"rp-field [^\"]*\\brp-set\\b", h)
  for (ty in c("string", "number", "enum[a|b]", "column", "logical")) {
    v <- switch(ty, string = "x", number = 3, "enum[a|b]" = "a", column = "A", logical = TRUE)
    set <- as.character(arg_field("m", "x", ty, value = v, default = if (ty == "logical") FALSE else "d",
                                  choices = c("A", "B")))
    dflt <- as.character(arg_field("m", "x", ty, default = if (ty == "logical") FALSE else "d",
                                   choices = c("A", "B")))
    expect_true(is_set(set), info = ty)
    expect_false(is_set(dflt), info = ty)
    expect_match(set, "class=\"rp-reset\"", fixed = TRUE)
    expect_match(set, "Reset to the default", fixed = TRUE)
  }
  # a switch knows whether it was set (a set FALSE is not the default)
  h <- as.character(arg_field("m", "x", "logical", value = FALSE, default = FALSE))
  expect_match(h, "data-flag-set=\"1\"", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "logical", default = TRUE))
  expect_match(h, "data-flag-set=\"0\"", fixed = TRUE)
  expect_match(h, "data-default=\"true\"", fixed = TRUE)
  # a locked field has no reset and no changed mark
  h <- as.character(arg_field("m", "x", "string", value = "x", locked = TRUE))
  expect_false(grepl("rp-reset", h, fixed = TRUE))
  expect_false(is_set(h))
})

test_that("a locked field has one badge, whatever its source", {
  h <- as.character(arg_field("m", "x", "string", value = "a", from = "template", locked = TRUE, lang = "en"))
  expect_identical(lengths(regmatches(h, gregexpr("class=\"rp-badge ", h))), 1L)
  expect_match(h, "rp-badge-locked", fixed = TRUE)
  expect_match(h, "title=\"Fixed by the template\"", fixed = TRUE)
  expect_false(grepl("rp-badge-template", h, fixed = TRUE))
  expect_false(grepl("\U0001F512", h, fixed = TRUE))
})

test_that("an R-only argument has a plain R label, not a switch, locked or not", {
  for (lk in c(FALSE, TRUE)) {
    h <- as.character(arg_field("m", "where", "r", value = "AGE > 1", locked = lk))
    expect_match(h, "class=\"rp-r-label\"", fixed = TRUE)
    expect_false(grepl("rp-r-toggle", h, fixed = TRUE))
    expect_false(grepl("type=\"checkbox\"", h, fixed = TRUE))
  }
})

test_that("the R switch is a checkbox the keyboard reaches, hidden visually only", {
  h <- as.character(arg_field("m", "x", "number"))
  expect_match(h, "<input type=\"checkbox\" id=\"m__x__isr\" class=\"rp-isr\"", fixed = TRUE)
  expect_false(grepl("tabindex", h, fixed = TRUE))
  expect_false(grepl("disabled", h, fixed = TRUE))
  css <- gsub("[[:space:]]+", " ", .builder_cards_css)
  expect_false(grepl("rp-r-toggle input \\{ display: none", css))
  expect_match(css, ".rp-r-toggle input { position: absolute; opacity: 0;", fixed = TRUE)
  expect_match(css, ".rp-r-toggle:focus-within { outline", fixed = TRUE)
  # (the script carries the current value over as R and focuses the R input)
  expect_match(.builder_cards_js, "valueAsR", fixed = TRUE)
  expect_match(.builder_cards_js, "ri.trigger('focus')", fixed = TRUE)
})

test_that("a required field with a default shows the star and the default, not 'Not set'", {
  h <- as.character(arg_field("m", "x", "number", default = 5, required = TRUE, lang = "en"))
  expect_false(grepl("rp-field-unset", h, fixed = TRUE))
  expect_match(h, "class=\"rp-req\"", fixed = TRUE)
  expect_match(h, "placeholder=\"5\"", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "number", required = TRUE, lang = "en"))
  expect_match(h, "rp-field-unset", fixed = TRUE)
  expect_match(h, "data-nodefault=\"1\"", fixed = TRUE)
})

test_that("a list carries its default for the chips; a plain choice or typed list gets its placeholder", {
  h <- as.character(arg_field("m", "x", "list<column>", choices = c("A", "B"), default = c("A", "B")))
  expect_match(h, "data-default=\"[&quot;A&quot;,&quot;B&quot;]\"", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "list<string>", lang = "en"))
  expect_match(h, "\"placeholder\":\"Choose or type\"", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "level", choices = c("A", "B"), lang = "en"))
  expect_match(h, "\"placeholder\":\"Choose or type\"", fixed = TRUE)
  h <- as.character(arg_field("m", "x", "column", choices = c("A", "B"), lang = "en"))
  expect_match(h, "\"placeholder\":\"Choose\"", fixed = TRUE)
})

test_that("the preview is made on Update only: no focusout regrouping, Ctrl+Enter is wired", {
  expect_false(grepl("focusout', '.rp-plan-args", .builder_cards_js, fixed = TRUE))
  expect_match(.fs_js, "ctrlKey", fixed = TRUE)
  expect_match(.fs_js, "fs_update", fixed = TRUE)
  expect_match(.fs_js, "blur", fixed = TRUE)
})
