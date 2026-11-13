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
  h <- as.character(tpl_field("m", prm(type = "number"), value = v, from = "template", lang = "en"))
  expect_match(h, "rp-field-r", fixed = TRUE)
  expect_match(h, "AGE &gt;= 18", fixed = TRUE)
  expect_match(h, "rp-badge-template", fixed = TRUE)
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
