# field_type() (R/field_type.R): the basic type of an argument's field,
# one of eight words made from the definition's type and input form
# (s1-tools one-dispatcher-plan-2026-11-25.md section 1).  Every
# definition of the schema has exactly one; the cases the plan names have
# theirs.

test_that("every schema definition has exactly one of the eight basic types", {
  skip_on_cran()
  fns <- names(tryCatch(tflspec::tfl_schema(), error = function(e) NULL))
  expect_gt(length(fns), 0L)
  for (fn in fns) {
    args <- .schema_of(fn)$args
    for (a in names(args)) {
      ty <- as.character(field_type(args[[a]]))
      expect_true(length(ty) == 1L && ty %in% .field_types,
                  info = sprintf("%s(%s): type %s, input %s -> %s", fn, a, args[[a]]$type %||% "NULL",
                                 args[[a]]$input %||% "NULL", paste(ty, collapse = ", ")))
    }
  }
})

test_that("the plan's cases have their basic types", {
  cases <- list(
    # (the plan's hand list)
    list("list<stat>", "pick", "pick_many"),
    list("r|call<vars>|list<column>|column", "pick", "pick_many"),
    list("call<expansion, expand_scale, waiver>|list<number>|r", "text", "pick_many"),
    list("call<unit>|r", "text", "group"),
    list("number|enum[fit]", "text", "number"),
    list("integer|enum[fit]", "text", "number"),
    list("logical|enum[single|double|thick]|call<rtf_border_line>", "pick", "pick_one"),
    list("call<rtf_border>", "text", "group"),
    list("map<string,column|r>", "table", "rows"),
    list("r", "r", "r"),
    list("r", "condition", "r"),
    # (one of each rule)
    list("rows<subgroup, subgroup_lvl, where>", "table", "rows"),
    list("list<map<string,string>>", "table", "rows"),
    list("formula", "r", "r"),
    list("list<r>", "r", "r"),
    list("call<rtf_border>", "r", "r"),
    list("call|string|r", "text", "r"),
    list("r|call", "text", "r"),
    list("list<call>", "r", "r"),
    list("step<content>", "table", "r"),
    list("step<data>", "table", "r"),
    list("list<call<no_such_function_xyz>>", "r", "r"),
    list("list<vec<string, string>>", "text", "rows"),
    list("list<list<string>|vec<string, string>>", "text", "rows"),
    list("logical|string", "pick", "pick_one"),
    list("logical", "pick", "yesno"),
    list("logical", "text", "yesno"),
    list("integer", "text", "number"),
    list("number|string", "text", "number"),
    list("enum[first|last]", "pick", "pick_one"),
    list("column", "pick", "pick_one"),
    list("level", "pick", "pick_one"),
    list("colour|r|map<column, r>", "text", "pick_one"),
    list("object|r", "text", "pick_one"),
    list("string", "pick", "pick_one"),
    list("string", "text", "text"),
    list("template", "text", "text"),
    list("date", "text", "text"),
    list("list<column>|column", "pick", "pick_many"),
    list("list<string>", "text", "pick_many"),
    list("list<number>|r", "text", "pick_many"),
    list("vec<column, number>", "text", "rows"),
    list("plot", "text", "r"))
  for (k in cases) {
    got <- as.character(field_type(list(type = k[[1L]], input = k[[2L]])))
    expect_identical(got, k[[3L]], info = sprintf("%s, input %s", k[[1L]], k[[2L]]))
  }
  # (no type: R; no input: the type alone)
  expect_identical(as.character(field_type(list())), "r")
  expect_identical(as.character(field_type(list(type = "column"))), "pick_one")
  expect_identical(as.character(field_type(NULL)), "r")
})

test_that("the word is the type's and the input's only; many, ordered and free are data", {
  d <- list(type = "list<column>", input = "pick")
  rich <- c(d, list(label = "Columns", choices = "vars(from)", values = list("A"), default = "A",
                    tier = "key", ordered = TRUE, free = TRUE))
  expect_identical(as.character(field_type(rich)), as.character(field_type(d)))
  ft <- field_type(rich)
  expect_true(attr(ft, "many"))
  expect_true(attr(ft, "ordered"))
  expect_true(attr(ft, "free"))
  one <- field_type(list(type = "column", input = "pick"))
  expect_false(attr(one, "many"))
  expect_false(attr(one, "ordered"))
  expect_false(attr(one, "free"))
  expect_true(attr(field_type(list(type = "list<column>|column", input = "pick")), "many"))
})

test_that("G6: a plain member beside a call decides the field; the call is the detail opener", {
  f <- field_type(list(type = "r|call<vars>|list<column>|column", input = "pick"))
  expect_identical(as.character(f), "pick_many")
  expect_identical(attr(f, "detail"), "vars")
  e <- field_type(list(type = "call<expansion, expand_scale, waiver>|list<number>|r", input = "text"))
  expect_identical(as.character(e), "pick_many")
  expect_identical(attr(e, "detail"), c("expansion", "expand_scale", "waiver"))
  # (a call alone, or with only R, is still its own group)
  u <- field_type(list(type = "call<unit>|r", input = "text"))
  expect_identical(as.character(u), "group")
  expect_identical(attr(u, "detail"), "unit")
  # (a first member that is plain decides as before; the call after it opens)
  b <- field_type(list(type = "logical|enum[single|double]|call<rtf_border_line>", input = "pick"))
  expect_identical(as.character(b), "pick_one")
  expect_identical(attr(b, "detail"), "rtf_border_line")
})

test_that("G1: a bare call is a pick of the def's names, a value of one's own too, its arguments the detail", {
  f <- field_type(list(type = "call|string|r", values = list("dodge", "stack")))
  expect_identical(as.character(f), "pick_one")
  expect_true(attr(f, "free"))
  expect_identical(attr(f, "detail"), "*")
  # (no names: R, as before; theme, strip)
  expect_identical(as.character(field_type(list(type = "call|r"))), "r")
  # (a call that is not first is a text with a call after it)
  expect_identical(as.character(field_type(list(type = "string|call"))), "text")
})

test_that("G3: step<data> is a group of its columns:, step<content> is not a field", {
  cols <- list(source = list(type = "enum[ard|dataset]"), dataset = list(type = "string"))
  expect_identical(as.character(field_type(list(type = "step<data>", columns = cols))), "group")
  expect_identical(as.character(field_type(list(type = "step<data>"))), "r")
  expect_identical(as.character(field_type(list(type = "step<content>", columns = cols))), "r")
})

test_that("G4 G5: a list of vectors, a list of calls of a schema function, is rows", {
  skip_if_not_installed("tflspec")
  expect_identical(as.character(field_type(list(type = "list<vec<string, string>>"))), "rows")
  lc <- field_type(list(type = "list<call<listing_col>>", input = "r"))
  expect_identical(as.character(lc), "rows")
  expect_identical(attr(lc, "of"), "listing_col")
  expect_identical(as.character(field_type(list(type = "list<call<nope_not_a_fn>>"))), "r")
  expect_identical(as.character(field_type(list(type = "list<call>"))), "r")
})

test_that("G10: the three borders are groups from the schema, like the other nine", {
  skip_on_cran()
  skip_if_not_installed("tflspec")
  n <- 0L
  for (fn in names(tflspec::tfl_schema())) {
    args <- .schema_of(fn)$args
    for (a in names(args)) {
      if (identical(args[[a]]$type, "call<rtf_border>")) {
        n <- n + 1L
        expect_identical(as.character(field_type(args[[a]])), "group", info = paste(fn, a))
      }
    }
  }
  expect_gte(n, 12L)
})

test_that("a bare call with names is a pick; the layers' position among them (G1)", {
  skip_on_cran()
  skip_if_not_installed("tflspec")
  n <- 0L
  for (fn in names(tflspec::tfl_schema())) {
    args <- .schema_of(fn)$args
    for (a in names(args)) {
      f <- field_type(args[[a]])
      if (identical(attr(f, "detail"), "*")) {
        n <- n + 1L
        expect_identical(as.character(f), "pick_one")
        expect_gt(length(args[[a]]$values), 0L, label = paste(fn, a))
      }
    }
  }
  expect_gte(n, 80L)
  p <- field_type(.schema_of("geom_bar")$args$position)
  expect_identical(as.character(p), "pick_one")
  expect_true(attr(p, "free"))
})
