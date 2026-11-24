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
    list("r|call<vars>|list<column>|column", "pick", "group"),
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
    list("step<data>", "table", "r"),
    list("list<vec<string, string>>", "text", "r"),
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
