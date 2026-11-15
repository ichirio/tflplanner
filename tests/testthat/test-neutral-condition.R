# A condition in an argument that takes only R: the text it is, never a string
# literal (tflspec reads and writes a neutral one untagged).

.nc_report <- function(where) {
  f <- tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "title: x", "content:", "- plan_filter:",
               paste0("    ", where)), f)
  f
}

test_that("a saved condition reads as the condition, and is saved without !r", {
  for (w in c("where: AGE >= 18", "where: !r AGE >= 18")) {
    f <- .nc_report(w)
    o <- tflspec::tfl_read_output(f)
    a <- o$content[[1L]]$args
    expect_identical(.filter_where_text(a$where), "AGE >= 18")
    expect_identical(args_to_r("plan_filter", a), "AGE >= 18")
    tflspec::tfl_write_output(o, f)
    expect_true(any(grepl("^ *where: AGE >= 18$", readLines(f))))
  }
})

test_that("a string the check refuses opens as written, and is kept R when it is applied", {
  o <- tflspec::tfl_read_output(.nc_report("where: between(AGE, 1, 2)"))
  a <- o$content[[1L]]$args
  expect_identical(a$where, "between(AGE, 1, 2)")
  txt <- args_to_r("plan_filter", a)
  expect_identical(txt, "between(AGE, 1, 2)")
  back <- args_from_r("plan_filter", txt)
  expect_identical(.args_problems(back), character())
  expect_s3_class(.args_plain(back)$where, "tfl_r")
})

test_that("the R-only field shows a string as its text, other fields as they did", {
  v <- function(type, value) {
    h <- as.character(arg_field("c1", "where", type, value = value))
    regmatches(h, regexpr('id="c1__where"[^>]*value="[^"]*"', h))
  }
  expect_match(v("formula", "p > 0.5"), 'value="p &gt; 0.5"', fixed = TRUE)
  # no type known: a string typed stays a string (read back as a literal)
  expect_match(v(NULL, "a"), 'value="&quot;a&quot;"', fixed = TRUE)
})

test_that("only a type of nothing but R is read so", {
  expect_true(all(vapply(c("r", "formula", "list<r>|r"), .type_only_r, NA)))
  expect_false(any(vapply(c("string|r", "call|string|r", "enum[a|b]|r", "map<string, column|r>"),
                          .type_only_r, NA)))
  expect_false(.type_only_r(NULL))
  expect_identical(.arg_schema_type("plan_filter", "where"), "list<r>|r")
})

test_that("a plain string in a list of conditions is its text", {
  expect_identical(.filter_where_text(list("p > 1", tflspec::tfl_r("n > 0"))),
                   "(p > 1) & (n > 0)")
  expect_identical(.filter_where_text(list("p > 1")), "p > 1")
})

test_that("the check's two messages say the argument once, and in Japanese", {
  ja <- function(x) tr(x, "ja")
  a <- args_from_r("plan_filter", '"between(AGE, 1, 2)"')
  p <- .args_problems(a)
  expect_length(p, 1L)
  expect_match(p, "^error: `where` is a string that is not a neutral condition")
  pj <- .args_problems(a, ja)
  expect_false(identical(pj, p))
  expect_match(pj, "`where`", fixed = TRUE)
  expect_match(pj, "%in% c(...)", fixed = TRUE)
  expect_match(pj, "`between`", fixed = TRUE)

  it <- tflspec::tfl_item("plan_filter", list(where = tflspec::tfl_r('"AGE >= 18"')))
  m <- tflspec::tfl_schema_check(it)$message
  mj <- .error_view(m, ja)
  expect_false(identical(mj, m))
  expect_match(mj, '"AGE >= 18"', fixed = TRUE)
  expect_identical(.error_view(m), m)
})
