# The template card shows `!r` guarded entries (tflspec's tfl_r) in YAML form
# and reads an edit back.

.tcr_cells <- function() {
  list(
    n = "{N:.0f}",
    `Mean (SD)` = list(
      tflspec::tfl_r("N < 3 ~ \"{mean:.1f} (-)\""),
      "{mean:.1f} ({sd:.2f})"))
}
.tcr_count <- function() {
  list(
    tflspec::tfl_r("n == 0 ~ \"0\""),
    tflspec::tfl_r("p > 0 & p < 0.0005 ~ \"{n} (<0.1%)\""),
    "{n} ({p:.1f%}%)")
}
.tcr_back <- function(txt) .tpl_text_parse(txt)

test_that("a guarded entry list shows in YAML form and reads back as itself", {
  skip_if_not_installed("tflspec")
  x <- .tcr_count()
  txt <- .tpl_show(x)
  expect_false(grepl("structure(", txt, fixed = TRUE))
  expect_true(startsWith(txt, "[!r n == 0 ~ \"0\", !r "))
  expect_identical(.tcr_back(txt), x)
  cells <- .tcr_cells()
  txt <- .tpl_show(cells)
  expect_false(grepl("structure(", txt, fixed = TRUE))
  expect_true(startsWith(txt, "{\"n\": "))
  expect_identical(.tcr_back(txt), cells)
})

test_that("a tagged text with a comma or bracket is quoted so it reads back", {
  skip_if_not_installed("tflspec")
  x <- list(tflspec::tfl_r("a %in% c(1, 2) ~ \"x]\""), "y")
  expect_identical(.tcr_back(.tpl_show(x)), x)
  one <- tflspec::tfl_r("n == 0 ~ \"0\"")
  expect_identical(.tcr_back(.tpl_show(one)), one)
})

test_that("numbers are written as numbers, never scientific", {
  expect_identical(.tpl_show(0.0005), "0.0005")
  expect_identical(.tpl_show(1e-10), "0.0000000001")
  expect_identical(.tpl_show(c(0.0005, 1e6)), "[0.0005, 1000000]")
  expect_identical(.tpl_show(TRUE), "true")
  expect_identical(.tpl_show(list(a = 1.5, b = FALSE)), "{a: 1.5, b: false}")
  expect_identical(.tpl_show(c("a", "b")), "[a, b]")
  expect_identical(.tcr_back(.tpl_show(c("a", "b"))), c("a", "b"))
  expect_identical(.tcr_back(.tpl_show(list(a = 1.5, b = FALSE))), list(a = 1.5, b = FALSE))
})

test_that("a plain string is shown as itself and read back as itself", {
  expect_identical(.tpl_show("{n} ({p:.1f}%)"), "{n} ({p:.1f}%)")
  expect_identical(.tpl_value("{n} ({p:.1f}%)", "text"), "{n} ({p:.1f}%)")
  expect_identical(.tpl_value("[x] y", "text"), "[x] y")
})

test_that("a text that does not parse raises an error for the card to say", {
  expect_error(.tpl_text_parse("[a, b"))
  expect_error(.tpl_text_parse("{a: [}"))
})
