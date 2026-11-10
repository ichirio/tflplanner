# Every template parameter is a field on the template card: a list the rows
# box cannot show (demog's `cells`, with `!r` guards) is edited as YAML text.

.tca_cells <- function() {
  list(
    n = "{N:.0f}",
    `Mean (SD)` = list(
      tflspec::tfl_r("N < 3 ~ \"{mean:.1f} (-)\""),
      "{mean:.1f} ({sd:.2f})"),
    Median = "{median:.1f}",
    `Min, Max` = "{min}, {max}")
}
.tca_prm <- function() .tpl_params("demog")$cells

test_that("a list the rows cannot show is drawn as YAML text and reads back as itself", {
  skip_if_not_installed("tflspec")
  x <- .tca_cells()
  expect_null(.tpl_list_text(x, .tca_prm()))
  txt <- .tpl_yaml_block(x)
  expect_false(grepl("structure(", txt, fixed = TRUE))
  expect_true(grepl("- !r N < 3 ~", txt, fixed = TRUE))
  expect_gt(length(strsplit(txt, "\n")[[1L]]), 3L)
  expect_identical(.tpl_yaml_read(txt), x)
  d <- .tpl_params("demog")$cells$default
  expect_identical(.tpl_yaml_read(.tpl_yaml_block(d)), d)
})

test_that("numbers keep their digits in the YAML text", {
  x <- list(a = list(0.0005, 3L), b = 1e-10)
  txt <- .tpl_yaml_block(x)
  expect_true(grepl("0.0005", txt, fixed = TRUE))
  expect_false(grepl("e-", txt, fixed = TRUE))
  expect_identical(.tpl_yaml_read(txt), x)
})

test_that("an edit in the text gives the changed value; the same text is no edit", {
  skip_if_not_installed("tflspec")
  x <- .tca_cells()
  txt <- .tpl_yaml_block(x)
  expect_identical(.tpl_yaml_edit(txt, x)$kind, "same")
  expect_identical(.tpl_yaml_edit(paste0(txt, "\n"), x)$kind, "same")
  ed <- .tpl_yaml_edit(sub("{median:.1f}", "{median:.2f}", txt, fixed = TRUE), x)
  expect_identical(ed$kind, "edit")
  expect_identical(ed$value$Median, "{median:.2f}")
  expect_identical(ed$value[["Mean (SD)"]], x[["Mean (SD)"]])
})

test_that("YAML that does not parse says why and gives no value", {
  skip_if_not_installed("tflspec")
  x <- .tca_cells()
  ed <- .tpl_yaml_edit("n: [unclosed\nMedian: 'x", x)
  expect_identical(ed$kind, "bad")
  expect_true(nzchar(ed$msg))
  expect_null(ed$value)
  ed2 <- .tpl_yaml_edit("just a word", x)
  expect_identical(ed2$kind, "bad")
})

test_that("the edit is written to template.args and nothing to content", {
  skip_if_not_installed("tflspec")
  x <- .tca_cells()
  prm <- .tca_prm()
  txt <- .tpl_yaml_block(x)
  ed <- .tpl_yaml_edit(sub("{median:.1f}", "{median:.2f}", txt, fixed = TRUE), x)
  args <- .tpl_set_arg(list(by = "ARM"), prm, ed$value)
  expect_identical(args$by, "ARM")
  expect_identical(args$cells$Median, "{median:.2f}")
  expect_s3_class(args$cells[["Mean (SD)"]][[1L]], "tfl_r")
  # the default itself is taken away, not written
  d <- prm$default
  expect_null(.tpl_set_arg(list(cells = x), prm, .tpl_yaml_edit(.tpl_yaml_block(d), x)$value)$cells)
  # a bad text leaves the args as they are
  bad <- .tpl_yaml_edit("a: [", x)
  expect_identical(bad$kind, "bad")
})

test_that("every parameter of every template is drawn on the card, but the hidden ones", {
  skip_if_not_installed("tflspec")
  tt <- tflspec::tfl_templates()
  nms <- if (is.data.frame(tt)) tt$name else names(tt) %||% tt
  known <- c("population", "data", "columns", "chips", "choice", "choices", "flag", "levels",
             "list", "number", "text", "var", "vars")
  for (n in nms) {
    for (a in .tpl_shown(.tpl_params(n))) {
      expect_true(.tpl_type(a, character()) %in% known, info = paste(n, a$name))
    }
  }
})
