# A template's map argument in the card's lines (R/template_report.R):
# values a line comma separated (fields `values`), a rule a side (fields `edge`, `line`)

test_that("a map of several values a name: shown comma separated, read back as vectors", {
  prm <- list(type = "list", multiple = FALSE, fields = c("variable", "values"))
  x <- list(RACE = c("ASIAN", "WHITE"), SEX = c("F", "M"))
  txt <- .tpl_list_text(x, prm)
  expect_identical(txt, "RACE | ASIAN, WHITE\nSEX | F, M")
  expect_identical(.tpl_list_parse(txt, prm), x)
  expect_identical(.tpl_list_parse("RACE | ASIAN", prm), list(RACE = "ASIAN"))
})

test_that("a rule a side: TRUE / FALSE read as flags", {
  prm <- list(type = "list", multiple = FALSE, fields = c("edge", "line"))
  expect_identical(.tpl_list_parse("bottom | TRUE\ntop | FALSE", prm), list(bottom = TRUE, top = FALSE))
  expect_identical(.tpl_list_text(list(bottom = TRUE), prm), "bottom | TRUE")
})

test_that("a map of one text a name keeps its commas", {
  prm <- list(type = "list", fields = c("name", "text"))
  expect_identical(.tpl_list_parse("AGE | Age, years", prm), c(AGE = "Age, years"))
})
