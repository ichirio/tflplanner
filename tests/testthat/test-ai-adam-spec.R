test_that("the ADaM spec field goes to tfl_ai_prepare() as adam_spec or define", {
  expect_identical(.ai_adam_spec_args(""), list())
  expect_identical(.ai_adam_spec_args(NULL), list())
  expect_identical(.ai_adam_spec_args(" C:/s/ADaM_spec.xlsx "), list(adam_spec = "C:/s/ADaM_spec.xlsx"))
  expect_identical(.ai_adam_spec_args('"C:/s/define.XML"'), list(define = "C:/s/define.XML"))
})

test_that("a wrong ADaM spec path is said before preparing", {
  d <- tempfile("adamspec"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  x <- file.path(d, "define.xml"); writeLines("<ODM/>", x)
  expect_null(.ai_adam_spec_problem(""))
  expect_null(.ai_adam_spec_problem(x))
  expect_match(.ai_adam_spec_problem(file.path(d, "spec.csv")), "P21 Spec workbook")
  expect_match(.ai_adam_spec_problem(file.path(d, "none.xlsx")), "No ADaM spec", fixed = TRUE)
})

test_that("the prepare step has the ADaM spec input, labelled with its arguments", {
  html <- as.character(ai_ui("ai"))
  expect_match(html, 'id="ai-adam_spec"', fixed = TRUE)
  expect_match(html, "(adam_spec / define)", fixed = TRUE)
})
