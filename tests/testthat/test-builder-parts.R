test_that("a part's items read back from the YAML the card shows", {
  items <- .part_quickstart("ATPT")
  txt <- .part_items_yaml(items)
  expect_match(txt, "table_plan")
  back <- .part_items_parse(txt)
  expect_identical(vapply(back, `[[`, "", "fn"), c("normalize_ard", "table_plan", "plan_cells"))
  expect_identical(back[[2L]]$args$cols, "ATPT")
  expect_identical(.part_items_parse("  "), list())
})

test_that("a new part starts as the ARD's summary statistics by the table's columns", {
  a <- .plan_starter_args("plan_append", list(key = "ATPT"))
  expect_identical(a$name, "summary")
  expect_identical(a$data, list(source = "ard"))
  expect_identical(a$content[[2L]]$args$cols, "ATPT")
})

test_that("the card's fields make the item's arguments", {
  it <- list(path = "plan_append", args = list())
  input <- list(c__name = "summary", c__absent = "skip", c__source = "dataset", c__dataset = "",
                c__content = .part_items_yaml(.part_quickstart()))
  expect_error(.part_args_read(input, "c", it), "dataset")
  input$c__dataset <- "ADPC"
  a <- .part_args_read(input, "c", it)
  expect_identical(a$absent, "skip")
  expect_identical(a$data, list(source = "dataset", dataset = "ADPC"))
  expect_length(a$content, 3L)
})
