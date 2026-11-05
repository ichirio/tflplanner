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

# a sample study whose T-14-1-2 has a part read from ADSL (its data code
# calls order_arms(), which programs/study_helpers.R defines)
.part_study <- function(env = parent.frame()) {
  home <- local_home(env)
  s <- suppressMessages(create_sample_study(run = FALSE))
  args <- list(name = "demog", absent = "skip",
               data = list(source = "dataset", dataset = "ADSL"),
               content = .part_items_parse("- fn: table_plan\n  args:\n    cols: SEX\n"))
  s$planner <- .builder_item_write(s$planner, "T-14-1-2", "plan_append", args, 1L)
  s
}

test_that("the preview of a part from a dataset runs the data code with the study's helpers", {
  skip_on_cran()
  skip_if_not_installed("cards")
  s <- .part_study()
  expect_true(any(grepl("order_arms()", .table_dataset_lines(s$planner, "T-14-1-2"), fixed = TRUE)))
  wd <- getwd()
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_gt(length(r), 0L)
  expect_true(all(vapply(r, is.data.frame, NA)))
  expect_gt(nrow(r[[1L]]), 0L)
  # the app's own session is as it was
  expect_identical(getwd(), wd)
  expect_false(exists("order_arms", envir = globalenv(), inherits = FALSE))
  expect_false(exists("adsl", envir = globalenv(), inherits = FALSE))
})

test_that("the preview reads the study's own study_helpers.R, and says when it stops", {
  skip_on_cran()
  skip_if_not_installed("cards")
  s <- .part_study()
  f <- file.path(s$path, "programs", "study_helpers.R")
  writeLines("order_arms <- function(data) { data$MARKED <- 1L; data }", f)
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_true(all(vapply(r, function(d) "MARKED" %in% names(d), NA)))
  writeLines("stop('boom')", f)
  expect_error(.preview_records(s$planner, "T-14-1-2", s$path), "study_helpers.R stopped: boom")
  # no helpers file: tflspec's functions stand
  unlink(f)
  r <- .preview_records(s$planner, "T-14-1-2", s$path)
  expect_gt(nrow(r[[1L]]), 0L)
})
