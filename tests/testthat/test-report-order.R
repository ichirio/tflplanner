# The report list's order (the order the reports are made in): a new
# report where its id sorts, a TOC's in its own order, the whole list put
# in natural order on request.

ids <- function(p) p$outputs$output_id

test_that("ids in natural order: numbers as numbers", {
  x <- c("T-14-1-10", "T-14-1-2", "T-14-1-1S", "T-14-1-1", "L-16-2-7", "F-14-2-1")
  expect_identical(x[order(.natural_key(x), method = "radix")],
                   c("F-14-2-1", "L-16-2-7", "T-14-1-1", "T-14-1-1S", "T-14-1-2", "T-14-1-10"))
  expect_identical(.natural_key(c("AE", NA)), c("AE", ""))
})

test_that("a new report goes where its id sorts; at = 'end' puts it last", {
  p <- add_output(new_planner(), "T-14-1-1")
  p <- add_output(p, "T-14-1-10")
  p <- add_output(p, "T-14-1-2")
  expect_identical(ids(p), c("T-14-1-1", "T-14-1-2", "T-14-1-10"))
  p <- add_output(p, "T-14-0-1")
  expect_identical(ids(p)[1L], "T-14-0-1")
  p <- add_output(p, "A-1", at = "end")
  expect_identical(ids(p)[nrow(p$outputs)], "A-1")
  # a list ordered by hand keeps its order: before the first that sorts after
  q <- add_output(add_output(add_output(new_planner(), "T-3"), "T-1", at = "end"), "T-9", at = "end")
  expect_identical(ids(q), c("T-3", "T-1", "T-9"))
  q <- add_output(q, "T-2")
  expect_identical(ids(q), c("T-2", "T-3", "T-1", "T-9"))
  # the row is the report's, the others' rows unchanged
  r <- add_output(p, "T-14-1-3", description = "Third")
  expect_identical(r$outputs$description[ids(r) == "T-14-1-3"], "Third")
  expect_identical(r$outputs[ids(r) != "T-14-1-3", ], {
    o <- p$outputs
    rownames(o) <- NULL
    o
  }, ignore_attr = TRUE)
})

test_that("a copy goes beside the reports it sorts with", {
  p <- add_output(add_output(add_output(new_planner(), "T-14-1-1"), "T-14-1-2"), "T-14-1-3")
  p <- copy_output(p, "T-14-1-2", "T-14-1-2_2")
  expect_identical(ids(p), c("T-14-1-1", "T-14-1-2", "T-14-1-2_2", "T-14-1-3"))
})

test_that("sort_outputs() puts the list in natural order, the rows with their ids", {
  p <- add_output(new_planner(), "T-10", description = "ten", at = "end")
  p <- add_output(p, "T-2", description = "two", at = "end")
  p <- add_output(p, "T-1", description = "one", at = "end")
  s <- sort_outputs(p)
  expect_identical(ids(s), c("T-1", "T-2", "T-10"))
  expect_identical(s$outputs$description, c("one", "two", "ten"))
  expect_identical(sort_outputs(new_planner())$outputs, new_planner()$outputs)
  expect_identical(ids(sort_outputs(s)), ids(s))
})

test_that("the report list's Sort by ID: asked first, then sorted", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "T-10", at = "end")
  p <- add_output(p, "T-2", at = "end")
  p <- add_output(p, "T-1", at = "end")
  create_study("ORD", planner = p)
  shiny::testServer(server_for("ORD"), {
    rv <- session$userData$rv
    session$setInputs(nav = "outputs", sort_ids = 1)
    # nothing moves until it is confirmed
    expect_identical(ids(rv$p), c("T-10", "T-2", "T-1"))
    session$setInputs(sort_ids_ok = 1)
    expect_identical(ids(rv$p), c("T-1", "T-2", "T-10"))
  })
})
