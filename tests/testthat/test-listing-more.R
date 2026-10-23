# The listing screen keeps and says the settings the listing sheets cannot
# say (tflspec's content_more / more, #474)

skip_if(utils::packageVersion("tflspec") < "0.0.24.9221")

lm_planner <- function() {
  x <- structure(list(lf = .empty_lf()), class = "tflplanner")
  x <- set_lf_rows(x, "listings", "L-1", data.frame(
    type = "listing", dataset = "ADAE",
    content_more = "- plan_hide:\n    cols: AESEQ\n- plan_paginate_group:\n    heading: '{USUBJID}'",
    stringsAsFactors = FALSE))
  set_lf_rows(x, "listing_cols", "L-1", data.frame(
    vars = c("USUBJID", "AEDECOD"), label = c("Subject", "Term"),
    more = c(NA, "rel_width: 2"), stringsAsFactors = FALSE))
}

test_that("the listing's rows keep content_more and more", {
  x <- lm_planner()
  l <- lf_rows(x, "listings", "L-1")
  expect_match(l$content_more, "plan_hide", fixed = TRUE)
  cl <- lf_rows(x, "listing_cols", "L-1")
  expect_identical(cl$more, c(NA, "rel_width: 2"))
  # a form's edit of another column keeps them
  l$output_id <- NULL
  l$where <- "AESER == \"Y\""
  x <- set_lf_rows(x, "listings", "L-1", l)
  expect_match(lf_rows(x, "listings", "L-1")$content_more, "plan_paginate_group", fixed = TRUE)
})

test_that("the count and the note: items and columns kept", {
  x <- lm_planner()
  expect_identical(.lf_more_count(x, "L-1"), c(items = 2L, columns = 1L))
  h <- as.character(.lf_more_note(x, "L-1"))
  expect_match(h, "3 settings the sheet cannot say (kept)", fixed = TRUE)
  y <- structure(list(lf = .empty_lf()), class = "tflplanner")
  y <- set_lf_rows(y, "listings", "L-2", data.frame(type = "listing", stringsAsFactors = FALSE))
  expect_null(.lf_more_note(y, "L-2"))
})

test_that("the grids show the kept columns read-only", {
  d <- data.frame(vars = "USUBJID", more = "rel_width: 2", stringsAsFactors = FALSE)
  h <- .grid(d, "listing_cols", "k", list(), read_only = "more")
  cols <- h$x$columns
  ro <- vapply(cols, function(c) isTRUE(c$readOnly), NA)
  expect_identical(ro, c(FALSE, TRUE))
})
