test_that("the drafts tfl_ai_adopt() skipped for an ERROR are named with their first problem", {
  res <- data.frame(output_id = c("T-1", "T-2", "T-3"),
                    status = c("adopted", "skipped (error)", "skipped (error)"),
                    file = c("spec/outputs/T-1.yml", NA, NA),
                    problem = c(NA, "plot: risk_title is not an argument", ""),
                    stringsAsFactors = FALSE)
  expect_identical(.ai_adopt_skipped(res),
                   c(`T-2` = "plot: risk_title is not an argument", `T-3` = "ERROR"))
  # an older tflspec (no `problem` column, no skipped rows), a failed call
  expect_identical(.ai_adopt_skipped(res[1L, c("output_id", "status", "file")]), character())
  expect_identical(.ai_adopt_skipped(NULL), character())
})
