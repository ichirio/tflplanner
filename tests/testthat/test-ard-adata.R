
test_that("the analysis data sheet's rows follow every change (it is normalized once while the same)", {
  x <- new_planner()
  x$ard$analysis_data <- .normalize_ard_sheet(
    data.frame(output_id = "T1", data_id = "adsl_saf", from = "ADSL"), "analysis_data")
  expect_identical(.adata_rows(x, "T1")$data_id, "adsl_saf")
  # another sheet (a row added, a value changed, the sheet emptied): its rows
  x$ard$analysis_data <- rbind(x$ard$analysis_data, .normalize_ard_sheet(
    data.frame(output_id = "T1", data_id = "adae_saf", from = "ADAE"), "analysis_data"))
  expect_identical(.adata_rows(x, "T1")$data_id, c("adsl_saf", "adae_saf"))
  x$ard$analysis_data$from[2] <- "ADCM"
  expect_identical(.adata_rows(x, "T1")$from[2], "ADCM")
  y <- new_planner()
  expect_identical(nrow(.adata_rows(y, "T1")), 0L)
  # and back to the first planner's
  expect_identical(.adata_rows(x, "T1")$from, c("ADSL", "ADCM"))
})
