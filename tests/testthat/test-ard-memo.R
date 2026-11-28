# A report's own ARD file is read once per file (PROTOTYPE, tflspec#1032:
# .report_ard_rows() is asked by every report as a study opens); a file
# written again is read again.
test_that("a report's ARD file is read once per file, and again when it changes", {
  d <- withr_tempdir()
  f <- file.path(d, "output", "ard", "report", "DM.rds"); dir.create(dirname(f), recursive = TRUE)
  expect_null(.read_report_ard(d, "DM"))
  saveRDS(data.frame(output_id = "DM", stat = c(1, 2)), f)
  expect_equal(.read_report_ard(d, "DM")$stat, c(1, 2))
  key1 <- tflplanner:::.ard_memo[["ard DM"]]$key
  expect_type(key1, "character")
  # the same file: the memo's rows, the key unchanged
  expect_equal(.read_report_ard(d, "DM")$stat, c(1, 2))
  expect_identical(tflplanner:::.ard_memo[["ard DM"]]$key, key1)
  # written again (a later time, another size): the new rows
  saveRDS(data.frame(output_id = "DM", stat = c(10, 20, 30)), f)
  Sys.setFileTime(f, Sys.time() + 60)
  expect_equal(.read_report_ard(d, "DM")$stat, c(10, 20, 30))
  expect_false(identical(tflplanner:::.ard_memo[["ard DM"]]$key, key1))
  # the file gone: no rows, nothing stale
  unlink(f)
  expect_null(.read_report_ard(d, "DM"))
})

test_that("a report's ARD file without output_id gets the report's", {
  d <- withr_tempdir()
  f <- file.path(d, "output", "ard", "report", "DM.rds"); dir.create(dirname(f), recursive = TRUE)
  saveRDS(data.frame(stat = 1), f)
  Sys.setFileTime(f, Sys.time() + 120)
  expect_identical(.read_report_ard(d, "DM")$output_id, "DM")
})
