# The study ARD is read once per file (study_ard_rows() is asked by every
# report as a study opens); a file written again is read again.
test_that("the study ARD is read once per file, and again when it changes", {
  d <- withr_tempdir()
  f <- file.path(d, "output", "ard", "ard.rds"); dir.create(dirname(f), recursive = TRUE)
  study <- list(path = d, planner = list(ard = list()))
  expect_null(study_ard_rows(study, "DM"))
  one <- data.frame(output_id = c("DM", "DM", "AE"), stat = c(1, 2, 3))
  saveRDS(one, f)
  expect_equal(study_ard_rows(study, "DM")$stat, c(1, 2))
  key1 <- tflplanner:::.ard_memo$ard$key
  expect_type(key1, "character")
  # the same file: the memo's rows, the key unchanged
  expect_equal(study_ard_rows(study, "AE")$stat, 3)
  expect_identical(tflplanner:::.ard_memo$ard$key, key1)
  # written again (a later time, another size): the new rows
  two <- data.frame(output_id = c("DM", "AE", "AE"), stat = c(10, 20, 30))
  saveRDS(two, f)
  Sys.setFileTime(f, Sys.time() + 60)
  expect_equal(study_ard_rows(study, "DM")$stat, 10)
  expect_equal(study_ard_rows(study, "AE")$stat, c(20, 30))
  expect_false(identical(tflplanner:::.ard_memo$ard$key, key1))
  # the file gone: no rows, nothing stale
  unlink(f)
  expect_null(study_ard_rows(study, "DM"))
})

test_that("an ARD without output_id gives no rows", {
  d <- withr_tempdir()
  f <- file.path(d, "output", "ard", "ard.rds"); dir.create(dirname(f), recursive = TRUE)
  saveRDS(data.frame(stat = 1), f)
  Sys.setFileTime(f, Sys.time() + 120)
  study <- list(path = d, planner = list(ard = list()))
  expect_null(study_ard_rows(study, "DM"))
})
